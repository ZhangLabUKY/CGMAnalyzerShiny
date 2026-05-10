metrics_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Core Metrics"),
    shiny::h4("Metric Overview"),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("summary_cards")), type = 4),
    shiny::h4("Detailed Metrics"),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("participant_filter"))),
      shiny::column(3, shiny::uiOutput(ns("group_filter"))),
      shiny::column(3, shiny::uiOutput(ns("visit_filter"))),
      shiny::column(3, shiny::uiOutput(ns("category_filter")))
    ),
    shiny::uiOutput(ns("metrics_empty_state")),
    shinycssloaders::withSpinner(DT::DTOutput(ns("metrics_table")), type = 4)
  )
}

metrics_module_server <- function(id, standardized, settings, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    base_metric_state <- shiny::reactive({
      req_active_tab(active_tab, c("metrics", "statistics", "export"))
      data <- tryCatch(
        standardized(),
        shiny.silent.error = function(error) NULL,
        error = function(error) error
      )
      if (inherits(data, "error")) {
        return(metric_state("base_error", error = conditionMessage(data)))
      }
      compute_base_metric_state(data, thresholds = settings()$thresholds_mg_dl)
    })

    adapter_metrics <- shiny::reactiveVal(NULL)
    adapter_state <- new.env(parent = emptyenv())
    adapter_state$key <- NULL
    adapter_state$worker_token <- NULL

    adapter_key <- shiny::reactive({
      paste(
        utils::capture.output(utils::str(list(
          data = cgm_data_signature(standardized()),
          thresholds = threshold_signature(settings()$thresholds_mg_dl)
        ))),
        collapse = "\n"
      )
    })

    shiny::observe({
      req_active_tab(active_tab, c("metrics", "statistics", "export"))
      state <- base_metric_state()
      if (!should_start_additional_metrics(state)) {
        adapter_metrics(NULL)
        adapter_state$key <- NULL
        adapter_state$worker_token <- NULL
        return(NULL)
      }
      key <- adapter_key()
      if (identical(adapter_state$key, key)) {
        return(NULL)
      }
      adapter_state$key <- key
      adapter_metrics(NULL)

      data <- state$data
      by <- default_metric_groups(data)
      worker_token <- configure_background_workers()
      adapter_state$worker_token <- worker_token
      promise <- promises::future_promise({
        compute_metric_adapters(data, by = by)
      })
      promises::then(
        promise,
        onFulfilled = function(value) {
          if (identical(adapter_state$key, key)) {
            adapter_metrics(value)
          }
          schedule_background_worker_cleanup(worker_token)
        },
        onRejected = function(error) {
          if (identical(adapter_state$key, key)) {
            adapter_metrics(NULL)
          }
          schedule_background_worker_cleanup(worker_token)
        }
      )
      NULL
    })

    metrics <- shiny::reactive({
      state <- base_metric_state()
      if (!should_start_additional_metrics(state)) {
        return(state$base)
      }
      base <- state$base
      adapters <- adapter_metrics()
      if (is.null(adapters) && is_active_tab(active_tab, "export")) {
        adapters <- compute_metric_adapters(state$data, by = default_metric_groups(state$data))
        adapter_metrics(adapters)
      }
      if (is.null(adapters)) {
        return(base)
      }
      merge_core_metric_outputs(base, adapters, by = default_metric_groups(base))
    })

    display_metric_state <- shiny::reactive({
      state <- base_metric_state()
      if (!should_start_additional_metrics(state)) {
        return(state)
      }
      tryCatch({
        raw_metrics <- metrics()
        display <- prepare_metrics_display(raw_metrics)
        if (!nrow(display)) {
          return(metric_state("no_analysis_rows", data = state$data, base = raw_metrics, display = display))
        }
        metric_state("base_ready", data = state$data, base = raw_metrics, display = display)
      }, error = function(error) {
        metric_state("base_error", data = state$data, base = state$base, display = state$display, error = conditionMessage(error))
      })
    })

    display_metrics <- shiny::reactive({
      display_metric_state()$display
    })

    output$participant_filter <- shiny::renderUI({
      state <- display_metric_state()
      if (!identical(state$status, "base_ready") || !"Subject ID" %in% names(state$display)) {
        return(NULL)
      }
      display <- state$display
      choices <- metric_participant_filter_choices(display)
      shiny::selectInput(
        session$ns("participant"),
        "Subject ID",
        choices = choices,
        selected = preserve_filter_selection(input$participant, choices)
      )
    })

    output$category_filter <- shiny::renderUI({
      display <- display_metrics()
      choices <- metric_category_filter_choices(display)
      shiny::selectInput(
        session$ns("category"),
        "Metric category",
        choices = choices,
        selected = preserve_filter_selection(input$category, choices)
      )
    })

    output$group_filter <- shiny::renderUI({
      display <- display_metrics()
      if (!"Group" %in% names(display) || length(clean_filter_values(display$Group)) < 2L) {
        return(NULL)
      }
      choices <- filter_select_choices(sort(unique(display$Group)), all_label = "All")
      shiny::selectInput(
        session$ns("group"),
        "Group",
        choices = choices,
        selected = preserve_filter_selection(input$group, choices)
      )
    })

    output$visit_filter <- shiny::renderUI({
      display <- display_metrics()
      if (!"Visit" %in% names(display) || length(clean_filter_values(display$Visit)) < 2L) {
        return(NULL)
      }
      choices <- filter_select_choices(sort(unique(display$Visit)), all_label = "All")
      shiny::selectInput(
        session$ns("visit"),
        "Visit",
        choices = choices,
        selected = preserve_filter_selection(input$visit, choices)
      )
    })

    card_display <- shiny::reactive({
      filter_metrics_display(
        display_metrics(),
        participant = input$participant %||% "",
        group = input$group %||% "",
        visit = input$visit %||% "",
        include_category = FALSE
      )
    })

    filtered_display <- shiny::reactive({
      filter_metrics_display(
        card_display(),
        category = input$category %||% "",
        include_category = TRUE
      )
    })

    output$summary_cards <- shiny::renderUI({
      state <- display_metric_state()
      if (!identical(state$status, "base_ready")) {
        return(shiny::div(class = "alert alert-info", state$message))
      }
      display <- card_display()
      if (!nrow(display)) {
        return(shiny::div(class = "alert alert-info", "No metrics match the current filters."))
      }
      key_metrics <- c(
        "Mean glucose",
        "Coefficient of variation",
        "Time in range (70-180 mg/dL)",
        "Time below range (<70 mg/dL)",
        "Time above range (>180 mg/dL)",
        "Glucose management indicator"
      )
      cards <- lapply(key_metrics, function(metric_name) {
        values <- display$Value[display$Metric == metric_name]
        units <- display$Units[display$Metric == metric_name]
        value <- if (length(values) && any(!is.na(values))) round(mean(values, na.rm = TRUE), 2) else NA_real_
        unit <- if (length(units)) units[[1L]] else ""
        shiny::div(
          class = "card",
          style = "display:inline-block; min-width: 150px; margin: 0 8px 12px 0; padding: 12px;",
          shiny::div(style = "font-size: 0.85rem; color: #555;", metric_name),
          shiny::div(style = "font-size: 1.35rem; font-weight: 600;", ifelse(is.na(value), "NA", paste(value, unit)))
        )
      })
      shiny::tagList(cards)
    })

    output$metrics_empty_state <- shiny::renderUI({
      state <- display_metric_state()
      if (!identical(state$status, "base_ready")) {
        return(NULL)
      }
      if (nrow(filtered_display())) {
        return(NULL)
      }
      shiny::div(class = "alert alert-info", "No metrics match the current filters.")
    })

    output$metrics_table <- DT::renderDT({
      DT::datatable(filtered_display(), rownames = FALSE, options = list(scrollX = FALSE, pageLength = 15))
    })

    metrics
  })
}
