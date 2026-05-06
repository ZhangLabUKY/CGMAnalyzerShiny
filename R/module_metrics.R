metrics_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Core Metrics"),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("summary_cards")), type = 4),
    shiny::uiOutput(ns("adapter_status")),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("participant"), "Participant", choices = character())),
      shiny::column(3, shiny::uiOutput(ns("group_filter"))),
      shiny::column(3, shiny::uiOutput(ns("visit_filter"))),
      shiny::column(3, shiny::selectInput(ns("category"), "Metric category", choices = character()))
    ),
    shiny::uiOutput(ns("metrics_empty_state")),
    shinycssloaders::withSpinner(DT::DTOutput(ns("metrics_table")), type = 4)
  )
}

metrics_module_server <- function(id, standardized, settings, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    base_metrics <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, c("metrics", "statistics", "export"))
      compute_base_core_metrics(
        standardized(),
        thresholds = settings()$thresholds_mg_dl
      )
    }),
    cgm_data_signature(standardized()),
    threshold_signature(settings()$thresholds_mg_dl)
    )

    adapter_metrics <- shiny::reactiveVal(NULL)
    adapter_status <- shiny::reactiveVal("")
    adapter_key_started <- shiny::reactiveVal(NULL)

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
      key <- adapter_key()
      if (identical(adapter_key_started(), key)) {
        return(NULL)
      }
      adapter_key_started(key)
      adapter_metrics(NULL)
      adapter_status("Additional metrics are being calculated.")

      data <- standardized()
      by <- default_metric_groups(data)
      promise <- promises::future_promise({
        compute_metric_adapters(data, by = by)
      })
      promises::then(
        promise,
        onFulfilled = function(value) {
          if (identical(adapter_key_started(), key)) {
            adapter_metrics(value)
            adapter_status("")
          }
        },
        onRejected = function(error) {
          if (identical(adapter_key_started(), key)) {
            adapter_metrics(NULL)
            adapter_status("Additional metrics could not be calculated. Core metrics are still available.")
          }
        }
      )
      NULL
    })

    metrics <- shiny::reactive({
      base <- base_metrics()
      adapters <- adapter_metrics()
      if (is.null(adapters) && is_active_tab(active_tab, "export")) {
        adapters <- compute_metric_adapters(standardized(), by = default_metric_groups(standardized()))
        adapter_metrics(adapters)
        adapter_status("")
      }
      if (is.null(adapters)) {
        return(base)
      }
      merge_core_metric_outputs(base, adapters, by = default_metric_groups(base))
    })

    display_metrics <- shiny::reactive({
      prepare_metrics_display(metrics())
    })

    output$adapter_status <- shiny::renderUI({
      req_active_tab(active_tab, "metrics")
      status <- adapter_status()
      if (!nzchar(status)) {
        return(NULL)
      }
      shiny::div(
        class = "alert alert-info",
        style = "padding: 8px 12px;",
        status
      )
    })

    shiny::observeEvent(display_metrics(), {
      display <- display_metrics()
      update_filter_select(session, "participant", sort(unique(display$Participant)), selected = input$participant)
      categories <- metric_category_order()
      categories <- categories[categories %in% unique(display$Category)]
      update_filter_select(session, "category", categories, selected = input$category)
    }, ignoreInit = FALSE)

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
      display <- card_display()
      if (!nrow(display)) {
        return(shiny::div(class = "alert alert-info", "Select mapped glucose and timestamp columns to calculate metrics."))
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
