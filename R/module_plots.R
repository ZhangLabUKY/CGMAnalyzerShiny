plot_filter_layout_order <- function(plot_type = "trace") {
  if (identical(plot_type, "daily_overlay")) {
    c("day_filter", "subject_filter", "group_filter")
  } else {
    c("subject_filter", "group_filter")
  }
}

plot_filter_layout_ui <- function(ns, plot_type = "trace") {
  filters <- plot_filter_layout_order(plot_type)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(
          ns("plot_type"),
          "Plot type",
          choices = c("Trace" = "trace", "Daily overlay" = "daily_overlay", "AGP summary" = "agp"),
          selected = plot_type
        )
      ),
      lapply(filters, function(filter_id) {
        shiny::column(3, shiny::uiOutput(ns(filter_id)))
      })
    )
  )
}

plots_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(9, shiny::h3("Plots")),
      shiny::column(
        3,
        shiny::div(
          style = "text-align: right; padding-top: 12px;",
          shiny::downloadButton(ns("download_plot"), "Download PNG")
        )
      )
    ),
    shiny::uiOutput(ns("filter_layout")),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("plot_summary")), type = 4),
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("active_plot"), height = "460px"), type = 4)
  )
}

plots_module_server <- function(id, standardized, metrics, settings, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    day_selection <- shiny::reactiveVal(all_filter_value())

    output$filter_layout <- shiny::renderUI({
      req_active_tab(active_tab, "plots")
      plot_filter_layout_ui(session$ns, input$plot_type %||% "trace")
    })

    output$subject_filter <- shiny::renderUI({
      req_active_tab(active_tab, "plots")
      data <- standardized()
      if (!subject_id_filter_available(data)) {
        return(NULL)
      }
      choices <- plot_subject_filter_choices(data)
      shiny::selectInput(
        session$ns("participant"),
        "Subject ID",
        choices = choices,
        selected = preserve_filter_selection(input$participant, choices)
      )
    })

    output$group_filter <- shiny::renderUI({
      req_active_tab(active_tab, "plots")
      data <- standardized()
      if (!plot_filter_available(data, "group", min_values = 2L)) {
        return(NULL)
      }
      choices <- filter_select_choices(sort(unique(data$group)), all_label = "All")
      shiny::selectInput(
        session$ns("group"),
        "Group",
        choices = choices,
        selected = preserve_filter_selection(input$group, choices)
      )
    })

    day_choices <- shiny::reactive({
      req_active_tab(active_tab, "plots")
      if (!identical(input$plot_type, "daily_overlay")) {
        return(NULL)
      }
      plot_day_filter_choices(
        standardized(),
        participant = input$participant,
        group = input$group
      )
    })

    shiny::observeEvent(input$day, {
      current <- input$day %||% character()
      current <- as.character(current)
      current <- trimws(current)
      current <- current[!is.na(current) & nzchar(current)]
      if (length(current)) {
        day_selection(normalize_plot_days(current))
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(day_choices(), {
      choices <- day_choices()
      if (is.null(choices)) {
        return()
      }
      preserved <- preserve_plot_day_selection(day_selection(), choices)
      if (!identical(preserved, day_selection())) {
        day_selection(preserved)
      }
    }, ignoreInit = FALSE)

    output$day_filter <- shiny::renderUI({
      req_active_tab(active_tab, "plots")
      choices <- day_choices()
      if (is.null(choices)) {
        return(NULL)
      }
      shiny::selectizeInput(
        session$ns("day"),
        "Day",
        choices = choices,
        selected = preserve_plot_day_selection(day_selection(), choices),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    normalized_day <- shiny::reactive({
      normalize_plot_days(day_selection())
    })

    plot_display_context <- shiny::reactive({
      req_active_tab(active_tab, "plots")
      plot_type <- input$plot_type %||% "trace"
      filtered <- plot_filtered_data(
        standardized(),
        plot_type = plot_type,
        participant = input$participant,
        group = input$group,
        day = normalized_day()
      )
      if (identical(plot_type, "agp")) {
        return(list(
          filtered = filtered,
          max_points_per_participant = Inf,
          displayed_rows = nrow(filtered),
          optimized = FALSE
        ))
      }
      max_points <- adaptive_plot_max_points_per_subject(filtered)
      displayed_rows <- plot_display_row_count(filtered, max_points_per_participant = max_points)
      list(
        filtered = filtered,
        max_points_per_participant = max_points,
        displayed_rows = displayed_rows,
        optimized = displayed_rows < nrow(filtered)
      )
    })

    output$plot_summary <- shiny::renderUI({
      req_active_tab(active_tab, "plots")
      plot_type <- input$plot_type %||% "trace"
      display_context <- plot_display_context()
      filtered <- display_context$filtered
      note <- if (identical(plot_type, "daily_overlay")) {
        daily_overlay_legend_note(filtered)
      } else {
        ""
      }
      display_note <- if (isTRUE(display_context$optimized)) {
        "Interactive plot optimized for display; analyses and data exports use full data."
      } else {
        ""
      }
      shiny::tagList(
        summary_card_ui(
          plot_selection_summary(
            standardized(),
            plot_type = plot_type,
            participant = input$participant,
            group = input$group,
            day = normalized_day(),
            displayed_rows = display_context$displayed_rows
          ),
          compact = TRUE
        ),
        if (nzchar(note)) {
          shiny::div(class = "alert alert-info", note)
        },
        if (nzchar(display_note)) {
          shiny::div(class = "alert alert-info", display_note)
        }
      )
    })

    active_plot <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "plots")
      thresholds <- settings()$thresholds_mg_dl
      plot_type <- input$plot_type %||% "trace"
      display_context <- plot_display_context()
      if (identical(plot_type, "daily_overlay")) {
        return(create_daily_overlay_plot(
          standardized(),
          thresholds = thresholds,
          participant = input$participant,
          group = input$group,
          day = normalized_day(),
          max_points_per_participant = display_context$max_points_per_participant
        ))
      }
      if (identical(plot_type, "agp")) {
        return(create_agp_summary_plot(
          standardized(),
          thresholds = thresholds,
          participant = input$participant,
          group = input$group
        ))
      }

      data <- filter_plot_data(
        standardized(),
        participant = input$participant,
        group = input$group
      )
      create_trace_plot(
        data,
        thresholds = thresholds,
        max_points_per_participant = display_context$max_points_per_participant
      )
    }),
    cgm_data_signature(standardized()),
    threshold_signature(settings()$thresholds_mg_dl),
    input$plot_type,
    input$participant,
    input$group,
    plot_day_cache_key(normalized_day()),
    adaptive_plot_target_points()
    )

    output$active_plot <- plotly::renderPlotly({
      plotly_plot <- plotly::ggplotly(active_plot())
      if (identical(input$plot_type, "agp")) {
        plotly_plot <- layout_agp_plotly(plotly_plot)
      }
      plotly_plot
    })

    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        plot_download_filename(
          standardized(),
          plot_type = input$plot_type %||% "trace",
          participant = input$participant,
          group = input$group,
          day = normalized_day()
        )
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = active_plot(), width = 10, height = 6, dpi = 300, bg = "white")
      }
    )

    shiny::reactive({
      active_plot()
    })
  })
}
