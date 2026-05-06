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
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(
          ns("plot_type"),
          "Plot type",
          choices = c("Trace" = "trace", "Daily overlay" = "daily_overlay", "AGP summary" = "agp")
        )
      ),
      shiny::column(3, shiny::selectInput(ns("participant"), "Participant", choices = character())),
      shiny::column(3, shiny::uiOutput(ns("group_filter")))
    ),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("visit_filter"))),
      shiny::column(3, shiny::uiOutput(ns("day_filter")))
    ),
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("active_plot"), height = "460px"), type = 4)
  )
}

plots_module_server <- function(id, standardized, metrics, settings, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(standardized(), {
      req_active_tab(active_tab, "plots")
      ids <- unique(standardized()$id)
      update_filter_select(session, "participant", sort(ids), selected = input$participant)
    }, ignoreInit = FALSE)

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

    output$visit_filter <- shiny::renderUI({
      req_active_tab(active_tab, "plots")
      data <- standardized()
      if (!plot_filter_available(data, "visit", min_values = 2L)) {
        return(NULL)
      }
      choices <- filter_select_choices(sort(unique(data$visit)), all_label = "All")
      shiny::selectInput(
        session$ns("visit"),
        "Visit",
        choices = choices,
        selected = preserve_filter_selection(input$visit, choices)
      )
    })

    output$day_filter <- shiny::renderUI({
      req_active_tab(active_tab, "plots")
      if (!identical(input$plot_type, "daily_overlay")) {
        return(NULL)
      }
      days <- available_plot_days(
        standardized(),
        participant = input$participant,
        group = input$group,
        visit = input$visit
      )
      choices <- filter_select_choices(days, all_label = "All days")
      shiny::selectInput(
        session$ns("day"),
        "Day",
        choices = choices,
        selected = preserve_filter_selection(input$day, choices)
      )
    })

    active_plot <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "plots")
      thresholds <- settings()$thresholds_mg_dl
      plot_type <- input$plot_type %||% "trace"
      if (identical(plot_type, "daily_overlay")) {
        return(create_daily_overlay_plot(
          standardized(),
          thresholds = thresholds,
          participant = input$participant,
          group = input$group,
          visit = input$visit,
          day = input$day,
          max_points_per_participant = Inf
        ))
      }
      if (identical(plot_type, "agp")) {
        return(create_agp_summary_plot(
          standardized(),
          thresholds = thresholds,
          participant = input$participant,
          group = input$group,
          visit = input$visit
        ))
      }

      data <- filter_plot_data(
        standardized(),
        participant = input$participant,
        group = input$group,
        visit = input$visit
      )
      create_trace_plot(data, thresholds = thresholds, max_points_per_participant = Inf)
    }),
    cgm_data_signature(standardized()),
    threshold_signature(settings()$thresholds_mg_dl),
    input$plot_type,
    input$participant,
    input$group,
    input$visit,
    input$day
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
        paste0("cgm_", input$plot_type %||% "plot", ".png")
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
