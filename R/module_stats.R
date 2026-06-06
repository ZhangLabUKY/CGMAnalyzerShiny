stats_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Statistical Testing"),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("metric"), "Outcome metric", choices = character())),
      shiny::column(3, shiny::selectInput(ns("grouping"), "Grouping variable", choices = character())),
      shiny::column(3, shiny::uiOutput(ns("period_filter"))),
      shiny::column(
        3,
        shiny::selectInput(
          ns("test_type"),
          "Test",
          choices = c("Welch t-test" = "welch_t", "Wilcoxon rank-sum" = "wilcoxon")
        )
      )
    ),
    shinycssloaders::withSpinner(DT::DTOutput(ns("test_result")), type = 4)
  )
}

stats_module_server <- function(id, metrics, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_period <- shiny::reactive({
      normalize_time_window(input$period %||% default_time_window())
    })

    statistics_metrics <- shiny::reactive({
      filter_metrics_by_period(metrics(), selected_period())
    })

    output$period_filter <- shiny::renderUI({
      req_active_tab(active_tab, "statistics")
      data <- metrics()
      if (!is.data.frame(data) || !"metric_period" %in% names(data)) {
        return(NULL)
      }
      choices <- time_window_filter_choices()
      shiny::selectInput(
        session$ns("period"),
        "Period",
        choices = choices,
        selected = preserve_filter_selection(input$period %||% default_time_window(), choices)
      )
    })

    shiny::observeEvent(list(metrics(), selected_period()), {
      req_active_tab(active_tab, "statistics")
      current_metrics <- statistics_metrics()
      metric_choices <- metric_test_choices(current_metrics)
      grouping <- grouping_choices(current_metrics)
      shiny::updateSelectInput(
        session,
        "metric",
        choices = metric_choices,
        selected = if (length(metric_choices)) unname(metric_choices[[1L]]) else character()
      )
      shiny::updateSelectInput(
        session,
        "grouping",
        choices = stats::setNames(grouping, vapply(grouping, format_grouping_label, character(1))),
        selected = if (length(grouping)) grouping[[1L]] else character()
      )
    }, ignoreInit = FALSE)

    output$test_result <- DT::renderDT({
      req_active_tab(active_tab, "statistics")
      metric <- input$metric
      grouping <- input$grouping
      if (is.null(metric) || !nzchar(metric) || is.null(grouping) || !nzchar(grouping)) {
        result <- insufficient_stat_result(
          metric = metric %||% "",
          grouping = grouping %||% "",
          test_type = input$test_type %||% "welch_t",
          note = "Select a metric and grouping variable to run a test."
        )
      } else {
        result <- run_metric_stat_test(
          metrics(),
          metric = metric,
          grouping = grouping,
          test_type = input$test_type,
          period = selected_period()
        )
      }
      DT::datatable(result, rownames = FALSE, options = list(dom = "t", scrollX = FALSE))
    })
  })
}
