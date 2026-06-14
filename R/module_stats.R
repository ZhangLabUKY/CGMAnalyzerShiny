statistics_result_summary_ui <- function(result, test_type = "welch_t") {
  if (!is.data.frame(result) || !nrow(result)) {
    return(NULL)
  }
  note <- result$Note[[1L]] %||% ""
  has_note <- nzchar(note)
  shiny::div(
    class = paste(
      "cgm-stat-result-card",
      if (has_note) "cgm-stat-result-card-review" else "cgm-stat-result-card-ready"
    ),
    shiny::div(
      class = "cgm-stat-result-card-header",
      shiny::div(
        shiny::span(class = "cgm-stat-result-eyebrow", result$Test[[1L]]),
        shiny::h5(result$Metric[[1L]])
      ),
      shiny::span(
        class = paste("cgm-stat-result-badge", if (has_note) "is-review" else "is-ready"),
        if (has_note) "Review" else "Ready"
      )
    ),
    shiny::div(
      class = "cgm-stat-result-grid",
      shiny::div(class = "cgm-stat-kv", shiny::span("Comparison"), shiny::strong(result$Groups[[1L]] %||% "NA")),
      shiny::div(class = "cgm-stat-kv", shiny::span("N"), shiny::strong(result$N[[1L]] %||% "NA")),
      shiny::div(class = "cgm-stat-kv", shiny::span("Statistic"), shiny::strong(format_stat_number(result$Statistic[[1L]]))),
      shiny::div(class = "cgm-stat-kv", shiny::span("P-value"), shiny::strong(format_p_value(result$`P-value`[[1L]])))
    ),
    if (has_note) {
      shiny::div(class = "cgm-stat-note is-review", note)
    },
    shiny::div(class = "cgm-stat-note", stat_method_note(test_type))
  )
}

stats_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "cgm-statistics-dashboard",
    shiny::div(
      class = "cgm-statistics-overview",
      shiny::h3("Statistics")
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-statistics-analysis-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Analysis setup"),
        shiny::div(
          class = "cgm-filter-bar cgm-statistics-filter-bar",
          shiny::selectInput(ns("metric"), "Outcome metric", choices = character()),
          shiny::selectInput(ns("grouping"), "Grouping column", choices = character()),
          shiny::uiOutput(ns("period_filter")),
          shiny::selectInput(
            ns("test_type"),
            "Test",
            choices = c("Welch t-test" = "welch_t", "Wilcoxon rank-sum" = "wilcoxon")
          )
        )
      ),
      shiny::div(
        class = "cgm-dashboard-section-body",
        shiny::uiOutput(ns("stats_status_note"))
      )
    ),
    shiny::div(
      class = "cgm-statistics-results-grid",
      shiny::tags$section(
        class = "cgm-dashboard-section cgm-statistics-summary-section",
        shiny::div(
          class = "cgm-dashboard-section-header",
          shiny::h4("Group summary")
        ),
        shiny::div(
          class = "cgm-dashboard-section-body cgm-table-panel",
          shinycssloaders::withSpinner(DT::DTOutput(ns("group_summary")), type = 4)
        )
      ),
      shiny::tags$section(
        class = "cgm-dashboard-section cgm-statistics-result-section",
        shiny::div(
          class = "cgm-dashboard-section-header",
          shiny::h4("Test result")
        ),
        shiny::div(
          class = "cgm-dashboard-section-body",
          shiny::uiOutput(ns("test_result_summary")),
          shinycssloaders::withSpinner(DT::DTOutput(ns("test_result")), type = 4)
        )
      )
    )
  )
}

stats_module_server <- function(id, metrics, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_period <- shiny::reactive({
      normalize_time_window(input$period %||% default_time_window())
    })

    statistics_metrics <- shiny::reactive({
      req_active_tab(active_tab, "statistics")
      filter_metrics_by_period(metrics(), selected_period())
    })

    output$period_filter <- shiny::renderUI({
      req_active_tab(active_tab, "statistics")
      data <- statistics_metrics()
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

    shiny::observe({
      req_active_tab(active_tab, "statistics")
      current_metrics <- statistics_metrics()
      metric_choices <- metric_test_choices(current_metrics)
      grouping <- grouping_choices(current_metrics)
      shiny::updateSelectInput(
        session,
        "metric",
        choices = metric_choices,
        selected = if (length(metric_choices)) {
          metric_values <- unname(metric_choices)
          if ((input$metric %||% "") %in% metric_values) input$metric else metric_values[[1L]]
        } else {
          character()
        }
      )
      shiny::updateSelectInput(
        session,
        "grouping",
        choices = stats::setNames(grouping, vapply(grouping, format_grouping_label, character(1))),
        selected = if (length(grouping)) {
          if ((input$grouping %||% "") %in% grouping) input$grouping else grouping[[1L]]
        } else {
          character()
        }
      )
    })

    selected_test_result <- shiny::reactive({
      metric <- input$metric
      grouping <- input$grouping
      if (is.null(metric) || !nzchar(metric) || is.null(grouping) || !nzchar(grouping)) {
        return(insufficient_stat_result(
          metric = metric %||% "",
          grouping = grouping %||% "",
          test_type = input$test_type %||% "welch_t",
          note = "Select a metric and grouping variable to run a test."
        ))
      }
      run_metric_stat_test(
        statistics_metrics(),
        metric = metric,
        grouping = grouping,
        test_type = input$test_type,
        period = selected_period()
      )
    })

    output$stats_status_note <- shiny::renderUI({
      req_active_tab(active_tab, "statistics")
      current_metrics <- statistics_metrics()
      if (!is.data.frame(current_metrics) || !nrow(current_metrics)) {
        return(shiny::div(class = "cgm-compact-info-note", "Metrics are required before statistical testing."))
      }
      if (!length(grouping_choices(current_metrics))) {
        return(shiny::div(
          class = "cgm-compact-info-note",
          "Add a two-level subject metadata column, such as group or sex, to enable two-sample tests."
        ))
      }
      shiny::div(
        class = "cgm-stat-method-note",
        "Two-sample tests use one row per Subject ID for the selected period."
      )
    })

    output$group_summary <- DT::renderDT({
      req_active_tab(active_tab, "statistics")
      metric <- input$metric
      grouping <- input$grouping
      summary <- if (is.null(metric) || !nzchar(metric) || is.null(grouping) || !nzchar(grouping)) {
        summarize_metric_by_group(data.frame(), metric %||% "", grouping %||% "")
      } else {
        summarize_metric_by_group(statistics_metrics(), metric = metric, grouping = grouping, period = selected_period())
      }
      DT::datatable(summary, rownames = FALSE, options = list(dom = "t", scrollX = FALSE))
    })

    output$test_result_summary <- shiny::renderUI({
      req_active_tab(active_tab, "statistics")
      statistics_result_summary_ui(selected_test_result(), input$test_type %||% "welch_t")
    })

    output$test_result <- DT::renderDT({
      req_active_tab(active_tab, "statistics")
      result <- selected_test_result()
      DT::datatable(result, rownames = FALSE, options = list(dom = "t", scrollX = FALSE))
    })
  })
}
