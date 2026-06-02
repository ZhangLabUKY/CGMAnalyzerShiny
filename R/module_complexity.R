complexity_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Complexity analytics"),
    shiny::p("Complexity metrics describe regularity and long-range structure in glucose patterns after the current preprocessing choices are applied."),
    shiny::tags$style(
      "
      .complexity-controls-panel {
        display: grid;
        grid-template-columns: minmax(220px, 0.9fr) minmax(360px, 1.5fr) minmax(220px, 0.8fr);
        gap: 16px;
        align-items: start;
        margin: 12px 0 18px;
      }
      .complexity-control-group {
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        padding: 12px 14px 6px;
        background: #fff;
      }
      .complexity-control-group h4 {
        margin: 0 0 10px;
        font-size: 15px;
        font-weight: 600;
      }
      .complexity-control-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(140px, 1fr));
        gap: 10px 14px;
      }
      .complexity-control-grid .form-group {
        margin-bottom: 8px;
      }
      @media (max-width: 1100px) {
        .complexity-controls-panel {
          grid-template-columns: 1fr;
        }
      }
      @media (max-width: 650px) {
        .complexity-control-grid {
          grid-template-columns: 1fr;
        }
      }
      "
    ),
    shiny::div(
      class = "complexity-controls-panel",
      shiny::div(
        class = "complexity-control-group",
        shiny::h4("Filters"),
        shiny::div(
          class = "complexity-control-grid",
          shiny::uiOutput(ns("subject_filter")),
          shiny::uiOutput(ns("group_filter"))
        )
      ),
      shiny::div(
        class = "complexity-control-group",
        shiny::h4("Core parameters"),
        shiny::div(
          class = "complexity-control-grid",
          shiny::numericInput(ns("min_points"), "Minimum usable points", value = 100, min = 20, step = 10),
          shiny::numericInput(ns("entropy_bin_width"), "Entropy bin width", value = 10, min = 1, step = 1),
          shiny::numericInput(ns("embedding_dimension"), "Embedding dimension", value = 2, min = 2, step = 1)
        )
      ),
      shiny::div(
        class = "complexity-control-group",
        shiny::h4("Curve parameters"),
        shiny::div(
          class = "complexity-control-grid",
          shiny::numericInput(ns("mse_scale_max"), "MSE max scale", value = 5, min = 1, step = 1),
          shiny::numericInput(ns("higuchi_kmax"), "Higuchi kmax", value = 8, min = 2, step = 1)
        )
      )
    ),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("summary_cards")), type = 4),
    shiny::uiOutput(ns("mse_status_note")),
    shiny::h4("Visual summary"),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns("visual_mode"), "Visual type", choices = complexity_visual_mode_choices(), selected = "metric_summary")),
      shiny::column(3, shiny::uiOutput(ns("metric_filter"))),
      shiny::column(3, shiny::uiOutput(ns("curve_filter")))
    ),
    shiny::uiOutput(ns("complexity_plot_ui")),
    shiny::div(
      style = "display:flex; justify-content:space-between; align-items:center; gap:12px;",
      shiny::h4("Complexity metrics"),
      shiny::downloadButton(ns("download_complexity"), "Download CSV")
    ),
    shiny::uiOutput(ns("metrics_table_ui"))
  )
}

complexity_loading_ui <- function(message = "Complexity metrics are calculating.") {
  shiny::div(
    class = "complexity-loading",
    style = "display:flex; align-items:center; gap:12px; min-height:96px;",
    shiny::tags$style("@keyframes cgm-complexity-spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }"),
    shiny::div(style = "width:28px; height:28px; border:4px solid #d9edf7; border-top-color:#31708f; border-radius:50%; animation:cgm-complexity-spin 0.8s linear infinite;"),
    shiny::span(message)
  )
}

complexity_module_server <- function(id, standardized, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    compute_data <- shiny::reactive({
      req_active_tab(active_tab, "complexity")
      standardized()
    })

    subject_choices_data <- shiny::reactive({
      req_active_tab(active_tab, "complexity")
      filter_complexity_data(standardized(), group = input$group)
    })

    output$subject_filter <- shiny::renderUI({
      req_active_tab(active_tab, "complexity")
      data <- subject_choices_data()
      if (!subject_id_filter_available(data)) {
        return(NULL)
      }
      choices <- filter_select_choices(sort(subject_id_values(data)), all_label = "All")
      shiny::selectInput(
        session$ns("subject"),
        "Subject ID",
        choices = choices,
        selected = preserve_filter_selection(input$subject, choices)
      )
    })

    output$group_filter <- shiny::renderUI({
      req_active_tab(active_tab, "complexity")
      data <- compute_data()
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

    parameters <- shiny::reactive({
      complexity_default_parameters(
        min_points = input$min_points %||% 100,
        entropy_bin_width = input$entropy_bin_width %||% 10,
        embedding_dimension = input$embedding_dimension %||% 2,
        mse_scale_max = input$mse_scale_max %||% 5,
        higuchi_kmax = input$higuchi_kmax %||% 8,
        max_gap_intervals = 4
      )
    })

    display_data <- shiny::reactive({
      req_active_tab(active_tab, "complexity")
      filter_complexity_data(standardized(), subject = input$subject, group = input$group)
    })

    pending_complexity_results <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "complexity")
      cgm_timed(
        "complexity_pending_summary",
        compute_complexity_pending_summary(compute_data(), parameters(), status = "running")
      )
    }),
    cgm_data_signature(compute_data()),
    parameters()
    )

    complexity_metrics <- shiny::reactiveVal(NULL)
    dfa_higuchi_curves <- shiny::reactiveVal(NULL)
    mse_complexity_curves <- shiny::reactiveVal(NULL)
    quick_status <- shiny::reactiveVal("idle")
    hurst_status <- shiny::reactiveVal("idle")
    curve_status <- shiny::reactiveVal("idle")
    mse_status <- shiny::reactiveVal("idle")
    complexity_state <- new.env(parent = emptyenv())
    complexity_state$key <- NULL
    complexity_state$worker_token <- NULL
    complexity_state$cleanup_scheduled <- FALSE
    complexity_state$hurst_pending <- FALSE
    complexity_state$curve_pending <- FALSE
    complexity_state$mse_pending <- FALSE
    complexity_state$mse_planned <- FALSE

    complexity_key <- shiny::reactive({
      complexity_compute_key(compute_data(), parameters())
    })

    reset_complexity_job_state <- function() {
      complexity_state$worker_token <- NULL
      complexity_state$cleanup_scheduled <- FALSE
      complexity_state$hurst_pending <- FALSE
      complexity_state$curve_pending <- FALSE
      complexity_state$mse_pending <- FALSE
      complexity_state$mse_planned <- FALSE
      hurst_status("idle")
      curve_status("idle")
      mse_status("idle")
    }

    maybe_schedule_complexity_worker_cleanup <- function(key) {
      if (!identical(complexity_state$key, key)) {
        return(FALSE)
      }
      if (isTRUE(complexity_state$cleanup_scheduled) || is.null(complexity_state$worker_token)) {
        return(FALSE)
      }
      if (
        isTRUE(complexity_state$hurst_pending) ||
          isTRUE(complexity_state$curve_pending) ||
          isTRUE(complexity_state$mse_pending) ||
          isTRUE(complexity_state$mse_planned)
      ) {
        return(FALSE)
      }
      complexity_state$cleanup_scheduled <- TRUE
      schedule_background_worker_cleanup(complexity_state$worker_token)
    }

    run_quick_job <- function(data, params, key, failed_results) {
      quick_status("running")
      complexity_state$worker_token <- configure_background_workers()
      complexity_state$cleanup_scheduled <- FALSE
      promise <- promises::future_promise({
        cgm_timed(
          "complexity_quick_metrics",
          compute_complexity_quick_metrics(data, params)
        )
      })
      promises::then(
        promise,
        onFulfilled = function(value) {
          if (!identical(complexity_state$key, key)) {
            return(NULL)
          }
          quick_ok <- is.data.frame(value) && nrow(value) > 0L
          quick_results <- if (quick_ok) value else failed_results
          if (quick_ok) {
            complexity_metrics(merge_complexity_hurst_results(quick_results, NULL, status = "running"))
            quick_status("complete")
            eligible_ids <- quick_results$id[quick_results$eligible %in% TRUE]
            if (length(eligible_ids)) {
              complexity_state$mse_planned <- TRUE
              mse_status("running")
              run_hurst_job(data, params, key)
              run_dfa_higuchi_curve_job(data, params, key)
            } else {
              hurst_status("idle")
              curve_status("idle")
              mse_status("idle")
              maybe_schedule_complexity_worker_cleanup(key)
            }
          } else {
            complexity_metrics(failed_results)
            quick_status("failed")
            hurst_status("idle")
            curve_status("idle")
            mse_status("idle")
            maybe_schedule_complexity_worker_cleanup(key)
          }
          NULL
        },
        onRejected = function(error) {
          if (identical(complexity_state$key, key)) {
            complexity_metrics(failed_results)
            quick_status("failed")
            hurst_status("idle")
            curve_status("idle")
            mse_status("idle")
            maybe_schedule_complexity_worker_cleanup(key)
          }
          NULL
        }
      )
      NULL
    }

    run_hurst_job <- function(data, params, key) {
      complexity_state$hurst_pending <- TRUE
      hurst_status("running")
      promise <- promises::future_promise({
        cgm_timed(
          "complexity_hurst_background",
          compute_complexity_hurst_metrics(data, params)
        )
      })
      promises::then(
        promise,
        onFulfilled = function(value) {
          if (identical(complexity_state$key, key)) {
            complexity_metrics(merge_complexity_hurst_results(complexity_metrics(), value, status = "complete"))
            complexity_state$hurst_pending <- FALSE
            hurst_status("complete")
            maybe_schedule_complexity_worker_cleanup(key)
          }
        },
        onRejected = function(error) {
          if (identical(complexity_state$key, key)) {
            complexity_metrics(merge_complexity_hurst_results(complexity_metrics(), NULL, status = "failed"))
            complexity_state$hurst_pending <- FALSE
            hurst_status("failed")
            maybe_schedule_complexity_worker_cleanup(key)
          }
        }
      )
      NULL
    }

    run_dfa_higuchi_curve_job <- function(data, params, key) {
      curve_status("running")
      complexity_state$curve_pending <- TRUE
      promise <- promises::future_promise({
        cgm_timed(
          "complexity_dfa_higuchi_background",
          compute_complexity_dfa_higuchi_curves(data, params)
        )
      })
      promises::then(
        promise,
        onFulfilled = function(value) {
          if (identical(complexity_state$key, key)) {
            dfa_higuchi_curves(if (is.data.frame(value)) value else NULL)
            curve_status("complete")
            complexity_state$curve_pending <- FALSE
            run_mse_curve_job(data, params, key)
            maybe_schedule_complexity_worker_cleanup(key)
          }
        },
        onRejected = function(error) {
          if (identical(complexity_state$key, key)) {
            dfa_higuchi_curves(NULL)
            curve_status("failed")
            complexity_state$curve_pending <- FALSE
            run_mse_curve_job(data, params, key)
            maybe_schedule_complexity_worker_cleanup(key)
          }
        }
      )
      NULL
    }

    run_mse_curve_job <- function(data, params, key) {
      complexity_state$mse_planned <- FALSE
      complexity_state$mse_pending <- TRUE
      mse_status("running")
      promise <- promises::future_promise({
        cgm_timed(
          "complexity_mse_background",
          compute_complexity_mse_curves(
            data,
            min_points = params$min_points,
            entropy_bin_width = params$entropy_bin_width,
            embedding_dimension = params$embedding_dimension,
            mse_scale_max = params$mse_scale_max,
            higuchi_kmax = params$higuchi_kmax,
            max_gap_intervals = params$max_gap_intervals
          )
        )
      })
      promises::then(
        promise,
        onFulfilled = function(value) {
          if (identical(complexity_state$key, key)) {
            mse_complexity_curves(if (is.data.frame(value)) value else NULL)
            mse_status(if (is.data.frame(value) && nrow(value)) "complete" else "failed")
            complexity_state$mse_pending <- FALSE
            maybe_schedule_complexity_worker_cleanup(key)
          }
        },
        onRejected = function(error) {
          if (identical(complexity_state$key, key)) {
            mse_complexity_curves(NULL)
            mse_status("failed")
            complexity_state$mse_pending <- FALSE
            maybe_schedule_complexity_worker_cleanup(key)
          }
        }
      )
      NULL
    }

    shiny::observe({
      req_active_tab(active_tab, "complexity")
      pending_results <- pending_complexity_results()
      key <- complexity_key()
      if (identical(complexity_state$key, key)) {
        return(NULL)
      }
      complexity_state$key <- key
      complexity_metrics(NULL)
      dfa_higuchi_curves(NULL)
      mse_complexity_curves(NULL)
      reset_complexity_job_state()

      if (!is.data.frame(pending_results) || !nrow(pending_results)) {
        quick_status("idle")
        curve_status("idle")
        mse_status("idle")
        return(NULL)
      }

      quick_status("running")
      curve_status("idle")
      mse_status("idle")
      data <- compute_data()
      params <- parameters()
      failed_results <- compute_complexity_pending_summary(data, params, status = "failed")
      run_quick_job(data, params, key, failed_results)
      NULL
    })

    displayed_complexity_results <- shiny::reactive({
      results <- complexity_metrics()
      status <- quick_status()
      if (identical(status, "complete") && is.data.frame(results) && nrow(results)) {
        return(filter_complexity_results(
          results,
          compute_data(),
          subject = input$subject,
          group = input$group
        ))
      }
      NULL
    })

    complexity_output_ids <- c("summary_cards", "metrics_table", "complexity_plot")
    shiny::observe({
      req_active_tab(active_tab, "complexity")
      status <- quick_status()
      if (identical(status, "running")) {
        lapply(complexity_output_ids, shinycssloaders::showSpinner)
      } else {
        lapply(complexity_output_ids, shinycssloaders::hideSpinner)
      }
      NULL
    })

    complexity_curves <- shiny::reactive({
      rows <- list(dfa_higuchi_curves(), mse_complexity_curves())
      rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
      if (!length(rows)) {
        return(empty_complexity_curve_rows())
      }
      out <- do.call(rbind, rows)
      row.names(out) <- NULL
      out
    })

    displayed_complexity_curves <- shiny::reactive({
      filter_complexity_curves(
        complexity_curves(),
        compute_data(),
        subject = input$subject,
        group = input$group
      )
    })

    force_subject_id_display <- shiny::reactive({
      specific_filter_selected(input$subject)
    })

    output$summary_cards <- shiny::renderUI({
      status <- quick_status()
      if (identical(status, "running")) {
        return(complexity_loading_ui("Complexity summary cards are calculating."))
      }
      if (identical(status, "failed")) {
        return(shiny::div(class = "alert alert-warning", "Complexity metrics could not be computed for the current selection."))
      }
      results <- displayed_complexity_results()
      if (!is.data.frame(results) || !nrow(results)) {
        return(shiny::div(class = "text-muted", "No complexity results are available for the current selection."))
      }
      summary_card_ui(complexity_summary_cards(results, parameters()), compact = TRUE)
    })

    output$mse_status_note <- shiny::renderUI({
      req_active_tab(active_tab, "complexity")
      notes <- c(
        complexity_status_text(quick_status()),
        complexity_scalar_status_text(hurst_status()),
        complexity_curve_status_text(curve_status()),
        complexity_mse_status_text(mse_status())
      )
      note <- paste(notes[nzchar(notes)], collapse = " ")
      if (!nzchar(note)) {
        return(NULL)
      }
      shiny::div(class = "alert alert-info", note)
    })

    output$metric_filter <- shiny::renderUI({
      req_active_tab(active_tab, "complexity")
      if (!identical(input$visual_mode %||% "metric_summary", "metric_summary")) {
        return(NULL)
      }
      choices <- complexity_metric_filter_choices()
      shiny::selectInput(
        session$ns("metric"),
        "Complexity metric",
        choices = choices,
        selected = preserve_filter_selection(input$metric, choices)
      )
    })

    output$curve_filter <- shiny::renderUI({
      req_active_tab(active_tab, "complexity")
      if (!identical(input$visual_mode %||% "metric_summary", "scale_curves")) {
        return(NULL)
      }
      choices <- complexity_curve_filter_choices()
      shiny::selectInput(
        session$ns("curve_metric"),
        "Scale curve",
        choices = choices,
        selected = preserve_filter_selection(input$curve_metric, choices)
      )
    })

    complexity_plot_data <- shiny::reactive({
      results <- displayed_complexity_results()
      if (!is.data.frame(results) || !nrow(results)) {
        return(data.frame())
      }
      prepare_complexity_plot_data(
        results,
        display_data(),
        metric = input$metric %||% all_filter_value(),
        show_subject_id = force_subject_id_display()
      )
    })

    scale_curve_plot_data <- shiny::reactive({
      prepare_complexity_curve_plot_data(
        displayed_complexity_curves(),
        display_data(),
        curve_metric = input$curve_metric %||% all_filter_value(),
        show_subject_id = force_subject_id_display()
      )
    })

    output$complexity_plot_ui <- shiny::renderUI({
      mode <- input$visual_mode %||% "metric_summary"
      status <- quick_status()
      if (identical(status, "running")) {
        return(complexity_loading_ui("Complexity plots are calculating."))
      }
      if (identical(status, "failed")) {
        return(shiny::div(class = "alert alert-warning", "Complexity plots could not be computed for the current selection."))
      }
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          session$ns("complexity_plot"),
          height = paste0(complexity_visual_plot_height(mode, input$metric %||% all_filter_value()), "px")
        ),
        type = 4
      )
    })

    output$complexity_plot <- plotly::renderPlotly({
      mode <- input$visual_mode %||% "metric_summary"
      if (identical(quick_status(), "running")) {
        shiny::req(FALSE)
      }
      plot <- if (identical(mode, "scale_curves")) {
        selected_curve <- normalize_filter_value(input$curve_metric %||% all_filter_value())
        if (identical(selected_curve, "mse") && identical(mse_status(), "running")) {
          empty_plot("MSE curves are still calculating. Scalar metrics and DFA/Higuchi curves are available now.")
        } else if (identical(selected_curve, "mse") && identical(mse_status(), "failed")) {
          empty_plot("MSE curves could not be computed for the current selection.")
        } else if ((identical(selected_curve, "dfa") || identical(selected_curve, "higuchi")) && identical(curve_status(), "running")) {
          empty_plot("DFA/Higuchi curves are still calculating. Scalar metrics are available now.")
        } else if ((identical(selected_curve, "dfa") || identical(selected_curve, "higuchi")) && identical(curve_status(), "failed")) {
          empty_plot("DFA/Higuchi curves could not be computed for the current selection.")
        } else {
          create_complexity_scale_curve_plot(scale_curve_plot_data())
        }
      } else {
        create_complexity_summary_plot(
          complexity_plot_data(),
          metric = input$metric %||% all_filter_value()
        )
      }
      cgm_timed(
        "complexity_plotly_render",
        plotly::ggplotly(
          plot,
          tooltip = "text"
        )
      )
    })

    output$metrics_table_ui <- shiny::renderUI({
      status <- quick_status()
      if (identical(status, "running")) {
        return(complexity_loading_ui("Complexity metrics table is calculating."))
      }
      if (identical(status, "failed")) {
        return(shiny::div(class = "alert alert-warning", "Complexity metrics could not be computed for the current selection."))
      }
      shinycssloaders::withSpinner(DT::DTOutput(session$ns("metrics_table")), type = 4)
    })

    output$metrics_table <- DT::renderDT({
      status <- quick_status()
      if (identical(status, "running")) {
        shiny::req(FALSE)
      }
      if (identical(status, "failed")) {
        return(DT::datatable(
          data.frame(
            Status = "Could not compute",
            Message = "Complexity metrics could not be computed for the current selection.",
            stringsAsFactors = FALSE
          ),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      results <- displayed_complexity_results()
      if (!is.data.frame(results) || !nrow(results)) {
        return(DT::datatable(
          data.frame(
            Status = "No results",
            Message = "No complexity results are available for the current selection.",
            stringsAsFactors = FALSE
          ),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      display <- cgm_timed(
        "complexity_metrics_display_prepare",
        prepare_complexity_metrics_display(
          results,
          display_data(),
          show_subject_id = force_subject_id_display()
        )
      )
      cgm_timed(
        "complexity_metrics_table_dt_render",
        DT::datatable(
          display,
          rownames = FALSE,
          options = list(scrollX = TRUE, pageLength = 10)
        ),
        rows = nrow(display)
      )
    })

    output$download_complexity <- shiny::downloadHandler(
      filename = function() "cgm_complexity_metrics.csv",
      content = function(file) {
        results <- displayed_complexity_results()
        out <- if (is.data.frame(results) && nrow(results)) {
          prepare_complexity_export(
            results,
            displayed_complexity_curves(),
            display_data(),
            show_subject_id = force_subject_id_display()
          )
        } else {
          data.frame(
            Status = quick_status(),
            Message = "Complexity results are not available yet for the current selection.",
            stringsAsFactors = FALSE
          )
        }
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    displayed_complexity_results
  })
}
