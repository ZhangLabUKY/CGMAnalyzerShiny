complexity_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "cgm-complexity-dashboard",
    shiny::div(
      class = "cgm-complexity-overview",
      shiny::h3("Complexity analytics"),
      shiny::p(
        class = "cgm-dashboard-intro",
        "Complexity metrics describe regularity and long-range structure in glucose patterns after the current preprocessing choices are applied."
      )
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-complexity-controls-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Analysis controls")
      ),
      shiny::div(
        class = "cgm-dashboard-section-body",
        shiny::div(
          class = "cgm-complexity-controls-panel",
          shiny::div(
            class = "cgm-complexity-control-group",
            shiny::h4("Filters"),
            shiny::div(
              class = "cgm-complexity-control-grid",
              shiny::uiOutput(ns("subject_filter")),
              shiny::uiOutput(ns("group_filter"))
            )
          ),
          shiny::div(
            class = "cgm-complexity-control-group",
            shiny::h4("Core parameters"),
            shiny::div(
              class = "cgm-complexity-control-grid",
              shiny::numericInput(ns("min_points"), "Minimum usable points", value = 100, min = 20, step = 10),
              shiny::numericInput(ns("entropy_bin_width"), "Entropy bin width", value = 10, min = 1, step = 1),
              shiny::numericInput(ns("embedding_dimension"), "Embedding dimension", value = 2, min = 2, step = 1)
            )
          ),
          shiny::div(
            class = "cgm-complexity-control-group",
            shiny::h4("Curve parameters"),
            shiny::div(
              class = "cgm-complexity-control-grid",
              shiny::numericInput(ns("mse_scale_max"), "MSE max scale", value = 5, min = 1, step = 1),
              shiny::numericInput(ns("higuchi_kmax"), "Higuchi kmax", value = 8, min = 2, step = 1)
            )
          )
        )
      )
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-complexity-summary-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Complexity status")
      ),
      shiny::div(
        class = "cgm-dashboard-section-body",
        shiny::uiOutput(ns("complexity_progress")),
        shinycssloaders::withSpinner(shiny::uiOutput(ns("summary_cards")), type = 4),
        shiny::uiOutput(ns("mse_status_note"))
      )
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-complexity-visual-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Visual summary"),
        shiny::div(
          class = "cgm-filter-bar cgm-complexity-visual-filter-bar",
          shiny::selectInput(ns("visual_mode"), "Visual type", choices = complexity_visual_mode_choices(), selected = "metric_summary"),
          shiny::uiOutput(ns("metric_filter")),
          shiny::uiOutput(ns("curve_filter"))
        )
      ),
      shiny::div(
        class = "cgm-dashboard-section-body cgm-plot-panel",
        shiny::uiOutput(ns("complexity_plot_ui"))
      )
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-complexity-table-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Complexity metrics"),
        shiny::downloadButton(ns("download_complexity"), "Download CSV")
      ),
      shiny::div(
        class = "cgm-dashboard-section-body cgm-table-panel",
        shiny::uiOutput(ns("metrics_table_ui"))
      )
    )
  )
}

complexity_loading_ui <- function(message = "Complexity metrics are calculating.") {
  shiny::div(
    class = "cgm-complexity-loading",
    shiny::div(class = "cgm-complexity-spinner"),
    shiny::span(message)
  )
}

complexity_status_chip_ui <- function(label, status = "idle", text = "") {
  if (!nzchar(text %||% "") && identical(status %||% "idle", "idle")) {
    return(NULL)
  }
  shiny::div(
    class = paste("cgm-status-chip", paste0("cgm-status-chip-", status %||% "idle")),
    shiny::strong(label),
    shiny::span(text %||% "")
  )
}

complexity_status_chips_ui <- function(
  quick_status = "idle",
  hurst_status = "idle",
  curve_status = "idle",
  mse_status = "idle"
) {
  chips <- Filter(Negate(is.null), list(
    complexity_status_chip_ui("Summary", quick_status, complexity_status_text(quick_status)),
    complexity_status_chip_ui("Hurst", hurst_status, complexity_scalar_status_text(hurst_status)),
    complexity_status_chip_ui("DFA/Higuchi", curve_status, complexity_curve_status_text(curve_status)),
    complexity_status_chip_ui("MSE", mse_status, complexity_mse_status_text(mse_status))
  ))
  if (!length(chips)) {
    return(NULL)
  }
  shiny::div(class = "cgm-status-chip-row cgm-complexity-status-chips", shiny::tagList(chips))
}

complexity_subject_id_display_override <- function(subject = NULL) {
  if (specific_filter_selected(subject)) TRUE else NULL
}

complexity_make_store <- function(key, ids, selected = "") {
  list(
    key = key,
    ids = ids,
    entries = new.env(parent = emptyenv()),
    queue = subject_queue_new(ids, selected, subject_background_batch_size("complexity")),
    worker_tokens = integer()
  )
}

complexity_store_get <- function(store, id) {
  if (is.null(store$entries) || !exists(id, envir = store$entries, inherits = FALSE)) {
    return(NULL)
  }
  get(id, envir = store$entries, inherits = FALSE)
}

complexity_store_set <- function(store, id, entry) {
  assign(id, entry, envir = store$entries)
  invisible(entry)
}

complexity_store_ids <- function(store) {
  if (is.null(store$entries)) {
    return(character())
  }
  ls(store$entries, all.names = TRUE)
}

complexity_update_entry <- function(store, id, values) {
  existing <- complexity_store_get(store, id) %||% list(
    id = id,
    metrics = NULL,
    curves = empty_complexity_curve_rows(),
    quick_status = "idle",
    hurst_status = "idle",
    curve_status = "idle",
    mse_status = "idle"
  )
  entry <- utils::modifyList(existing, values)
  entry$id <- id
  complexity_store_set(store, id, entry)
  entry
}

complexity_entry_metrics <- function(entry) {
  if (is.null(entry) || !is.data.frame(entry$metrics) || !nrow(entry$metrics)) {
    return(data.frame())
  }
  entry$metrics
}

complexity_entry_curves <- function(entry) {
  if (is.null(entry) || !is.data.frame(entry$curves) || !nrow(entry$curves)) {
    return(empty_complexity_curve_rows())
  }
  entry$curves
}

complexity_aggregate_metrics <- function(store, ids = NULL) {
  ids <- ids %||% complexity_store_ids(store)
  rows <- lapply(ids, function(id) complexity_entry_metrics(complexity_store_get(store, id)))
  rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(rows)) {
    return(data.frame())
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

complexity_aggregate_curves <- function(store, ids = NULL) {
  ids <- ids %||% complexity_store_ids(store)
  rows <- lapply(ids, function(id) complexity_entry_curves(complexity_store_get(store, id)))
  rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(rows)) {
    return(empty_complexity_curve_rows())
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

complexity_compute_quick_batch <- function(data, ids, parameters) {
  batch_data <- data[as.character(data$id) %in% ids, , drop = FALSE]
  if (!nrow(batch_data)) {
    return(complexity_result_columns())
  }
  compute_complexity_quick_metrics(batch_data, parameters)
}

complexity_compute_advanced_batch <- function(data, ids, parameters) {
  batch_data <- data[as.character(data$id) %in% ids, , drop = FALSE]
  if (!nrow(batch_data)) {
    return(list(
      hurst = empty_complexity_hurst_rows(),
      dfa_higuchi_curves = empty_complexity_curve_rows(),
      mse_curves = empty_complexity_curve_rows()
    ))
  }
  list(
    hurst = compute_complexity_hurst_metrics(batch_data, parameters),
    dfa_higuchi_curves = compute_complexity_dfa_higuchi_curves(batch_data, parameters),
    mse_curves = compute_complexity_mse_curves(
      batch_data,
      min_points = parameters$min_points,
      entropy_bin_width = parameters$entropy_bin_width,
      embedding_dimension = parameters$embedding_dimension,
      mse_scale_max = parameters$mse_scale_max,
      higuchi_kmax = parameters$higuchi_kmax,
      max_gap_intervals = parameters$max_gap_intervals
    )
  )
}

complexity_progress_text <- function(store) {
  if (is.null(store) || is.null(store$queue)) {
    return("")
  }
  subject_queue_progress_text("Complexity", store$queue, store$ids)
}

complexity_stage_status <- function(store, selected, stage) {
  if (is.null(store)) {
    return("idle")
  }
  ids <- if (identical(selected, all_filter_value())) {
    store$ids
  } else {
    normalize_filter_value(selected)
  }
  ids <- ids[nzchar(ids)]
  if (!length(ids)) {
    return("idle")
  }
  values <- vapply(ids, function(id) {
    entry <- complexity_store_get(store, id)
    if (is.null(entry)) {
      return("running")
    }
    entry[[stage]] %||% "idle"
  }, character(1))
  if (any(values %in% "running")) {
    return("running")
  }
  if (all(values %in% "complete")) {
    return("complete")
  }
  if (all(values %in% "failed")) {
    return("failed")
  }
  if (any(values %in% "complete")) {
    return("running")
  }
  "idle"
}

complexity_module_server <- function(id, standardized, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    store_state <- new.env(parent = emptyenv())
    store_state$store <- NULL
    progress_version <- shiny::reactiveVal(0L)
    display_version <- shiny::reactiveVal(0L)

    bump_progress <- function() progress_version(progress_version() + 1L)
    bump_display <- function() display_version(display_version() + 1L)

    all_compute_data <- shiny::reactive({
      req_active_tab(active_tab, "complexity")
      standardized()
    })

    queue_data <- shiny::reactive({
      filter_complexity_data(all_compute_data(), group = input$group)
    })

    subject_choices_data <- queue_data

    complexity_subject_ids <- shiny::reactive({
      sort(subject_id_values(queue_data()))
    })

    selected_subject <- shiny::reactive({
      input$subject %||% default_subject_selection(complexity_subject_ids())
    })

    compute_data <- shiny::reactive({
      filter_complexity_data(
        all_compute_data(),
        subject = selected_subject(),
        group = input$group
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

    complexity_key <- shiny::reactive({
      paste(
        utils::capture.output(utils::str(list(
          data = cgm_data_signature(queue_data()),
          group = normalize_filter_value(input$group),
          parameters = parameters()
        ))),
        collapse = "\n"
      )
    })

    ensure_complexity_store <- function() {
      key <- complexity_key()
      ids <- complexity_subject_ids()
      if (is.null(store_state$store) || !identical(store_state$store$key, key)) {
        store_state$store <- complexity_make_store(key, ids, selected_subject())
        bump_progress()
        bump_display()
      }
      store_state$store
    }

    should_update_complexity_display <- function(ids) {
      selected <- selected_subject()
      identical(selected, all_filter_value()) || normalize_filter_value(selected) %in% ids
    }

    run_next_complexity_batch <- function()
      NULL

    run_complexity_advanced_batch <- function(data, ids, params, key) {
      worker_token <- configure_background_workers()
      store_state$store$worker_tokens <- unique(c(store_state$store$worker_tokens, worker_token))
      promise <- promises::future_promise({
        cgm_timed(
          "complexity_advanced_batch_background",
          complexity_compute_advanced_batch(data, ids, params),
          context = list(subjects = length(ids))
        )
      })
      promises::then(
        promise,
        onFulfilled = function(value) {
          if (is.null(store_state$store) || !identical(store_state$store$key, key)) {
            schedule_background_worker_cleanup(worker_token)
            return(NULL)
          }
          failed <- character()
          for (subject_id in ids) {
            entry <- complexity_store_get(store_state$store, subject_id)
            if (is.null(entry) || !is.data.frame(entry$metrics) || !nrow(entry$metrics)) {
              failed <- c(failed, subject_id)
              next
            }
            hurst_rows <- value$hurst[as.character(value$hurst$id) == subject_id, , drop = FALSE]
            dfa_rows <- value$dfa_higuchi_curves[as.character(value$dfa_higuchi_curves$id) == subject_id, , drop = FALSE]
            mse_rows <- value$mse_curves[as.character(value$mse_curves$id) == subject_id, , drop = FALSE]
            curves <- rbind(dfa_rows, mse_rows)
            row.names(curves) <- NULL
            metrics <- merge_complexity_hurst_results(entry$metrics, hurst_rows, status = "complete")
            complexity_update_entry(store_state$store, subject_id, list(
              metrics = metrics,
              curves = curves,
              hurst_status = "complete",
              curve_status = if (nrow(dfa_rows)) "complete" else "failed",
              mse_status = if (nrow(mse_rows)) "complete" else "failed"
            ))
          }
          store_state$store$queue <- subject_queue_mark_finished(store_state$store$queue, ids, failed = failed)
          bump_progress()
          if (should_update_complexity_display(ids)) {
            bump_display()
          }
          schedule_background_worker_cleanup(worker_token)
          run_next_complexity_batch()
          NULL
        },
        onRejected = function(error) {
          if (!is.null(store_state$store) && identical(store_state$store$key, key)) {
            for (subject_id in ids) {
              entry <- complexity_store_get(store_state$store, subject_id)
              metrics <- if (!is.null(entry) && is.data.frame(entry$metrics)) {
                merge_complexity_hurst_results(entry$metrics, NULL, status = "failed")
              } else {
                compute_complexity_pending_summary(data[as.character(data$id) == subject_id, , drop = FALSE], params, status = "failed")
              }
              complexity_update_entry(store_state$store, subject_id, list(
                metrics = metrics,
                curves = empty_complexity_curve_rows(),
                hurst_status = "failed",
                curve_status = "failed",
                mse_status = "failed",
                error = conditionMessage(error)
              ))
            }
            store_state$store$queue <- subject_queue_mark_finished(store_state$store$queue, ids, failed = ids)
            bump_progress()
            if (should_update_complexity_display(ids)) {
              bump_display()
            }
          }
          schedule_background_worker_cleanup(worker_token)
          run_next_complexity_batch()
          NULL
        }
      )
      NULL
    }

    run_next_complexity_batch <- function() {
      store <- ensure_complexity_store()
      if (isTRUE(store$queue$running)) {
        return(NULL)
      }
      ids <- subject_queue_next_batch(store$queue)
      if (!length(ids)) {
        return(NULL)
      }
      data <- queue_data()
      params <- parameters()
      key <- store$key
      store$queue <- subject_queue_mark_running(store$queue, TRUE)
      store_state$store <- store
      bump_progress()
      worker_token <- configure_background_workers()
      store_state$store$worker_tokens <- unique(c(store_state$store$worker_tokens, worker_token))
      promise <- promises::future_promise({
        cgm_timed(
          "complexity_quick_batch_background",
          complexity_compute_quick_batch(data, ids, params),
          context = list(subjects = length(ids))
        )
      })
      promises::then(
        promise,
        onFulfilled = function(value) {
          if (is.null(store_state$store) || !identical(store_state$store$key, key)) {
            schedule_background_worker_cleanup(worker_token)
            return(NULL)
          }
          if (!is.data.frame(value) || !nrow(value)) {
            store_state$store$queue <- subject_queue_mark_finished(store_state$store$queue, ids, failed = ids)
            bump_progress()
            schedule_background_worker_cleanup(worker_token)
            run_next_complexity_batch()
            return(NULL)
          }
          eligible_ids <- character()
          for (subject_id in ids) {
            quick_rows <- value[as.character(value$id) == subject_id, , drop = FALSE]
            if (!nrow(quick_rows)) {
              quick_rows <- compute_complexity_pending_summary(
                data[as.character(data$id) == subject_id, , drop = FALSE],
                params,
                status = "failed"
              )
            }
            eligible <- any(quick_rows$eligible %in% TRUE)
            if (eligible) {
              eligible_ids <- c(eligible_ids, subject_id)
            }
            complexity_update_entry(store_state$store, subject_id, list(
              metrics = merge_complexity_hurst_results(quick_rows, NULL, status = if (eligible) "running" else "idle"),
              curves = empty_complexity_curve_rows(),
              quick_status = "complete",
              hurst_status = if (eligible) "running" else "idle",
              curve_status = if (eligible) "running" else "idle",
              mse_status = if (eligible) "running" else "idle"
            ))
          }
          bump_progress()
          if (should_update_complexity_display(ids)) {
            bump_display()
          }
          schedule_background_worker_cleanup(worker_token)
          if (length(eligible_ids)) {
            run_complexity_advanced_batch(data, ids, params, key)
          } else {
            store_state$store$queue <- subject_queue_mark_finished(store_state$store$queue, ids)
            bump_progress()
            run_next_complexity_batch()
          }
          NULL
        },
        onRejected = function(error) {
          if (!is.null(store_state$store) && identical(store_state$store$key, key)) {
            for (subject_id in ids) {
              failed <- compute_complexity_pending_summary(
                data[as.character(data$id) == subject_id, , drop = FALSE],
                params,
                status = "failed"
              )
              complexity_update_entry(store_state$store, subject_id, list(
                metrics = failed,
                curves = empty_complexity_curve_rows(),
                quick_status = "failed",
                hurst_status = "failed",
                curve_status = "failed",
                mse_status = "failed",
                error = conditionMessage(error)
              ))
            }
            store_state$store$queue <- subject_queue_mark_finished(store_state$store$queue, ids, failed = ids)
            bump_progress()
            if (should_update_complexity_display(ids)) {
              bump_display()
            }
          }
          schedule_background_worker_cleanup(worker_token)
          run_next_complexity_batch()
          NULL
        }
      )
      NULL
    }

    shiny::observe({
      req_active_tab(active_tab, "complexity")
      ensure_complexity_store()
      run_next_complexity_batch()
      NULL
    })

    shiny::observeEvent(input$subject, {
      req_active_tab(active_tab, "complexity")
      store <- ensure_complexity_store()
      store$queue <- subject_queue_reprioritize(store$queue, input$subject)
      store_state$store <- store
      bump_progress()
      bump_display()
      run_next_complexity_batch()
      NULL
    }, ignoreInit = TRUE)

    output$subject_filter <- shiny::renderUI({
      req_active_tab(active_tab, "complexity")
      data <- subject_choices_data()
      if (!subject_id_filter_available(data)) {
        return(NULL)
      }
      ids <- sort(subject_id_values(data))
      choices <- subject_filter_choices(ids, all_label = "All")
      shiny::selectInput(
        session$ns("subject"),
        "Subject ID",
        choices = choices,
        selected = preserve_subject_filter_selection(
          input$subject,
          choices,
          ids
        )
      )
    })

    output$group_filter <- shiny::renderUI({
      req_active_tab(active_tab, "complexity")
      data <- all_compute_data()
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

    display_data <- shiny::reactive({
      req_active_tab(active_tab, "complexity")
      compute_data()
    })

    displayed_complexity_results <- shiny::reactive({
      display_version()
      store <- ensure_complexity_store()
      selected <- selected_subject()
      if (identical(selected, all_filter_value())) {
        return(complexity_aggregate_metrics(store))
      }
      complexity_entry_metrics(complexity_store_get(store, normalize_filter_value(selected)))
    })

    displayed_complexity_curves <- shiny::reactive({
      display_version()
      store <- ensure_complexity_store()
      selected <- selected_subject()
      if (identical(selected, all_filter_value())) {
        return(complexity_aggregate_curves(store))
      }
      complexity_entry_curves(complexity_store_get(store, normalize_filter_value(selected)))
    })

    quick_status <- shiny::reactive({
      display_version()
      complexity_stage_status(store_state$store, selected_subject(), "quick_status")
    })

    hurst_status <- shiny::reactive({
      display_version()
      complexity_stage_status(store_state$store, selected_subject(), "hurst_status")
    })

    curve_status <- shiny::reactive({
      display_version()
      complexity_stage_status(store_state$store, selected_subject(), "curve_status")
    })

    mse_status <- shiny::reactive({
      display_version()
      complexity_stage_status(store_state$store, selected_subject(), "mse_status")
    })

    force_subject_id_display <- shiny::reactive({
      complexity_subject_id_display_override(input$subject)
    })

    output$complexity_progress <- shiny::renderUI({
      req_active_tab(active_tab, "complexity")
      progress_version()
      text <- complexity_progress_text(store_state$store)
      if (!nzchar(text)) {
        return(NULL)
      }
      shiny::div(class = "cgm-compact-info-note", text)
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
      complexity_status_chips_ui(
        quick_status(),
        hurst_status(),
        curve_status(),
        mse_status()
      )
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
