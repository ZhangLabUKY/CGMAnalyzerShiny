metrics_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "cgm-metrics-dashboard",
    shiny::div(
      class = "cgm-metrics-overview",
      shiny::h3("CGM metrics"),
      shinycssloaders::withSpinner(shiny::uiOutput(ns("summary_cards")), type = 4),
      shiny::uiOutput(ns("optional_metric_note"))
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-metrics-detail-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Detailed metrics"),
        shiny::div(
          class = "cgm-filter-bar cgm-metrics-filter-bar",
          shiny::uiOutput(ns("participant_filter")),
          shiny::uiOutput(ns("group_filter")),
          shiny::uiOutput(ns("period_filter")),
          shiny::uiOutput(ns("category_filter"))
        )
      ),
      shiny::div(
        class = "cgm-dashboard-section-body",
        shiny::uiOutput(ns("metrics_progress")),
        shiny::uiOutput(ns("metrics_empty_state")),
        shinycssloaders::withSpinner(DT::DTOutput(ns("metrics_table")), type = 4)
      )
    )
  )
}

metrics_calculating_state <- function(data = NULL, message = "Metrics are calculating for the selected Subject ID.") {
  metric_state(
    "calculating",
    data = data,
    base = data.frame(),
    display = empty_metrics_display(),
    message = message
  )
}

metrics_entry_raw <- function(entry) {
  if (is.null(entry) || is.null(entry$base_state) || !identical(entry$base_state$status, "base_ready")) {
    return(data.frame())
  }
  base <- entry$base_state$base
  adapters <- entry$adapters
  if (is.data.frame(adapters) && nrow(adapters)) {
    return(merge_core_metric_outputs(base, adapters, by = default_metric_groups(base)))
  }
  base
}

metrics_store_ids <- function(store) {
  if (is.null(store$entries)) {
    return(character())
  }
  ls(store$entries, all.names = TRUE)
}

metrics_store_get <- function(store, id) {
  if (is.null(store$entries) || !exists(id, envir = store$entries, inherits = FALSE)) {
    return(NULL)
  }
  get(id, envir = store$entries, inherits = FALSE)
}

metrics_store_set <- function(store, id, entry) {
  assign(id, entry, envir = store$entries)
  invisible(entry)
}

metrics_make_store <- function(key, ids, selected = "") {
  list(
    key = key,
    ids = ids,
    entries = new.env(parent = emptyenv()),
    queue = subject_queue_new(ids, selected, subject_background_batch_size("metrics")),
    adapter_running = character(),
    worker_tokens = integer()
  )
}

metrics_progress_text <- function(store) {
  if (is.null(store) || is.null(store$queue)) {
    return("")
  }
  subject_queue_progress_text("Metrics", store$queue, store$ids)
}

metrics_compute_base_batch <- function(data, ids, thresholds) {
  rows <- lapply(ids, function(id) {
    subject_data <- data[as.character(data$id) == id, , drop = FALSE]
    list(id = id, base_state = compute_base_metric_state(subject_data, thresholds = thresholds))
  })
  stats::setNames(rows, ids)
}

metrics_compute_adapter_batch <- function(data, ids) {
  batch_data <- data[as.character(data$id) %in% ids, , drop = FALSE]
  if (!nrow(batch_data)) {
    return(data.frame())
  }
  compute_metric_adapters(batch_data, by = default_metric_groups(batch_data))
}

metrics_aggregate_entries <- function(store, ids = NULL) {
  ids <- ids %||% metrics_store_ids(store)
  rows <- lapply(ids, function(id) metrics_entry_raw(metrics_store_get(store, id)))
  rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(rows)) {
    return(data.frame())
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

metrics_module_server <- function(id, standardized, settings, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    store_state <- new.env(parent = emptyenv())
    store_state$store <- NULL
    store_state$running_key <- NULL
    progress_version <- shiny::reactiveVal(0L)
    display_version <- shiny::reactiveVal(0L)

    bump_progress <- function() progress_version(progress_version() + 1L)
    bump_display <- function() display_version(display_version() + 1L)

    all_metric_data <- shiny::reactive({
      req_active_tab(active_tab, "metrics")
      standardized()
    })

    metric_subject_ids <- shiny::reactive({
      sort(subject_id_values(all_metric_data()))
    })

    selected_participant <- shiny::reactive({
      input$participant %||% default_subject_selection(metric_subject_ids())
    })

    metrics_key <- shiny::reactive({
      paste(
        utils::capture.output(utils::str(list(
          data = cgm_data_signature(all_metric_data()),
          thresholds = threshold_signature(settings()$thresholds_mg_dl)
        ))),
        collapse = "\n"
      )
    })

    ensure_metrics_store <- function() {
      key <- metrics_key()
      ids <- metric_subject_ids()
      if (is.null(store_state$store) || !identical(store_state$store$key, key)) {
        store_state$store <- metrics_make_store(key, ids, selected_participant())
        bump_progress()
        bump_display()
      }
      store_state$store
    }

    selected_entry_status <- function() {
      selected <- selected_participant()
      store <- store_state$store
      if (identical(selected, all_filter_value())) {
        return("all")
      }
      entry <- metrics_store_get(store, selected)
      entry$status %||% "pending"
    }

    should_update_display_for_ids <- function(ids) {
      selected <- selected_participant()
      identical(selected, all_filter_value()) || selected %in% ids
    }

    update_metric_entry <- function(id, values) {
      store <- store_state$store
      existing <- metrics_store_get(store, id) %||% list(id = id, status = "pending")
      entry <- utils::modifyList(existing, values)
      entry$id <- id
      metrics_store_set(store, id, entry)
      invisible(entry)
    }

    run_metric_adapter_batch <- function(data, ids, key) {
      ids <- clean_filter_values(ids)
      if (!length(ids)) {
        return(NULL)
      }
      store_state$store$adapter_running <- unique(c(store_state$store$adapter_running, ids))
      bump_progress()
      worker_token <- configure_background_workers()
      store_state$store$worker_tokens <- unique(c(store_state$store$worker_tokens, worker_token))
      promise <- promises::future_promise({
        cgm_timed(
          "metrics_adapter_background",
          metrics_compute_adapter_batch(data, ids),
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
          for (subject_id in ids) {
            adapter_rows <- if (is.data.frame(value) && nrow(value) && "id" %in% names(value)) {
              value[as.character(value$id) == subject_id, , drop = FALSE]
            } else {
              data.frame()
            }
            update_metric_entry(subject_id, list(
              adapters = adapter_rows,
              adapter_status = if (nrow(adapter_rows)) "complete" else "failed"
            ))
          }
          store_state$store$adapter_running <- setdiff(store_state$store$adapter_running, ids)
          bump_progress()
          if (should_update_display_for_ids(ids)) {
            bump_display()
          }
          schedule_background_worker_cleanup(worker_token)
          NULL
        },
        onRejected = function(error) {
          if (!is.null(store_state$store) && identical(store_state$store$key, key)) {
            for (subject_id in ids) {
              update_metric_entry(subject_id, list(
                adapters = NULL,
                adapter_status = "failed",
                adapter_error = conditionMessage(error)
              ))
            }
            store_state$store$adapter_running <- setdiff(store_state$store$adapter_running, ids)
            bump_progress()
            if (should_update_display_for_ids(ids)) {
              bump_display()
            }
          }
          schedule_background_worker_cleanup(worker_token)
          NULL
        }
      )
      NULL
    }

    run_next_metric_batch <- function() {
      store <- ensure_metrics_store()
      if (isTRUE(store$queue$running)) {
        return(NULL)
      }
      ids <- subject_queue_next_batch(store$queue)
      if (!length(ids)) {
        return(NULL)
      }
      data <- all_metric_data()
      thresholds <- settings()$thresholds_mg_dl
      key <- store$key
      store$queue <- subject_queue_mark_running(store$queue, TRUE)
      store_state$store <- store
      bump_progress()
      worker_token <- configure_background_workers()
      store_state$store$worker_tokens <- unique(c(store_state$store$worker_tokens, worker_token))
      promise <- promises::future_promise({
        cgm_timed(
          "metrics_base_batch_background",
          metrics_compute_base_batch(data, ids, thresholds),
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
          adapter_ids <- character()
          for (subject_id in names(value)) {
            base_state <- value[[subject_id]]$base_state
            if (!identical(base_state$status, "base_ready")) {
              failed <- c(failed, subject_id)
            } else if (should_start_additional_metrics(base_state)) {
              adapter_ids <- c(adapter_ids, subject_id)
            }
            update_metric_entry(subject_id, list(
              status = base_state$status,
              base_state = base_state,
              adapter_status = if (subject_id %in% adapter_ids) "running" else "idle"
            ))
          }
          store_state$store$queue <- subject_queue_mark_finished(store_state$store$queue, ids, failed = failed)
          bump_progress()
          if (should_update_display_for_ids(ids)) {
            bump_display()
          }
          if (length(adapter_ids)) {
            run_metric_adapter_batch(data, adapter_ids, key)
          }
          schedule_background_worker_cleanup(worker_token)
          run_next_metric_batch()
          NULL
        },
        onRejected = function(error) {
          if (!is.null(store_state$store) && identical(store_state$store$key, key)) {
            for (subject_id in ids) {
              update_metric_entry(subject_id, list(
                status = "base_error",
                base_state = metric_state("base_error", error = conditionMessage(error)),
                adapter_status = "failed"
              ))
            }
            store_state$store$queue <- subject_queue_mark_finished(store_state$store$queue, ids, failed = ids)
            bump_progress()
            if (should_update_display_for_ids(ids)) {
              bump_display()
            }
          }
          schedule_background_worker_cleanup(worker_token)
          run_next_metric_batch()
          NULL
        }
      )
      NULL
    }

    shiny::observe({
      req_active_tab(active_tab, "metrics")
      ensure_metrics_store()
      run_next_metric_batch()
      NULL
    })

    shiny::observeEvent(input$participant, {
      req_active_tab(active_tab, "metrics")
      store <- ensure_metrics_store()
      store$queue <- subject_queue_reprioritize(store$queue, input$participant)
      store_state$store <- store
      bump_progress()
      run_next_metric_batch()
      bump_display()
      NULL
    }, ignoreInit = TRUE)

    display_metric_state <- shiny::reactive({
      req_active_tab(active_tab, "metrics")
      display_version()
      if (!nrow(all_metric_data())) {
        return(compute_base_metric_state(all_metric_data(), thresholds = settings()$thresholds_mg_dl))
      }
      store <- ensure_metrics_store()
      selected <- selected_participant()
      if (identical(selected, all_filter_value())) {
        raw_metrics <- metrics_aggregate_entries(store)
        if (!nrow(raw_metrics)) {
          return(metrics_calculating_state(all_metric_data(), "Metrics are calculating. Completed Subject IDs will appear here as batches finish."))
        }
      } else {
        entry <- metrics_store_get(store, selected)
        if (is.null(entry)) {
          return(metrics_calculating_state(
            filter_data_by_subject_selection(all_metric_data(), selected),
            "Metrics are calculating for the selected Subject ID."
          ))
        }
        if (!identical(entry$base_state$status, "base_ready")) {
          return(entry$base_state)
        }
        raw_metrics <- metrics_entry_raw(entry)
      }

      tryCatch({
        display <- cgm_timed(
          "metrics_display_prepare",
          prepare_metrics_display(raw_metrics, thresholds = settings()$thresholds_mg_dl)
        )
        if (!nrow(display)) {
          return(metric_state("no_analysis_rows", data = all_metric_data(), base = raw_metrics, display = display))
        }
        metric_state("base_ready", data = all_metric_data(), base = raw_metrics, display = display)
      }, error = function(error) {
        metric_state("base_error", data = all_metric_data(), base = raw_metrics, error = conditionMessage(error))
      })
    })

    display_metrics <- shiny::reactive({
      display_metric_state()$display
    })

    output$participant_filter <- shiny::renderUI({
      data <- tryCatch(
        standardized(),
        shiny.silent.error = function(error) NULL,
        error = function(error) NULL
      )
      if (!subject_id_filter_available(data)) {
        return(NULL)
      }
      ids <- sort(subject_id_values(data))
      choices <- subject_filter_choices(ids, all_label = "All")
      shiny::selectInput(
        session$ns("participant"),
        "Subject ID",
        choices = choices,
        selected = preserve_subject_filter_selection(
          input$participant,
          choices,
          ids
        )
      )
    })

    output$metrics_progress <- shiny::renderUI({
      req_active_tab(active_tab, "metrics")
      progress_version()
      text <- metrics_progress_text(store_state$store)
      if (!nzchar(text)) {
        return(NULL)
      }
      shiny::div(class = "cgm-compact-info-note", text)
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

    output$period_filter <- shiny::renderUI({
      display <- display_metrics()
      if (!"Period" %in% names(display)) {
        return(NULL)
      }
      choices <- metric_period_filter_choices(display)
      shiny::selectInput(
        session$ns("period"),
        "Period",
        choices = choices,
        selected = preserve_filter_selection(input$period %||% default_time_window(), choices)
      )
    })

    output$optional_metric_note <- shiny::renderUI({
      state <- display_metric_state()
      if (!identical(state$status, "base_ready")) {
        return(NULL)
      }
      selected <- selected_participant()
      adapter_status <- if (identical(selected, all_filter_value())) {
        store <- store_state$store
        if (!is.null(store) && length(store$adapter_running)) "running" else "complete"
      } else {
        (metrics_store_get(store_state$store, selected) %||% list())$adapter_status %||% "idle"
      }
      note <- optional_metric_note_text(adapter_status)
      if (!nzchar(note)) {
        return(NULL)
      }
      shiny::div(
        class = "cgm-compact-info-note cgm-metrics-optional-note",
        shiny::span(note)
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

    card_display <- shiny::reactive({
      filter_metrics_display(
        display_metrics(),
        participant = input$participant %||% "",
        group = input$group %||% "",
        period = input$period %||% default_time_window(),
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
      summary_card_ui(metric_summary_cards(display, settings()$thresholds_mg_dl))
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
      display <- filtered_display()
      cgm_timed(
        "metrics_table_dt_render",
        DT::datatable(
          display,
          rownames = FALSE,
          extensions = "RowGroup",
          options = metrics_table_options(display)
        ),
        rows = nrow(display)
      )
    })

    metrics <- shiny::reactive({
      if (is_active_tab(active_tab, "metrics")) {
        display_version()
        selected <- selected_participant()
        store <- ensure_metrics_store()
        if (identical(selected, all_filter_value())) {
          return(metrics_aggregate_entries(store))
        }
        return(metrics_entry_raw(metrics_store_get(store, selected)))
      }
      data <- standardized()
      cgm_timed(
        "metrics_sync_all",
        compute_core_metrics(data, thresholds = settings()$thresholds_mg_dl)
      )
    })

    metrics
  })
}
