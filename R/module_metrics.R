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

metrics_entry_replace_fields <- function(existing, values, id) {
  entry <- existing %||% list(id = id, status = "pending")
  for (name in names(values)) {
    entry[[name]] <- values[[name]]
  }
  entry$id <- id
  entry
}

metrics_make_store <- function(key, ids, selected = "") {
  list(
    key = key,
    ids = ids,
    entries = new.env(parent = emptyenv()),
    running = character()
  )
}

metrics_store_base_complete <- function(store) {
  if (is.null(store)) {
    return(FALSE)
  }
  setequal(metrics_store_cached_ids(store), clean_filter_values(store$ids))
}

metrics_store_queue_adapter_ids <- function(store, ids, selected = "") {
  store
}

metrics_store_cached_ids <- function(store) {
  ids <- clean_filter_values(metrics_store_ids(store))
  ids[vapply(ids, function(id) {
    entry <- metrics_store_get(store, id)
    identical(entry$status %||% "", "base_ready") || identical(entry$status %||% "", "no_analysis_rows")
  }, logical(1))]
}

metrics_progress_text <- function(store) {
  if (is.null(store)) {
    return("")
  }
  total <- length(clean_filter_values(store$ids))
  if (!total) {
    return("")
  }
  cached <- length(metrics_store_cached_ids(store))
  if (cached >= total) {
    return(sprintf("Metrics cached for all %s Subject IDs.", format(total, big.mark = ",")))
  }
  sprintf(
    "Metrics cached for %s of %s Subject IDs. Select a Subject ID to calculate and cache it.",
    format(cached, big.mark = ","),
    format(total, big.mark = ",")
  )
}

metrics_compute_base_batch <- function(data, ids, thresholds) {
  ids <- clean_filter_values(ids)
  batch_data <- data[as.character(data$id) %in% ids, , drop = FALSE]
  combined_base <- if (nrow(batch_data) && valid_metric_thresholds(thresholds)) {
    tryCatch(
      compute_base_core_metrics(batch_data, thresholds = thresholds, by = default_metric_groups(batch_data)),
      error = function(error) error
    )
  } else {
    data.frame()
  }
  rows <- lapply(ids, function(id) {
    subject_data <- batch_data[as.character(batch_data$id) == id, , drop = FALSE]
    if (!nrow(subject_data)) {
      return(list(id = id, base_state = compute_base_metric_state(subject_data, thresholds = thresholds)))
    }
    if (!valid_metric_thresholds(thresholds)) {
      return(list(id = id, base_state = metric_state("base_error", data = subject_data, error = "Invalid metric threshold settings.")))
    }
    if (inherits(combined_base, "error")) {
      return(list(id = id, base_state = metric_state("base_error", data = subject_data, error = conditionMessage(combined_base))))
    }
    subject_base <- if (is.data.frame(combined_base) && nrow(combined_base) && "id" %in% names(combined_base)) {
      combined_base[as.character(combined_base$id) == id, , drop = FALSE]
    } else {
      data.frame()
    }
    row.names(subject_base) <- NULL
    display <- prepare_metrics_display(subject_base, thresholds = thresholds)
    state <- if (nrow(subject_base) && nrow(display)) {
      metric_state("base_ready", data = subject_data, base = subject_base, display = display)
    } else {
      metric_state("no_analysis_rows", data = subject_data, base = subject_base, display = display)
    }
    list(id = id, base_state = state)
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

metrics_compute_subject <- function(data, id, thresholds) {
  id <- normalize_filter_value(id)
  if (!nzchar(id)) {
    return(list(id = id, status = "idle"))
  }
  subject_data <- data[as.character(data$id) == id, , drop = FALSE]
  base_state <- compute_base_metric_state(subject_data, thresholds = thresholds)
  adapters <- data.frame()
  adapter_status <- "idle"
  adapter_error <- NULL
  if (should_start_additional_metrics(base_state)) {
    adapters <- tryCatch(
      cgm_suppress_non_cgma_messages(metrics_compute_adapter_batch(subject_data, id)),
      error = function(error) {
        adapter_error <<- conditionMessage(error)
        data.frame()
      }
    )
    adapter_status <- if (is.data.frame(adapters) && nrow(adapters)) "complete" else "failed"
  }
  list(
    id = id,
    status = base_state$status,
    base_state = base_state,
    adapters = adapters,
    adapter_status = adapter_status,
    adapter_error = adapter_error,
    computed_at = Sys.time()
  )
}

metrics_aggregate_entries <- function(store, ids = NULL) {
  ids <- ids %||% metrics_store_cached_ids(store)
  rows <- lapply(ids, function(id) metrics_entry_raw(metrics_store_get(store, id)))
  bind_metric_rows(rows)
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
      cgm_cache_key(
        cgm_data_signature(all_metric_data()),
        threshold_signature(settings()$thresholds_mg_dl)
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

    update_metric_entry <- function(id, values) {
      store <- store_state$store
      existing <- metrics_store_get(store, id) %||% list(id = id, status = "pending")
      entry <- metrics_entry_replace_fields(existing, values, id)
      metrics_store_set(store, id, entry)
      invisible(entry)
    }

    ensure_metric_subject <- function(subject = selected_participant()) {
      subject <- normalize_filter_value(subject)
      if (!nzchar(subject)) {
        return(NULL)
      }
      store <- ensure_metrics_store()
      if (subject %in% store$running) {
        return(NULL)
      }
      entry <- metrics_store_get(store, subject)
      if (!is.null(entry) && (entry$status %||% "") %in% c("base_ready", "no_analysis_rows", "base_error") && (entry$adapter_status %||% "idle") %in% c("complete", "failed", "idle")) {
        return(entry)
      }
      store$running <- unique(c(store$running %||% character(), subject))
      store_state$store <- store
      bump_progress()
      update_metric_entry(subject, list(
        status = "running",
        base_state = metrics_calculating_state(
          filter_data_by_subject_selection(all_metric_data(), subject),
          "Metrics are calculating for the selected Subject ID."
        ),
        adapter_status = "pending"
      ))
      bump_display()
      key <- store$key
      result <- tryCatch(
        cgm_timed(
          "metrics_selected_subject_compute",
          metrics_compute_subject(all_metric_data(), subject, settings()$thresholds_mg_dl),
          context = list(subject = subject)
        ),
        error = function(error) list(
          id = subject,
          status = "base_error",
          base_state = metric_state("base_error", error = conditionMessage(error)),
          adapters = NULL,
          adapter_status = "failed",
          adapter_error = conditionMessage(error)
        )
      )
      if (!is.null(store_state$store) && identical(store_state$store$key, key)) {
        update_metric_entry(subject, result)
        store_state$store$running <- setdiff(store_state$store$running %||% character(), subject)
        bump_progress()
        bump_display()
      }
      NULL
    }

    shiny::observe({
      req_active_tab(active_tab, "metrics")
      ensure_metrics_store()
      ensure_metric_subject()
      NULL
    })

    shiny::observeEvent(input$participant, {
      req_active_tab(active_tab, "metrics")
      ensure_metric_subject(input$participant)
      bump_progress()
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
          return(metrics_calculating_state(all_metric_data(), "Select a Subject ID to calculate and cache Metrics. Cached Subject IDs will appear in All."))
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
        cached <- if (!is.null(store)) metrics_store_cached_ids(store) else character()
        statuses <- vapply(cached, function(subject_id) {
          (metrics_store_get(store, subject_id) %||% list())$adapter_status %||% "idle"
        }, character(1))
        if (any(statuses == "failed")) "failed" else "complete"
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
      display <- metrics_display_table_frame(filtered_display())
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
    }, server = TRUE)

    metrics <- shiny::reactive({
      display_version()
      store <- store_state$store
      if (is.null(store)) {
        if (is_active_tab(active_tab, "metrics")) {
          store <- ensure_metrics_store()
        } else {
          return(data.frame())
        }
      }
      selected <- if (is_active_tab(active_tab, "metrics")) selected_participant() else all_filter_value()
      if (identical(selected, all_filter_value())) {
        return(metrics_aggregate_entries(store))
      }
      metrics_entry_raw(metrics_store_get(store, selected))
    })

    metrics
  })
}
