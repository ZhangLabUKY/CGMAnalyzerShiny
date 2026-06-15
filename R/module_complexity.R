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
  store <- new.env(parent = emptyenv())
  store$key <- key
  store$ids <- complexity_subject_keys(ids)
  store$entries <- new.env(parent = emptyenv())
  store$quick_running <- character()
  store$advanced_running <- character()
  store
}

complexity_store_get <- function(store, id) {
  id <- complexity_subject_key(id)
  if (is.null(store$entries) || !exists(id, envir = store$entries, inherits = FALSE)) {
    return(NULL)
  }
  get(id, envir = store$entries, inherits = FALSE)
}

complexity_store_set <- function(store, id, entry) {
  id <- complexity_subject_key(id)
  assign(id, entry, envir = store$entries)
  invisible(entry)
}

complexity_replace_entry_fields <- function(existing, values, id) {
  entry <- existing
  if (!is.list(entry)) {
    entry <- list()
  }
  for (name in names(values)) {
    entry[[name]] <- values[[name]]
  }
  entry$id <- id
  entry
}

complexity_store_ids <- function(store) {
  if (is.null(store$entries)) {
    return(character())
  }
  ls(store$entries, all.names = TRUE)
}

complexity_update_entry <- function(store, id, values) {
  id <- complexity_subject_key(id)
  existing <- complexity_store_get(store, id) %||% list(
    id = id,
    metrics = NULL,
    curves = empty_complexity_curve_rows(),
    quick_status = "idle",
    hurst_status = "idle",
    curve_status = "idle",
    mse_status = "idle"
  )
  entry <- complexity_replace_entry_fields(existing, values, id)
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
  bind_complexity_rows(rows)
}

complexity_aggregate_curves <- function(store, ids = NULL) {
  ids <- ids %||% complexity_store_ids(store)
  rows <- lapply(ids, function(id) complexity_entry_curves(complexity_store_get(store, id)))
  bind_complexity_rows(rows, empty = empty_complexity_curve_rows())
}

complexity_compute_quick_batch <- function(data, ids, parameters) {
  ids <- complexity_subject_keys(ids)
  batch_data <- data[as.character(data$id) %in% ids, , drop = FALSE]
  if (!nrow(batch_data)) {
    return(complexity_result_columns())
  }
  compute_complexity_quick_metrics(batch_data, parameters)
}

complexity_advanced_stage <- function(label, expr, empty, context = NULL) {
  tryCatch(
    list(
      data = cgm_timed(label, expr, context = context),
      status = "complete",
      error = ""
    ),
    error = function(error) {
      list(
        data = empty,
        status = "failed",
        error = conditionMessage(error)
      )
    }
  )
}

complexity_stage_data <- function(result, empty) {
  if (is.list(result) && is.data.frame(result$data)) {
    return(result$data)
  }
  if (is.data.frame(result)) {
    return(result)
  }
  empty
}

complexity_stage_status_value <- function(result) {
  status <- if (is.list(result)) result$status %||% "" else ""
  if (identical(status, "failed")) "failed" else "complete"
}

complexity_stage_error_value <- function(result) {
  error <- if (is.list(result)) result$error %||% "" else ""
  as.character(error %||% "")
}

complexity_subject_key <- function(value) {
  value <- value %||% ""
  if (!length(value)) {
    return("")
  }
  value <- as.character(value)
  value <- trimws(value[[1L]] %||% "")
  if (identical(value, all_filter_value())) "" else value
}

complexity_subject_keys <- function(values) {
  values <- as.character(values)
  values <- trimws(values)
  values[is.na(values)] <- ""
  values
}

complexity_make_version_bumper <- function(set_version, initial = 0L) {
  counter <- as.integer(initial %||% 0L)
  force(set_version)
  function() {
    counter <<- counter + 1L
    set_version(counter)
    invisible(counter)
  }
}

complexity_should_update_display_for <- function(display_subject, ids) {
  selected <- display_subject %||% ""
  selected <- if (length(selected)) selected[[1L]] else ""
  if (identical(selected, all_filter_value())) {
    return(TRUE)
  }
  selected <- complexity_subject_key(selected)
  nzchar(selected) && selected %in% complexity_subject_keys(ids)
}

complexity_subject_stage_rows <- function(rows, subject_id, empty) {
  if (!is.data.frame(rows) || !"id" %in% names(rows)) {
    return(empty)
  }
  rows[complexity_subject_keys(rows$id) == complexity_subject_key(subject_id), , drop = FALSE]
}

complexity_advanced_entry_values <- function(entry, value, subject_id) {
  subject_id <- complexity_subject_key(subject_id)
  if (is.null(entry) || !is.data.frame(entry$metrics) || !nrow(entry$metrics)) {
    stop("Quick Complexity results were not available for the selected Subject ID.", call. = FALSE)
  }
  if (!is.list(value)) {
    value <- list()
  }
  hurst_status <- complexity_stage_status_value(value$hurst)
  curve_status <- complexity_stage_status_value(value$dfa_higuchi_curves)
  mse_status <- complexity_stage_status_value(value$mse_curves)
  hurst_rows <- complexity_subject_stage_rows(
    complexity_stage_data(value$hurst, empty_complexity_hurst_rows()),
    subject_id,
    empty_complexity_hurst_rows()
  )
  dfa_rows <- complexity_subject_stage_rows(
    complexity_stage_data(value$dfa_higuchi_curves, empty_complexity_curve_rows()),
    subject_id,
    empty_complexity_curve_rows()
  )
  mse_rows <- complexity_subject_stage_rows(
    complexity_stage_data(value$mse_curves, empty_complexity_curve_rows()),
    subject_id,
    empty_complexity_curve_rows()
  )
  curves <- bind_complexity_rows(list(dfa_rows, mse_rows), empty = empty_complexity_curve_rows())
  metrics <- merge_complexity_hurst_results(entry$metrics, hurst_rows, status = hurst_status)
  stage_errors <- c(
    complexity_stage_error_value(value$hurst),
    complexity_stage_error_value(value$dfa_higuchi_curves),
    complexity_stage_error_value(value$mse_curves)
  )
  stage_errors <- stage_errors[nzchar(stage_errors)]
  list(
    metrics = metrics,
    curves = curves,
    hurst_status = hurst_status,
    curve_status = curve_status,
    mse_status = mse_status,
    error = if (length(stage_errors)) paste(unique(stage_errors), collapse = " | ") else NULL
  )
}

complexity_mark_advanced_failed <- function(
    store,
    data,
    params,
    subject_id,
    key,
    error,
    display_subject,
    bump_progress = function() NULL,
    bump_display = function() NULL,
    session_closed = function() FALSE) {
  subject_id <- complexity_subject_key(subject_id)
  if (isTRUE(session_closed()) || is.null(store) || !identical(store$key, key)) {
    return(FALSE)
  }
  error_message <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error %||% "")
  entry <- complexity_store_get(store, subject_id)
  metrics <- if (!is.null(entry) && is.data.frame(entry$metrics)) {
    merge_complexity_hurst_results(entry$metrics, NULL, status = "failed")
  } else {
    compute_complexity_pending_summary(data, params, status = "failed")
  }
  complexity_update_entry(store, subject_id, list(
    metrics = metrics,
    curves = empty_complexity_curve_rows(),
    hurst_status = "failed",
    curve_status = "failed",
    mse_status = "failed",
    error = error_message
  ))
  store$advanced_running <- setdiff(complexity_subject_keys(store$advanced_running %||% character()), subject_id)
  bump_progress()
  if (complexity_should_update_display_for(display_subject, subject_id)) {
    bump_display()
  }
  TRUE
}

complexity_apply_advanced_update <- function(
    store,
    subject_id,
    key,
    value,
    display_subject,
    bump_progress = function() NULL,
    bump_display = function() NULL,
    session_closed = function() FALSE) {
  subject_id <- complexity_subject_key(subject_id)
  if (isTRUE(session_closed()) || is.null(store) || !identical(store$key, key)) {
    return(FALSE)
  }
  entry <- complexity_store_get(store, subject_id)
  update_values <- complexity_advanced_entry_values(entry, value, subject_id)
  complexity_update_entry(store, subject_id, update_values)
  store$advanced_running <- setdiff(complexity_subject_keys(store$advanced_running %||% character()), subject_id)
  bump_progress()
  if (complexity_should_update_display_for(display_subject, subject_id)) {
    bump_display()
  }
  TRUE
}

complexity_compute_advanced_batch <- function(data, ids, parameters) {
  ids <- complexity_subject_keys(ids)
  batch_data <- data[as.character(data$id) %in% ids, , drop = FALSE]
  if (!nrow(batch_data)) {
    return(list(
      hurst = list(data = empty_complexity_hurst_rows(), status = "complete", error = ""),
      dfa_higuchi_curves = list(data = empty_complexity_curve_rows(), status = "complete", error = ""),
      mse_curves = list(data = empty_complexity_curve_rows(), status = "complete", error = "")
    ))
  }
  list(
    hurst = complexity_advanced_stage(
      "complexity_selected_hurst",
      compute_complexity_hurst_metrics(batch_data, parameters),
      empty_complexity_hurst_rows(),
      context = list(subjects = length(ids))
    ),
    dfa_higuchi_curves = complexity_advanced_stage(
      "complexity_selected_dfa_higuchi",
      compute_complexity_dfa_higuchi_curves(batch_data, parameters),
      empty_complexity_curve_rows(),
      context = list(subjects = length(ids))
    ),
    mse_curves = complexity_advanced_stage(
      "complexity_selected_mse",
      compute_complexity_mse_curves(
        batch_data,
        min_points = parameters$min_points,
        entropy_bin_width = parameters$entropy_bin_width,
        embedding_dimension = parameters$embedding_dimension,
        mse_scale_max = parameters$mse_scale_max,
        higuchi_kmax = parameters$higuchi_kmax,
        max_gap_intervals = parameters$max_gap_intervals
      ),
      empty_complexity_curve_rows(),
      context = list(subjects = length(ids))
    )
  )
}

complexity_store_cached_ids <- function(store) {
  ids <- clean_filter_values(complexity_store_ids(store))
  ids[vapply(ids, function(id) {
    entry <- complexity_store_get(store, id)
    identical(entry$quick_status %||% "", "complete") || identical(entry$quick_status %||% "", "failed")
  }, logical(1))]
}

complexity_progress_text <- function(store) {
  if (is.null(store)) {
    return("")
  }
  total <- length(clean_filter_values(store$ids))
  if (!total) {
    return("")
  }
  cached <- length(complexity_store_cached_ids(store))
  if (cached >= total) {
    return(sprintf("Complexity cached for all %s Subject IDs.", format(total, big.mark = ",")))
  }
  sprintf(
    "Complexity cached for %s of %s Subject IDs. Select a Subject ID to calculate and cache it.",
    format(cached, big.mark = ","),
    format(total, big.mark = ",")
  )
}

complexity_stage_status <- function(store, selected, stage) {
  if (is.null(store)) {
    return("idle")
  }
  ids <- if (identical(selected, all_filter_value())) complexity_store_cached_ids(store) else complexity_subject_key(selected)
  ids <- ids[nzchar(ids)]
  if (!length(ids)) {
    return("idle")
  }
  values <- vapply(ids, function(id) {
    entry <- complexity_store_get(store, id)
    if (is.null(entry)) {
      return("idle")
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
    store_state$progress_counter <- 0L
    store_state$display_counter <- 0L
    progress_version <- shiny::reactiveVal(0L)
    display_version <- shiny::reactiveVal(0L)

    bump_progress <- complexity_make_version_bumper(function(value) {
      store_state$progress_counter <- value
      progress_version(value)
    }, store_state$progress_counter)
    bump_display <- complexity_make_version_bumper(function(value) {
      store_state$display_counter <- value
      display_version(value)
    }, store_state$display_counter)

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
      cgm_cache_key(
        cgm_data_signature(queue_data()),
        list(group = normalize_filter_value(input$group)),
        parameters()
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

    run_complexity_advanced_subject <- function(data, subject_id, params, key, display_subject) {
      subject_id <- complexity_subject_key(subject_id)
      display_subject <- if (identical(display_subject, all_filter_value())) all_filter_value() else complexity_subject_key(display_subject)
      if (!nzchar(subject_id) || subject_id %in% complexity_subject_keys(store_state$store$advanced_running %||% character())) {
        return(NULL)
      }
      store_state$store$advanced_running <- unique(c(complexity_subject_keys(store_state$store$advanced_running %||% character()), subject_id))
      mark_advanced_failed <- function(error) {
        complexity_mark_advanced_failed(
          store_state$store,
          data,
          params,
          subject_id,
          key,
          error,
          display_subject,
          bump_progress = bump_progress,
          bump_display = bump_display,
          session_closed = session$isClosed
        )
        NULL
      }
      later::later(function() {
        if (isTRUE(session$isClosed()) || is.null(store_state$store) || !identical(store_state$store$key, key)) {
          return(NULL)
        }
        value <- tryCatch(
          cgm_with_progress(
            "Calculating advanced Complexity",
            detail = "Running Hurst, DFA/Higuchi, and MSE stages...",
            value = 0.2,
            session = session,
            cgm_timed(
              "complexity_selected_advanced_deferred",
              cgm_suppress_non_cgma_messages(complexity_compute_advanced_batch(data, subject_id, params)),
              context = list(subject = subject_id)
            )
          ),
          error = function(error) {
            mark_advanced_failed(error)
            NULL
          }
        )
        if (is.null(value)) {
          return(NULL)
        }
        tryCatch({
          if (isTRUE(session$isClosed()) || is.null(store_state$store) || !identical(store_state$store$key, key)) {
            return(NULL)
          }
          if (!is.list(value)) {
            value <- list()
          }
          complexity_apply_advanced_update(
            store_state$store,
            subject_id,
            key,
            value,
            display_subject,
            bump_progress = bump_progress,
            bump_display = bump_display,
            session_closed = session$isClosed
          )
          NULL
        }, error = function(error) {
          mark_advanced_failed(error)
        })
      }, delay = 0)
      NULL
    }

    ensure_complexity_subject <- function(subject = selected_subject()) {
      subject <- complexity_subject_key(subject)
      if (!nzchar(subject)) {
        return(NULL)
      }
      store <- ensure_complexity_store()
      entry <- complexity_store_get(store, subject)
      if (!is.null(entry) && (entry$quick_status %||% "") %in% c("complete", "failed")) {
        return(entry)
      }
      if (subject %in% complexity_subject_keys(store$quick_running %||% character())) {
        return(NULL)
      }
      data <- filter_complexity_data(queue_data(), subject = subject)
      params <- parameters()
      key <- store$key
      store$quick_running <- unique(c(complexity_subject_keys(store$quick_running %||% character()), subject))
      store_state$store <- store
      bump_progress()
      value <- tryCatch(
        cgm_with_progress(
          "Calculating Complexity",
          detail = "Preparing quick summary for the selected Subject ID...",
          value = 0.2,
          cgm_timed(
            "complexity_selected_quick_compute",
            complexity_compute_quick_batch(data, subject, params),
            context = list(subject = subject)
          )
        ),
        error = function(error) {
          failed <- compute_complexity_pending_summary(data, params, status = "failed")
          attr(failed, "error_message") <- conditionMessage(error)
          failed
        }
      )
      if (!is.null(store_state$store) && identical(store_state$store$key, key)) {
        quick_rows <- if (is.data.frame(value) && nrow(value)) {
          value[complexity_subject_keys(value$id) == subject, , drop = FALSE]
        } else {
          compute_complexity_pending_summary(data, params, status = "failed")
        }
        failed_quick <- !is.null(attr(value, "error_message", exact = TRUE))
        eligible <- any(quick_rows$eligible %in% TRUE) && !failed_quick
        complexity_update_entry(store_state$store, subject, list(
          metrics = merge_complexity_hurst_results(quick_rows, NULL, status = if (eligible) "running" else if (failed_quick) "failed" else "idle"),
          curves = empty_complexity_curve_rows(),
          quick_status = if (failed_quick) "failed" else "complete",
          hurst_status = if (eligible) "running" else if (failed_quick) "failed" else "idle",
          curve_status = if (eligible) "running" else if (failed_quick) "failed" else "idle",
          mse_status = if (eligible) "running" else if (failed_quick) "failed" else "idle",
          error = attr(value, "error_message", exact = TRUE) %||% NULL
        ))
        store_state$store$quick_running <- setdiff(complexity_subject_keys(store_state$store$quick_running %||% character()), subject)
        bump_progress()
        bump_display()
        if (eligible) {
          run_complexity_advanced_subject(data, subject, params, key, selected_subject())
        }
      }
      NULL
    }

    shiny::observe({
      req_active_tab(active_tab, "complexity")
      ensure_complexity_store()
      ensure_complexity_subject()
      NULL
    })

    shiny::observeEvent(input$subject, {
      req_active_tab(active_tab, "complexity")
      ensure_complexity_subject(input$subject)
      bump_progress()
      bump_display()
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
      complexity_entry_metrics(complexity_store_get(store, complexity_subject_key(selected)))
    })

    displayed_complexity_curves <- shiny::reactive({
      display_version()
      store <- ensure_complexity_store()
      selected <- selected_subject()
      if (identical(selected, all_filter_value())) {
        return(complexity_aggregate_curves(store))
      }
      complexity_entry_curves(complexity_store_get(store, complexity_subject_key(selected)))
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
          curve_data <- scale_curve_plot_data()
          if (!is.data.frame(curve_data) || !nrow(curve_data)) {
            if (identical(selected_curve, "mse")) {
              empty_plot("MSE finished, but no finite MSE scale-curve values were produced for the selected data.")
            } else if (identical(selected_curve, "dfa") || identical(selected_curve, "higuchi")) {
              empty_plot("DFA/Higuchi finished, but no finite scale-curve values were produced for the selected data.")
            } else {
              empty_plot("Advanced Complexity finished, but no finite scale-curve values were produced for the selected data.")
            }
          } else {
            create_complexity_scale_curve_plot(curve_data)
          }
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
    }, server = TRUE)

    output$download_complexity <- shiny::downloadHandler(
      filename = function() "cgm_complexity_metrics.csv",
      content = function(file) {
        results <- displayed_complexity_results()
        selected <- selected_subject()
        generated_ids <- if (identical(selected, all_filter_value())) {
          complexity_store_cached_ids(store_state$store)
        } else if (is.data.frame(results) && nrow(results)) {
          normalize_filter_value(selected)
        } else {
          character()
        }
        cgm_with_progress(
          "Exporting Complexity CSV",
          detail = "Preparing cached Complexity results...",
          value = 0.1,
          session = session,
          {
            out <- prepare_complexity_cached_export(
              results,
              displayed_complexity_curves(),
              display_data(),
              generated_ids,
              show_subject_id = force_subject_id_display()
            )
            shiny::incProgress(0.6, detail = "Writing CSV...")
            data.table::fwrite(out, file)
          }
        )
      }
    )

    displayed_complexity_results
  })
}
