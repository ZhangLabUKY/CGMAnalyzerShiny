options(shiny.maxRequestSize = 1024^3)

analysis_missingness_review_cache_key <- function(data, date_range, interval_minutes) {
  cgm_cache_key(
    cgm_data_signature(data),
    analysis_date_range_signature(list(analysis_date_range = date_range)),
    list(interval_minutes = interval_minutes)
  )
}

compute_analysis_missingness_review <- function(data, interval_minutes) {
  grid <- cgm_timed(
    "data_missingness_review_fast",
    cgm_suppress_non_cgma_messages(
      fast_missingness_grid_summary_by_id(
        data,
        interval_minutes = interval_minutes
      )
    )
  )
  cgm_timed(
    "data_imputation_candidate_summary_fast",
    cgm_suppress_non_cgma_messages(
      imputation_missingness_summary(
        data,
        interval_minutes = interval_minutes,
        precomputed = grid,
        include_timestamp_gaps = TRUE
      )
    )
  )
}

shared_missingness_review_cached <- function(cache_env, key, compute) {
  if (
    is.environment(cache_env) &&
      identical(cache_env$analysis_missingness_review_key %||% NULL, key) &&
      !is.null(cache_env$analysis_missingness_review_value)
  ) {
    cgm_log_performance(
      "data_missingness_review_cache_hit",
      elapsed_ms = 0,
      rows = nrow(cache_env$analysis_missingness_review_value)
    )
    return(cache_env$analysis_missingness_review_value)
  }
  value <- compute()
  if (is.environment(cache_env)) {
    cache_env$analysis_missingness_review_key <- key
    cache_env$analysis_missingness_review_value <- value
  }
  value
}

app_server <- function(input, output, session) {
  session$onSessionEnded(function() {
    cleanup_background_workers()
  })
  active_tab <- shiny::reactive(input$active_tab %||% default_active_tab())
  uploaded <- upload_module_server("upload")
  mapping <- column_mapping_module_server("column_mapping", uploaded)
  shared_missingness <- new.env(parent = emptyenv())

  standardization_cache_signature <- function(upload, map) {
    datapaths <- upload$datapaths %||% character()
    file_info <- if (length(datapaths)) {
      info <- file.info(datapaths)
      data.frame(
        path = normalizePath(datapaths, winslash = "/", mustWork = FALSE),
        size = info$size,
        mtime = as.numeric(info$mtime),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
    list(
      files = upload$files,
      file_info = file_info,
      row_boundaries = upload$row_boundaries,
      upload_mode = upload$upload_mode %||% "single_file",
      mapping = map[c(required_cgm_columns(), optional_cgm_columns(), "source_units", "timestamp_parser")]
    )
  }

  standardization_input_data <- shiny::reactive({
    upload <- uploaded()
    map <- mapping()
    shiny::req(upload$data)
    shiny::req(map$timestamp, map$glucose)

    if (isTRUE(upload$sampled) && length(upload$datapaths %||% character())) {
      return(cgm_timed(
        "standardization_selected_column_read",
        combine_uploaded_files(
          upload$datapaths,
          upload$files,
          header_rows = upload$row_boundaries$header_row,
          first_data_rows = upload$row_boundaries$first_data_row,
          select_columns = selected_upload_columns(map, upload_mode = upload$upload_mode %||% "single_file")
        ),
        context = list(upload_mode = upload$upload_mode %||% "single_file")
      ))
    }

    upload$data
  })

  standardized <- shiny::bindCache(shiny::reactive({
    upload <- uploaded()
    map <- mapping()
    shiny::req(upload$data)
    shiny::req(map$timestamp, map$glucose)
    cgm_with_progress(
      "Preparing CGM data",
      detail = "Reading selected columns...",
      value = 0,
      {
        data <- standardization_input_data()
        shiny::incProgress(0.35, detail = "Parsing timestamps and glucose values...")
        result <- cgm_timed(
          "standardization",
          standardize_cgm_data(
            data,
            mapping = map,
            units = map$source_units,
            tz = "UTC",
            timestamp_parser = map$timestamp_parser %||% "compatibility",
            upload_mode = upload$upload_mode %||% "single_file"
          ),
          context = list(
            upload_mode = upload$upload_mode %||% "single_file",
            timestamp_parser = map$timestamp_parser %||% "compatibility"
          )
        )
        shiny::incProgress(0.55, detail = "Finalizing standardized data...")
        row_count <- nrow(data)
        rm(data)
        cgm_maybe_gc(row_count)
        result
      }
    )
  }),
  standardization_cache_signature(uploaded(), mapping()),
  cache = "session"
  )

  settings <- preprocessing_module_server("preprocessing", mapping, standardized, shared_missingness = shared_missingness)
  imputation_run <- attr(settings, "imputation_run", exact = TRUE) %||% shiny::reactive(0L)
  imputation_status <- attr(settings, "imputation_status", exact = TRUE) %||% shiny::reactive(list(state = "not_run"))
  set_imputation_status <- attr(settings, "set_imputation_status", exact = TRUE) %||% function(status) invisible(NULL)

  safe_standardized <- shiny::reactive({
    tryCatch(
      list(data = standardized(), error = NULL),
      shiny.silent.error = function(error) list(data = NULL, error = NULL),
      error = function(error) list(data = NULL, error = conditionMessage(error))
    )
  })

  safe_settings <- shiny::reactive({
    tryCatch(
      settings(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
  })

  output$data_setup_status <- shiny::renderUI({
    upload <- tryCatch(
      uploaded(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    map <- tryCatch(
      mapping(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    standardized_result <- safe_standardized()
    missingness_review <- tryCatch(
      if (is.function(shared_missingness$analysis_missingness_review)) {
        shared_missingness$analysis_missingness_review()
      } else {
        NULL
      },
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    data_validation_panel_ui(
      upload = upload,
      mapping = map,
      standardized_data = standardized_result$data,
      standardization_error = standardized_result$error,
      settings = safe_settings(),
      missingness_review = missingness_review
    )
  })

  output$data_status_strip <- shiny::renderUI({
    upload <- tryCatch(
      uploaded(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    map <- tryCatch(
      mapping(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    standardized_result <- safe_standardized()
    missingness_review <- tryCatch(
      if (is.function(shared_missingness$analysis_missingness_review)) {
        shared_missingness$analysis_missingness_review()
      } else {
        NULL
      },
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    data_status_strip_ui(
      upload = upload,
      mapping = map,
      standardized_data = standardized_result$data,
      standardization_error = standardized_result$error,
      settings = safe_settings(),
      missingness_review = missingness_review
    )
  })

  output$data_upload_hint <- shiny::renderUI({
    upload <- tryCatch(
      uploaded(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    if (has_uploaded_data(upload)) {
      return(NULL)
    }
    shiny::div(
      class = "alert alert-light border",
      style = "margin-top: 42px;",
      "Upload CGM files or load example data to continue."
    )
  })

  output$data_mapping_ui <- shiny::renderUI({
    upload <- tryCatch(
      uploaded(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    if (!has_uploaded_data(upload)) {
      return(NULL)
    }
    column_mapping_module_ui("column_mapping")
  })

  output$data_preprocessing_settings_ui <- shiny::renderUI({
    upload <- tryCatch(
      uploaded(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    if (!has_uploaded_data(upload)) {
      return(NULL)
    }
    preprocessing_settings_ui("preprocessing")
  })

  output$data_imputation_ui <- shiny::renderUI({
    upload <- tryCatch(
      uploaded(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    if (!has_uploaded_data(upload)) {
      return(shiny::div(
        class = "alert alert-light border",
        "Upload CGM files or load example data to review imputation."
      ))
    }
    preprocessing_imputation_ui("preprocessing")
  })

  output$data_preview_ui <- shiny::renderUI({
    upload <- tryCatch(
      uploaded(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    if (!has_uploaded_data(upload)) {
      return(shiny::div(
        class = "alert alert-light border cgm-data-preview-placeholder",
        "Upload CGM files or load example data to show the data preview."
      ))
    }
    map <- tryCatch(
      mapping(),
      shiny.silent.error = function(error) NULL,
      error = function(error) NULL
    )
    if (!required_preview_mappings_selected(map)) {
      return(shiny::div(
        class = "alert alert-light border cgm-data-preview-placeholder",
        "Select Timestamp and Glucose columns to show the data preview."
      ))
    }
    upload_preview_ui("upload")
  })

  analysis_input <- shiny::bindCache(shiny::reactive({
    cgm_timed(
      "analysis_date_filter",
      attach_cgm_data_signature(
        filter_analysis_date_range(standardized(), settings()$analysis_date_range)
      )
    )
  }),
  cgm_data_signature(standardized()),
  analysis_date_range_signature(settings()),
  cache = "session"
  )

  shared_missingness$analysis_missingness_review <- shiny::reactive({
    data <- analysis_input()
    interval_minutes <- settings()$imputation_interval_minutes %||% 5L
    review_key <- analysis_missingness_review_cache_key(
      data,
      settings()$analysis_date_range,
      interval_minutes
    )
    shared_missingness_review_cached(
      shared_missingness,
      review_key,
      function() {
        cgm_with_progress(
          "Reviewing timestamp gaps...",
          value = 0.4,
          compute_analysis_missingness_review(data, interval_minutes)
        )
      }
    )
  })

  imputation_cache <- shiny::reactiveVal(NULL)
  imputation_request_signature <- function(data, current_settings) {
    list(
      data = cgm_data_signature(data),
      date_range = analysis_date_range_signature(current_settings),
      imputation = imputation_settings_signature(current_settings)
    )
  }

  imputation_status_message <- function(state, details = NULL) {
    base <- switch(
      state,
      running = "Imputation is running. Analysis tabs continue to use non-imputed data until it completes.",
      complete = "Imputation is complete. Analysis tabs are using the imputed dataset.",
      stale = "Imputation settings or analysis data changed. Run imputation again to refresh imputed analysis data.",
      failed = "Imputation failed. Analysis tabs are using non-imputed data.",
      "Imputation has not been run for the current dataset. Analysis tabs are using non-imputed data."
    )
    if (!is.null(details) && nzchar(details)) {
      paste(base, "Details:", details)
    } else {
      base
    }
  }

  shiny::observe({
    current_settings <- settings()
    method <- current_settings$imputation_method %||% "none"
    if (!identical(method, "mice_only")) {
      set_imputation_status(list(state = "not_run", message = imputation_status_message("not_run")))
      return(invisible(NULL))
    }
    data <- analysis_input()
    signature <- imputation_request_signature(data, current_settings)
    cached <- imputation_cache()
    if (!is.null(cached) && identical(cached$signature, signature)) {
      set_imputation_status(list(state = "complete", message = imputation_status_message("complete")))
    } else if (!is.null(cached)) {
      set_imputation_status(list(state = "stale", message = imputation_status_message("stale")))
    } else {
      set_imputation_status(list(state = "not_run", message = imputation_status_message("not_run")))
    }
    invisible(NULL)
  })

  shiny::observeEvent(imputation_run(), {
    data <- analysis_input()
    current_settings <- settings()
    method <- current_settings$imputation_method %||% "none"
    if (!identical(method, "mice_only")) {
      set_imputation_status(list(state = "not_run", message = imputation_status_message("not_run")))
      return(invisible(NULL))
    }
    signature <- imputation_request_signature(data, current_settings)
    cached <- imputation_cache()
    if (!is.null(cached) && identical(cached$signature, signature)) {
      set_imputation_status(list(state = "complete", message = imputation_status_message("complete")))
      return(invisible(NULL))
    }

    set_imputation_status(list(state = "running", message = imputation_status_message("running")))
    result <- cgm_with_progress(
      "Running imputation",
      detail = "Preparing missingness inputs...",
      value = 0,
      {
        interval_minutes <- current_settings$imputation_interval_minutes %||% 5L
        precomputed <- tryCatch(
          {
            value <- cgm_timed(
              "analysis_data_imputation_precompute",
              cgm_suppress_non_cgma_messages(
                missingness_precompute(data, interval_minutes = interval_minutes)
              ),
              context = list(method = method)
            )
            cgm_maybe_gc(nrow(data))
            value
          },
          error = function(error) NULL
        )
        shiny::incProgress(0.35, detail = "Applying selected imputation method...")
        imputed <- attach_cgm_data_signature(apply_imputation_settings(data, current_settings, precomputed = precomputed))
        shiny::incProgress(0.55, detail = "Finalizing imputed analysis data...")
        imputed
      }
    )
    error_message <- attr(result, "imputation_error", exact = TRUE)
    if (!is.null(error_message) && nzchar(error_message)) {
      set_imputation_status(list(state = "failed", message = imputation_status_message("failed", error_message)))
      return(invisible(NULL))
    }

    imputation_cache(list(
      signature = signature,
      data = result,
      completed_at = Sys.time()
    ))
    set_imputation_status(list(state = "complete", message = imputation_status_message("complete")))
    invisible(NULL)
  }, ignoreInit = TRUE)

  analysis_data <- shiny::reactive({
    data <- analysis_input()
    current_settings <- settings()
    method <- current_settings$imputation_method %||% "none"
    cgm_timed(
      "analysis_data_select",
      {
        out <- if (!identical(method, "mice_only")) {
          data
        } else {
          signature <- imputation_request_signature(data, current_settings)
          cached <- imputation_cache()
          if (!is.null(cached) && identical(cached$signature, signature)) {
            cached$data
          } else {
            attr(data, "imputation_pending") <- if (is.null(cached)) "not_run" else "stale"
            data
          }
        }
        attach_cgm_data_signature(out)
      },
      context = list(method = method)
    )
  })

  shared_missingness$analysis_missingness_precompute <- shiny::bindCache(shiny::reactive({
    req_active_tab(active_tab, "quality")
    data <- analysis_data()
    result <- cgm_with_progress(
      "Preparing Quality missingness review",
      detail = "Scanning analysis data for timestamp gaps...",
      value = 0.2,
      {
        cgm_timed(
          "quality_analysis_missingness_precompute",
          cgm_suppress_non_cgma_messages(
            missingness_precompute(
              data,
              interval_minutes = settings()$imputation_interval_minutes %||% 5L
            )
          )
        )
      }
    )
    cgm_maybe_gc(nrow(data))
    result
  }),
  cgm_data_signature(analysis_data()),
  settings()$imputation_interval_minutes,
  cache = "session"
  )

  shared_missingness$standardized_missingness_precompute <- shiny::bindCache(shiny::reactive({
    req_active_tab(active_tab, "quality")
    if (
      !should_show_analysis_missingness(settings()) &&
        identical(cgm_data_signature(analysis_input()), cgm_data_signature(analysis_data()))
    ) {
      return(shared_missingness$analysis_missingness_precompute())
    }
    data <- analysis_input()
    result <- cgm_with_progress(
      "Preparing Quality baseline review",
      detail = "Scanning original standardized data...",
      value = 0.2,
      {
        cgm_timed(
          "quality_standardized_missingness_precompute",
          cgm_suppress_non_cgma_messages(
            missingness_precompute(
              data,
              interval_minutes = settings()$imputation_interval_minutes %||% 5L
            )
          )
        )
      }
    )
    cgm_maybe_gc(nrow(data))
    result
  }),
  cgm_data_signature(analysis_input()),
  cgm_data_signature(analysis_data()),
  settings()$imputation_interval_minutes,
  should_show_analysis_missingness(settings()),
  cache = "session"
  )

  qc_summary <- qc_module_server(
    "qc",
    analysis_input,
    analysis_data,
    settings,
    active_tab,
    standardized_missingness_precompute = shared_missingness$standardized_missingness_precompute,
    analysis_missingness_precompute = shared_missingness$analysis_missingness_precompute
  )
  metrics <- metrics_module_server("metrics", analysis_data, settings, active_tab)

  plots_module_server("plots", analysis_data, metrics, settings, active_tab)
  stats_module_server("stats", metrics, active_tab)
  complexity_module_server("complexity", analysis_data, active_tab)
  export_module_server("export", analysis_data, settings, imputation_status)
}
