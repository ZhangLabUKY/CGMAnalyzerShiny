options(shiny.maxRequestSize = 1024^3)

app_server <- function(input, output, session) {
  session$onSessionEnded(function() {
    cleanup_background_workers()
  })
  active_tab <- shiny::reactive(input$active_tab %||% default_active_tab())
  uploaded <- upload_module_server("upload")
  mapping <- column_mapping_module_server("column_mapping", uploaded)

  standardized <- shiny::reactive({
    upload <- uploaded()
    map <- mapping()
    shiny::req(upload$data)
    shiny::req(map$timestamp, map$glucose)

    cgm_timed(
      "standardization",
      standardize_cgm_data(
        upload$data,
        mapping = map,
        units = map$source_units,
        tz = "UTC",
        upload_mode = upload$upload_mode %||% "single_file"
      ),
      context = list(upload_mode = upload$upload_mode %||% "single_file")
    )
  })

  settings <- preprocessing_module_server("preprocessing", mapping, standardized)
  imputation_run <- attr(settings, "imputation_run", exact = TRUE) %||% shiny::reactive(0L)
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
    data_validation_panel_ui(
      upload = upload,
      mapping = map,
      standardization_error = standardized_result$error,
      settings = safe_settings()
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
    data_status_strip_ui(
      upload = upload,
      mapping = map,
      standardized_data = standardized_result$data,
      standardization_error = standardized_result$error,
      settings = safe_settings()
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
      filter_analysis_date_range(standardized(), settings()$analysis_date_range)
    )
  }),
  cgm_data_signature(standardized()),
  analysis_date_range_signature(settings())
  )

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
    interval_minutes <- current_settings$imputation_interval_minutes %||% 5L
    precomputed <- tryCatch(
      cgm_timed(
        "analysis_data_imputation_precompute",
        missingness_precompute(data, interval_minutes = interval_minutes),
        context = list(method = method)
      ),
      error = function(error) NULL
    )
    result <- apply_imputation_settings(data, current_settings, precomputed = precomputed)
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
        if (!identical(method, "mice_only")) {
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
      },
      context = list(method = method)
    )
  })

  qc_summary <- qc_module_server("qc", analysis_input, analysis_data, settings, active_tab)
  metrics <- metrics_module_server("metrics", analysis_data, settings, active_tab)

  plots_module_server("plots", analysis_data, metrics, settings, active_tab)
  stats_module_server("stats", metrics, active_tab)
  complexity_module_server("complexity", analysis_data, active_tab)
  export_module_server("export", standardized, analysis_data, metrics, qc_summary, settings)
}
