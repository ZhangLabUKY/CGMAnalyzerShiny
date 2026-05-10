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

    standardize_cgm_data(
      upload$data,
      mapping = map,
      units = map$source_units,
      tz = "UTC",
      upload_mode = upload$upload_mode %||% "single_file"
    )
  })

  settings <- preprocessing_module_server("preprocessing", mapping, standardized)

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
    upload <- tryCatch(uploaded(), shiny.silent.error = function(error) NULL, error = function(error) NULL)
    map <- tryCatch(mapping(), shiny.silent.error = function(error) NULL, error = function(error) NULL)
    standardized_result <- safe_standardized()
    status <- data_setup_status(
      upload = upload,
      mapping = map,
      standardized_data = standardized_result$data,
      standardization_error = standardized_result$error,
      settings = safe_settings()
    )

    shiny::tagList(
      shiny::h4("Setup Status"),
      summary_card_ui(data.frame(Label = status$Step, Value = paste(status$Status, "-", status$Detail), stringsAsFactors = FALSE), compact = TRUE)
    )
  })

  output$data_summary <- shiny::renderUI({
    upload <- tryCatch(uploaded(), shiny.silent.error = function(error) NULL, error = function(error) NULL)
    standardized_result <- safe_standardized()
    summary <- data_upload_summary(upload, standardized_result$data)
    if (!nrow(summary)) {
      return(NULL)
    }
    shiny::tagList(
      shiny::h4("Data Summary"),
      summary_card_ui(summary)
    )
  })

  output$data_upload_hint <- shiny::renderUI({
    upload <- tryCatch(uploaded(), shiny.silent.error = function(error) NULL, error = function(error) NULL)
    if (has_uploaded_data(upload)) {
      return(NULL)
    }
    shiny::div(
      class = "alert alert-light border",
      style = "margin-top: 42px;",
      "Upload CGM files or load demo data to continue."
    )
  })

  output$data_mapping_ui <- shiny::renderUI({
    upload <- tryCatch(uploaded(), shiny.silent.error = function(error) NULL, error = function(error) NULL)
    if (!has_uploaded_data(upload)) {
      return(NULL)
    }
    column_mapping_module_ui("column_mapping")
  })

  output$data_workflow_ui <- shiny::renderUI({
    upload <- tryCatch(uploaded(), shiny.silent.error = function(error) NULL, error = function(error) NULL)
    if (!has_uploaded_data(upload)) {
      return(NULL)
    }
    shiny::tagList(
      shiny::hr(),
      preprocessing_module_ui("preprocessing"),
      shiny::uiOutput("data_setup_status"),
      shiny::uiOutput("data_summary"),
      shiny::hr(),
      upload_preview_ui("upload")
    )
  })

  analysis_input <- shiny::bindCache(shiny::reactive({
    filter_analysis_date_range(standardized(), settings()$analysis_date_range)
  }),
  cgm_data_signature(standardized()),
  analysis_date_range_signature(settings())
  )

  analysis_data <- shiny::bindCache(shiny::reactive({
    data <- analysis_input()
    current_settings <- settings()
    if (!identical(current_settings$imputation_method, "mice_only")) {
      return(data)
    }
    if (!isTRUE(current_settings$imputation_available) || !any(is.na(data$glucose))) {
      return(data)
    }

    result <- run_cgmissingdata_imputation(
      data,
      model = current_settings$imputation_model,
      seed = current_settings$imputation_seed
    )
    apply_imputed_glucose(data, result, model = current_settings$imputation_model)
  }),
  cgm_data_signature(analysis_input()),
  analysis_date_range_signature(settings()),
  settings()$imputation_method,
  settings()$imputation_model,
  settings()$imputation_seed,
  settings()$imputation_available
  )

  qc_summary <- qc_module_server("qc", analysis_input, analysis_data, settings, active_tab)
  metrics <- metrics_module_server("metrics", analysis_data, settings, active_tab)

  plots_module_server("plots", analysis_data, metrics, settings, active_tab)
  stats_module_server("stats", metrics, active_tab)
  complexity_module_server("complexity", analysis_data, active_tab)
  export_module_server("export", standardized, analysis_data, metrics, qc_summary, settings)
}
