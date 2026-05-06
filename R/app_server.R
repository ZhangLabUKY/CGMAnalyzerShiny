app_server <- function(input, output, session) {
  if (requireNamespace("future", quietly = TRUE)) {
    future::plan(future::multisession)
  }
  active_tab <- shiny::reactive(input$active_tab %||% default_active_tab())
  uploaded <- upload_module_server("upload")
  mapping <- column_mapping_module_server("column_mapping", uploaded)

  standardized <- shiny::reactive({
    upload <- uploaded()
    map <- mapping()
    shiny::req(upload$data)
    shiny::req(map$timestamp, map$glucose)
    if (!is_multi_file_upload(upload)) {
      shiny::req(map$id)
    }

    standardize_cgm_data(
      upload$data,
      mapping = map,
      units = map$source_units,
      tz = "UTC",
      upload_mode = upload$upload_mode %||% "single_file"
    )
  })

  settings <- preprocessing_module_server("preprocessing", mapping, standardized)

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
