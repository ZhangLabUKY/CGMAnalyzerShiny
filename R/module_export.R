export_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Export"),
    shiny::downloadButton(ns("download_standardized"), "Original standardized data"),
    shiny::downloadButton(ns("download_analysis"), "Analysis data"),
    shiny::downloadButton(ns("download_metrics"), "Metrics"),
    shiny::downloadButton(ns("download_qc"), "QC summary"),
    shiny::downloadButton(ns("download_settings"), "Settings log")
  )
}

export_module_server <- function(id, standardized, analysis_data, metrics, qc_summary, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    output$download_standardized <- shiny::downloadHandler(
      filename = function() "cgm_standardized_data.csv",
      content = function(file) utils::write.csv(prepare_cgm_data_export(standardized()), file, row.names = FALSE)
    )

    output$download_metrics <- shiny::downloadHandler(
      filename = function() "cgm_core_metrics.csv",
      content = function(file) utils::write.csv(prepare_metrics_display(metrics(), thresholds = settings()$thresholds_mg_dl), file, row.names = FALSE)
    )

    output$download_analysis <- shiny::downloadHandler(
      filename = function() "cgm_analysis_data.csv",
      content = function(file) utils::write.csv(prepare_cgm_data_export(analysis_data()), file, row.names = FALSE)
    )

    output$download_qc <- shiny::downloadHandler(
      filename = function() "cgm_qc_summary.csv",
      content = function(file) utils::write.csv(prepare_qc_display(qc_summary(), analysis_data()), file, row.names = FALSE)
    )

    output$download_settings <- shiny::downloadHandler(
      filename = function() "cgm_reproducibility_settings.txt",
      content = function(file) {
        utils::capture.output(utils::str(settings()), file = file)
      }
    )
  })
}
