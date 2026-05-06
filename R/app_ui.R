app_ui <- function() {
  shiny::navbarPage(
    title = "CGMAnalyzerShiny",
    id = "active_tab",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    shiny::tabPanel(
      "Data",
      value = "data",
      shiny::fluidRow(
        shiny::column(5, upload_module_ui("upload")),
        shiny::column(7, column_mapping_module_ui("column_mapping"))
      ),
      shiny::hr(),
      preprocessing_module_ui("preprocessing"),
      shiny::hr(),
      upload_preview_ui("upload")
    ),
    shiny::tabPanel(
      "Quality",
      value = "quality",
      qc_module_ui("qc")
    ),
    shiny::tabPanel(
      "Metrics",
      value = "metrics",
      metrics_module_ui("metrics")
    ),
    shiny::tabPanel(
      "Plots",
      value = "plots",
      plots_module_ui("plots")
    ),
    shiny::tabPanel(
      "Statistics",
      value = "statistics",
      stats_module_ui("stats")
    ),
    shiny::tabPanel(
      "Complexity",
      value = "complexity",
      complexity_module_ui("complexity")
    ),
    shiny::tabPanel(
      "Export",
      value = "export",
      export_module_ui("export")
    )
  )
}
