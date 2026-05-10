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
        shiny::column(
          7,
          shiny::uiOutput("data_upload_hint"),
          shiny::uiOutput("data_mapping_ui")
        )
      ),
      shiny::uiOutput("data_workflow_ui")
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
