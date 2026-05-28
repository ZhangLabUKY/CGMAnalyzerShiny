app_ui <- function() {
  shiny::navbarPage(
    title = "CGMAnalyzerShiny",
    id = "active_tab",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    # header = shiny::div(
    #   style = "position: absolute; top: 8px; right: 15px; z-index: 1050;",
    #   shinyscreenshot::screenshotButton(
    #     label = "Capture",
    #     filename = "cgm-analyzer"
    #     # The 'id' argument has been completely removed
    #   )
    # ),
    shiny::tabPanel(
      "Data",
      value = "data",
      shiny::fluidRow(
        shiny::column(5, upload_module_ui("upload")),
        shiny::column(
          7,
          shiny::uiOutput("data_upload_hint"),
          shiny::uiOutput("upload-import_setup"),
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
      "Complexity",
      value = "complexity",
      complexity_module_ui("complexity")
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
      "Export",
      value = "export",
      export_module_ui("export")
    )
  )
}
