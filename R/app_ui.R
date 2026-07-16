app_ui <- function() {
  shiny::navbarPage(
    title = app_brand_ui(),
    id = "active_tab",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    header = app_theme_css(),
    shiny::tabPanel(
      "Data",
      value = "data",
      shiny::div(
        class = "cgm-data-tab",
        shiny::uiOutput("data_status_strip"),
        data_workflow_tabs_ui(
          setup_ui = shiny::tagList(
            shiny::fluidRow(
              shiny::column(5, upload_module_ui("upload")),
              shiny::column(
                7,
                shiny::uiOutput("data_upload_hint"),
                shiny::uiOutput("upload-import_setup"),
                shiny::uiOutput("data_mapping_ui")
              )
            ),
            shiny::hr(),
            shiny::uiOutput("data_preprocessing_settings_ui")
          ),
          validate_ui = shiny::uiOutput("data_setup_status"),
          impute_ui = shiny::uiOutput("data_imputation_ui"),
          preview_ui = shiny::uiOutput("data_preview_ui")
        )
      )
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
      "Glycemic Patterns",
      value = "functional_profiles",
      functional_profiles_module_ui("functional_profiles")
    ),
    shiny::tabPanel(
      "Predictive Risk",
      value = "predictive_risk",
      predictive_risk_module_ui("predictive_risk")
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
