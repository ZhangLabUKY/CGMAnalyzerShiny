preprocessing_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Preprocessing Settings"),
    shiny::fluidRow(
      shiny::column(2, shiny::numericInput(ns("tir_lower"), "TIR lower", value = 70, min = 1)),
      shiny::column(2, shiny::numericInput(ns("tir_upper"), "TIR upper", value = 180, min = 1)),
      shiny::column(2, shiny::numericInput(ns("tbr_level2"), "TBR level 2", value = 54, min = 1)),
      shiny::column(2, shiny::numericInput(ns("tar_level2"), "TAR level 2", value = 250, min = 1)),
      shiny::column(2, shiny::numericInput(ns("valid_day_hours"), "Valid day hours", value = 14, min = 1, max = 24)),
      shiny::column(
        2,
        shiny::selectInput(
          ns("imputation"),
          "Imputation",
          choices = c("None" = "none", "MICE imputation" = "mice_only")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(2, shiny::numericInput(ns("imputation_seed"), "Imputation seed", value = 42, min = 1, step = 1)),
      shiny::column(4, shiny::uiOutput(ns("analysis_date_range_ui")))
    )
  )
}

preprocessing_module_server <- function(id, mapping, standardized) {
  shiny::moduleServer(id, function(input, output, session) {
    output$analysis_date_range_ui <- shiny::renderUI({
      range <- available_analysis_date_range(standardized())
      if (is.na(range[["start"]]) || is.na(range[["end"]])) {
        return(shiny::dateRangeInput(session$ns("analysis_date_range"), "Analysis date range"))
      }
      shiny::dateRangeInput(
        session$ns("analysis_date_range"),
        "Analysis date range",
        start = range[["start"]],
        end = range[["end"]],
        min = range[["start"]],
        max = range[["end"]]
      )
    })

    shiny::reactive({
      thresholds <- list(
        tbr_level2 = input$tbr_level2,
        tir_lower = input$tir_lower,
        tir_upper = input$tir_upper,
        tar_level2 = input$tar_level2
      )
      date_range <- normalize_analysis_date_range(input$analysis_date_range, standardized())

      create_reproducibility_settings(
        column_mapping = mapping(),
        thresholds = thresholds,
        units = mapping()$source_units,
        valid_day_hours = input$valid_day_hours,
        analysis_date_range = date_range,
        imputation_method = input$imputation,
        imputation_model = "mice_only",
        imputation_seed = input$imputation_seed,
        imputation_available = cgmissingdata_available(),
        selected_metrics = "core"
      )
    })
  })
}
