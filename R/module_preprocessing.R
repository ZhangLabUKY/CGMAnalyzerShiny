preprocessing_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Preprocessing settings"),
    shiny::fluidRow(
      shiny::column(2, shiny::numericInput(ns("tir_lower"), "TIR lower", value = 70, min = 1)),
      shiny::column(2, shiny::numericInput(ns("tir_upper"), "TIR upper", value = 180, min = 1)),
      shiny::column(2, shiny::numericInput(ns("tbr_level2"), "TBR level 2", value = 54, min = 1)),
      shiny::column(2, shiny::numericInput(ns("tar_level2"), "TAR level 2", value = 250, min = 1)),
      shiny::column(2, shiny::numericInput(ns("valid_day_hours"), "Valid day hours", value = 14, min = 1, max = 24))
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::uiOutput(ns("analysis_date_range_ui")))
    ),
    shiny::div(
      id = ns("imputation_panel"),
      class = "card mb-3",
      style = "padding: 14px 16px;",
      shiny::h4("Imputation"),
      shiny::uiOutput(ns("imputation_summary")),
      shiny::fluidRow(
        shiny::column(
          3,
          shiny::selectInput(
            ns("imputation"),
            "Missing glucose imputation",
            choices = c("None" = "none", "Apply imputation" = "mice_only")
          )
        )
      ),
      shiny::uiOutput(ns("imputation_options_ui"))
    )
  )
}

imputation_options_ui <- function(selected_method = "none", ns = identity) {
  if (!identical(selected_method %||% "none", "mice_only")) {
    return(NULL)
  }
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(2, shiny::numericInput(ns("imputation_seed"), "Seed", value = 42, min = 1, step = 1)),
      shiny::column(
        2,
        shiny::selectInput(
          ns("imputation_backend"),
          "Backend",
          choices = c("R/mice" = "mice", "Python/sklearn" = "sklearn"),
          selected = "mice"
        )
      ),
      shiny::column(2, shiny::numericInput(ns("imputation_interval_minutes"), "Interval minutes", value = 5, min = 1, step = 1)),
      shiny::column(2, shiny::numericInput(ns("imputation_arima_threshold"), "ARIMA threshold", value = 0.05, min = 0, max = 1, step = 0.01)),
      shiny::column(2, shiny::numericInput(ns("imputation_arima_min_history"), "ARIMA min history", value = 20, min = 1, step = 1)),
      shiny::column(2, shiny::numericInput(ns("imputation_xgb_rounds"), "XGBoost rounds", value = 300, min = 1, step = 10))
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

    output$imputation_summary <- shiny::renderUI({
      data <- tryCatch({
        date_range <- normalize_analysis_date_range(input$analysis_date_range, standardized())
        filter_analysis_date_range(standardized(), date_range)
      }, shiny.silent.error = function(error) NULL, error = function(error) NULL)
      imputation_summary_box_ui(imputation_missingness_summary(data))
    })

    output$imputation_options_ui <- shiny::renderUI({
      imputation_options_ui(input$imputation %||% "none", ns = session$ns)
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
        imputation_seed = input$imputation_seed %||% 42,
        imputation_available = cgmissingdata_available(),
        imputation_backend = input$imputation_backend %||% "mice",
        imputation_interval_minutes = input$imputation_interval_minutes %||% 5L,
        imputation_arima_threshold = input$imputation_arima_threshold %||% 0.05,
        imputation_arima_min_history = input$imputation_arima_min_history %||% 20L,
        imputation_xgb_rounds = input$imputation_xgb_rounds %||% 300L,
        selected_metrics = "core"
      )
    })
  })
}
