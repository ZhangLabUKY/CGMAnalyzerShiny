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
      shiny::column(4, shiny::uiOutput(ns("analysis_date_range_ui"))),
      shiny::column(
        3,
        shiny::numericInput(
          ns("expected_study_duration_days"),
          "Expected study duration days",
          value = NA,
          min = 1,
          step = 1
        )
      )
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
      shiny::uiOutput(ns("imputation_options_ui")),
      shiny::uiOutput(ns("imputation_run_status"))
    )
  )
}

imputation_options_ui <- function(selected_method = "none", ns = identity) {
  if (!identical(selected_method %||% "none", "mice_only")) {
    return(NULL)
  }
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(
          ns("imputation_model"),
          "Model",
          choices = c(
            "Auto" = "auto",
            "ARIMA" = "arima",
            "XGBoost" = "xgboost",
            "Random Forest" = "rf",
            "kNN" = "knn",
            "LightGBM" = "lightgbm"
          ),
          selected = "auto"
        )
      ),
      shiny::column(
        2,
        shiny::br(),
        shiny::actionButton(
          ns("run_imputation"),
          label = "Run imputation",
          icon = shiny::icon("play"),
          class = "btn-primary"
        )
      )
    )
  )
}

normalize_imputation_integer_vector <- function(x, default, length_required = NULL) {
  if (is.null(x) || !nzchar(trimws(as.character(x)))) {
    return(default)
  }
  values <- suppressWarnings(as.integer(strsplit(as.character(x), "[, ]+")[[1L]]))
  values <- values[!is.na(values)]
  if (!length(values)) {
    return(default)
  }
  if (!is.null(length_required) && length(values) != length_required) {
    return(default)
  }
  values
}

normalize_imputation_boundary <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return(NULL)
  }
  as.character(x[[1L]])
}

imputation_run_status_ui <- function(status, selected_method = "none") {
  if (!identical(selected_method %||% "none", "mice_only")) {
    return(NULL)
  }
  status <- status %||% list(state = "not_run")
  state <- status$state %||% "not_run"
  message <- status$message %||% switch(
    state,
    running = "Imputation is running. Analysis tabs continue to use the current non-imputed data until it completes.",
    complete = "Imputation is complete. Analysis tabs are using the imputed dataset.",
    stale = "Imputation settings changed. Analysis tabs are using the previous non-imputed or cached data until imputation is run again.",
    failed = "Imputation failed. Analysis tabs are using the non-imputed dataset.",
    "Imputation has not been run for the current dataset. Analysis tabs are using non-imputed data."
  )
  class <- switch(
    state,
    running = "alert alert-info",
    complete = "alert alert-success",
    stale = "alert alert-warning",
    failed = "alert alert-danger",
    "alert alert-light border"
  )
  shiny::div(class = class, style = "margin-top: 10px; margin-bottom: 0;", message)
}

preprocessing_module_server <- function(id, mapping, standardized) {
  shiny::moduleServer(id, function(input, output, session) {
    imputation_status <- shiny::reactiveVal(list(
      state = "not_run",
      message = "Imputation has not been run for the current dataset. Analysis tabs are using non-imputed data."
    ))

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
      interval_minutes <- input$imputation_interval_minutes %||% 5L
      precomputed <- if (is.data.frame(data) && nrow(data) && all(c("id", "timestamp", "glucose") %in% names(data))) {
        tryCatch(
          cgm_timed(
            "data_imputation_summary_precompute",
            missingness_precompute(data, interval_minutes = interval_minutes)
          ),
          error = function(error) NULL
        )
      } else {
        NULL
      }
      summary <- cgm_timed(
        "data_imputation_candidate_summary",
        imputation_missingness_summary(
          data,
          interval_minutes = interval_minutes,
          precomputed = precomputed
        )
      )
      imputation_summary_box_ui(summary)
    })

    output$imputation_options_ui <- shiny::renderUI({
      imputation_options_ui(input$imputation %||% "none", ns = session$ns)
    })

    output$imputation_run_status <- shiny::renderUI({
      imputation_run_status_ui(imputation_status(), input$imputation %||% "none")
    })

    settings <- shiny::reactive({
      thresholds <- list(
        tbr_level2 = input$tbr_level2,
        tir_lower = input$tir_lower,
        tir_upper = input$tir_upper,
        tar_level2 = input$tar_level2
      )
      date_range <- normalize_analysis_date_range(input$analysis_date_range, standardized())
      expected_duration_days <- normalize_expected_study_duration_days(input$expected_study_duration_days)

      create_reproducibility_settings(
        column_mapping = mapping(),
        thresholds = thresholds,
        units = mapping()$source_units,
        valid_day_hours = input$valid_day_hours,
        analysis_date_range = date_range,
        expected_study_duration_days = expected_duration_days,
        imputation_method = input$imputation,
        imputation_model = input$imputation_model %||% "auto",
        imputation_seed = 42L,
        imputation_available = cgmissingdata_available(),
        imputation_backend = "mice",
        imputation_interval_minutes = 5L,
        imputation_missing_warning_threshold = 0.20,
        imputation_arima_threshold = 0.05,
        imputation_arima_order = c(4L, 1L, 0L),
        imputation_arima_min_history = 20L,
        imputation_xgb_rounds = 300L,
        imputation_rf_trees = 200L,
        imputation_knn_k = 7L,
        imputation_lgb_rounds = 400L,
        imputation_lag_values = c(1L, 2L, 3L),
        imputation_add_rollmean = TRUE,
        imputation_roll_window = 3L,
        imputation_study_start = date_range[["start"]],
        imputation_study_end = date_range[["end"]],
        selected_metrics = "core"
      )
    })
    attr(settings, "imputation_run") <- shiny::reactive(input$run_imputation %||% 0L)
    attr(settings, "imputation_status") <- shiny::reactive(imputation_status())
    attr(settings, "set_imputation_status") <- function(status) {
      imputation_status(status)
      invisible(NULL)
    }
    settings
  })
}
