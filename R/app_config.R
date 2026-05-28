default_cgm_thresholds <- function() {
  list(
    tbr_level2 = 54,
    tir_lower = 70,
    tir_upper = 180,
    tar_level2 = 250
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Create a reproducibility settings object
#'
#' @param column_mapping Named list of source column mappings.
#' @param thresholds Named list of glucose thresholds in mg/dL.
#' @param units Source glucose units selected by the user.
#' @param valid_day_hours Minimum observed hours for a valid CGM day.
#' @param analysis_date_range Length-two date range used to scope analysis data.
#' @param expected_study_duration_days Optional protocol duration in days.
#' @param imputation_method Imputation method label.
#' @param imputation_backend CGMissingDataR backend, either `mice` or `sklearn`.
#' @param selected_metrics Character vector of selected metrics.
#' @param selected_tests Character vector of selected statistical tests.
#'
#' @return A named list of analysis settings.
#' @noRd
create_reproducibility_settings <- function(
  column_mapping = list(),
  thresholds = default_cgm_thresholds(),
  units = "mg/dL",
  valid_day_hours = 14,
  analysis_date_range = c(start = NA_character_, end = NA_character_),
  expected_study_duration_days = NA_integer_,
  imputation_method = "none",
  imputation_model = "mice_only",
  imputation_seed = 42,
  imputation_available = cgmissingdata_available(),
  imputation_backend = "mice",
  imputation_interval_minutes = 5L,
  imputation_arima_threshold = 0.05,
  imputation_arima_min_history = 20L,
  imputation_xgb_rounds = 300L,
  selected_metrics = "core",
  selected_tests = character()
) {
  list(
    app = "CGMAnalyzerShiny",
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    timestamp_format = "YYYY-MM-DDThh:mm:ss",
    ambiguous_timestamp_rule = "day_first",
    column_mapping = column_mapping,
    thresholds_mg_dl = thresholds,
    source_units = units,
    valid_day_hours = valid_day_hours,
    analysis_date_range = analysis_date_range,
    expected_study_duration_days = normalize_expected_study_duration_days(expected_study_duration_days),
    imputation_method = imputation_method,
    imputation_model = imputation_model,
    imputation_seed = imputation_seed,
    imputation_available = imputation_available,
    imputation_backend = imputation_backend,
    imputation_interval_minutes = imputation_interval_minutes,
    imputation_arima_threshold = imputation_arima_threshold,
    imputation_arima_min_history = imputation_arima_min_history,
    imputation_xgb_rounds = imputation_xgb_rounds,
    selected_metrics = selected_metrics,
    selected_tests = selected_tests
  )
}

optional_engine_status <- function() {
  data.frame(
    package = c("CGManalyzer", "iglu", "CGMissingDataR"),
    role = c("preferred CGM analysis engine", "fallback and validation", "missing-value imputation"),
    installed = c(
      requireNamespace("CGManalyzer", quietly = TRUE),
      requireNamespace("iglu", quietly = TRUE),
      requireNamespace("CGMissingDataR", quietly = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}
