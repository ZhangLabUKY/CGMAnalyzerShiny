default_cgm_thresholds <- function() {
  list(
    tbr_level2 = 54,
    tir_lower = 70,
    tir_upper = 180,
    tar_level2 = 250
  )
}

app_package_name <- function() {
  "CGManalyzer2"
}

app_display_name <- function() {
  "CGManalyzer2"
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

app_version <- function() {
  if (file.exists("DESCRIPTION")) {
    description <- tryCatch(
      read.dcf("DESCRIPTION", fields = "Version"),
      error = function(error) NA_character_
    )
    version <- as.character(description[[1L]])
    if (!is.na(version) && nzchar(version)) {
      return(version)
    }
  }
  installed_version <- tryCatch(
    utils::packageDescription(app_package_name(), fields = "Version"),
    error = function(error) NA_character_
  )
  if (!is.na(installed_version) && nzchar(installed_version)) {
    return(as.character(installed_version))
  }
  "unknown"
}

app_version_label <- function() {
  paste0("v", app_version())
}

app_brand_ui <- function() {
  shiny::span(
    class = "cgm-brand-stack",
    shiny::span(class = "cgm-brand-name", app_display_name()),
    shiny::span(class = "cgm-version-label", app_version_label())
  )
}

app_theme_css_path <- function() {
  local_path <- file.path("www", "cgm-theme.css")
  if (file.exists(local_path)) {
    return(local_path)
  }
  source_package_path <- file.path("inst", "www", "cgm-theme.css")
  if (file.exists(source_package_path)) {
    return(source_package_path)
  }
  package_path <- system.file(
    "www",
    "cgm-theme.css",
    package = app_package_name()
  )
  if (nzchar(package_path)) {
    return(package_path)
  }
  NA_character_
}

app_theme_css <- function() {
  path <- app_theme_css_path()
  if (is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  shiny::includeCSS(path)
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
#' @param imputation_model CGMissingDataR model selection.
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
  imputation_model = "auto",
  imputation_seed = 42,
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
  imputation_study_start = NULL,
  imputation_study_end = NULL,
  selected_metrics = "core",
  selected_tests = character()
) {
  if (is.null(imputation_study_start) && length(analysis_date_range) >= 1L && !is.na(analysis_date_range[[1L]])) {
    imputation_study_start <- as.character(analysis_date_range[[1L]])
  }
  if (is.null(imputation_study_end) && length(analysis_date_range) >= 2L && !is.na(analysis_date_range[[2L]])) {
    imputation_study_end <- as.character(analysis_date_range[[2L]])
  }
  list(
    app = app_display_name(),
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
    imputation_missing_warning_threshold = imputation_missing_warning_threshold,
    imputation_arima_threshold = imputation_arima_threshold,
    imputation_arima_order = imputation_arima_order,
    imputation_arima_min_history = imputation_arima_min_history,
    imputation_xgb_rounds = imputation_xgb_rounds,
    imputation_rf_trees = imputation_rf_trees,
    imputation_knn_k = imputation_knn_k,
    imputation_lgb_rounds = imputation_lgb_rounds,
    imputation_lag_values = imputation_lag_values,
    imputation_add_rollmean = imputation_add_rollmean,
    imputation_roll_window = imputation_roll_window,
    imputation_study_start = imputation_study_start,
    imputation_study_end = imputation_study_end,
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
