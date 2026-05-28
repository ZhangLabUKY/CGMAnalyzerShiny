available_analysis_date_range <- function(data) {
  if (is.null(data) || !is.data.frame(data) || !"timestamp" %in% names(data)) {
    return(c(start = NA_character_, end = NA_character_))
  }

  dates <- as.Date(data$timestamp[is_finite_cgm_timestamp(data$timestamp)])
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    return(c(start = NA_character_, end = NA_character_))
  }

  c(start = as.character(min(dates)), end = as.character(max(dates)))
}

normalize_analysis_date_range <- function(date_range, data = NULL) {
  available <- available_analysis_date_range(data)
  start <- as.Date(date_range[[1L]] %||% available[["start"]])
  end <- as.Date(date_range[[2L]] %||% available[["end"]])

  if (is.na(start)) {
    start <- as.Date(available[["start"]])
  }
  if (is.na(end)) {
    end <- as.Date(available[["end"]])
  }
  if (!is.na(start) && !is.na(end) && start > end) {
    tmp <- start
    start <- end
    end <- tmp
  }

  c(start = as.character(start), end = as.character(end))
}

#' Filter standardized CGM data to the global analysis date range
#'
#' @param data Standardized CGM data with a POSIXct `timestamp` column.
#' @param date_range Length-two vector or list with start and end dates.
#'
#' @return Filtered standardized CGM data.
#' @noRd
filter_analysis_date_range <- function(data, date_range = NULL) {
  if (is.null(date_range) || !nrow(data) || !"timestamp" %in% names(data)) {
    return(data)
  }

  range <- normalize_analysis_date_range(date_range, data)
  start <- as.Date(range[["start"]])
  end <- as.Date(range[["end"]])
  if (is.na(start) && is.na(end)) {
    return(data)
  }

  dates <- as.Date(data$timestamp)
  keep <- !is.na(dates)
  if (!is.na(start)) {
    keep <- keep & dates >= start
  }
  if (!is.na(end)) {
    keep <- keep & dates <= end
  }
  data[keep, , drop = FALSE]
}

analysis_date_range_signature <- function(settings) {
  range <- settings$analysis_date_range %||% c(start = NA_character_, end = NA_character_)
  c(start = as.character(range[[1L]]), end = as.character(range[[2L]]))
}

normalize_expected_study_duration_days <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (!length(value) || is.na(value[[1L]]) || !is.finite(value[[1L]]) || value[[1L]] <= 0) {
    return(NA_integer_)
  }
  as.integer(round(value[[1L]]))
}

expected_study_duration_signature <- function(settings) {
  as.character(normalize_expected_study_duration_days(settings$expected_study_duration_days %||% NA))
}

study_window_summary <- function(data, expected_duration_days = NA) {
  expected_duration_days <- normalize_expected_study_duration_days(expected_duration_days)
  if (
    is.null(data) ||
      !is.data.frame(data) ||
      !nrow(data) ||
      !all(c("id", "timestamp") %in% names(data)) ||
      !any(is_finite_cgm_timestamp(data$timestamp))
  ) {
    return(data.frame(
      `Subject ID` = character(),
      `First recorded date` = as.Date(character()),
      `Last recorded date` = as.Date(character()),
      `Observed days` = integer(),
      `Expected days` = integer(),
      `Shortfall days` = integer(),
      `Study window status` = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  dt <- data.table::as.data.table(data[is_finite_cgm_timestamp(data$timestamp), c("id", "timestamp"), drop = FALSE])
  dt[, id := as.character(id)]
  dt <- dt[!is.na(id) & nzchar(id)]
  if (!nrow(dt)) {
    return(study_window_summary(data[0, , drop = FALSE], expected_duration_days))
  }

  dt[, date := as.Date(timestamp)]
  summary <- dt[, list(
    `First recorded date` = min(date, na.rm = TRUE),
    `Last recorded date` = max(date, na.rm = TRUE)
  ), by = id]
  summary[, `Observed days` := as.integer(`Last recorded date` - `First recorded date`) + 1L]
  summary[, `Expected days` := expected_duration_days]
  summary[, `Shortfall days` := if (is.na(expected_duration_days)) NA_integer_ else pmax(0L, expected_duration_days - `Observed days`)]
  summary[, `Study window status` := if (is.na(expected_duration_days)) {
    "No expected duration set"
  } else ifelse(`Shortfall days` > 0L, "Short observed span", "Complete observed span")]
  data.table::setnames(summary, "id", "Subject ID")

  out <- as.data.frame(summary[order(`Subject ID`)], stringsAsFactors = FALSE)
  if (!subject_id_filter_available(data)) {
    out <- out[, setdiff(names(out), "Subject ID"), drop = FALSE]
  }
  row.names(out) <- NULL
  out
}

imputation_settings_signature <- function(settings) {
  c(
    method = as.character(settings$imputation_method %||% "none"),
    model = as.character(settings$imputation_model %||% "mice_only"),
    seed = as.character(settings$imputation_seed %||% 42),
    available = as.character(isTRUE(settings$imputation_available %||% FALSE)),
    backend = as.character(settings$imputation_backend %||% "mice"),
    interval_minutes = as.character(settings$imputation_interval_minutes %||% 5L),
    arima_threshold = as.character(settings$imputation_arima_threshold %||% 0.05),
    arima_min_history = as.character(settings$imputation_arima_min_history %||% 20L),
    xgb_rounds = as.character(settings$imputation_xgb_rounds %||% 300L)
  )
}
