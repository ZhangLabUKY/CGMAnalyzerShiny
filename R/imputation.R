cgmissingdata_function <- function() {
  if (!requireNamespace("CGMissingDataR", quietly = TRUE)) {
    return(NULL)
  }
  exports <- getNamespaceExports("CGMissingDataR")
  if (!"run_missing_glucose_imputation" %in% exports) {
    return(NULL)
  }
  getExportedValue("CGMissingDataR", "run_missing_glucose_imputation")
}

#' Check whether CGMissingDataR imputation is available
#'
#' @return Logical.
#' @noRd
cgmissingdata_available <- function() {
  !is.null(cgmissingdata_function())
}

first_non_missing <- function(x, default = NA) {
  x <- x[!is.na(x)]
  if (!length(x)) default else x[[1L]]
}

prepare_cgmissingdata_input <- function(data) {
  subject_levels <- unique(as.character(data$id))
  out <- data.frame(
    .row_id = seq_len(nrow(data)),
    subject_index = as.integer(factor(as.character(data$id), levels = subject_levels)),
    time = format_cgm_timestamp_iso(data$timestamp, tz = "UTC"),
    glucose = as.numeric(data$glucose),
    stringsAsFactors = FALSE
  )
  out
}

#' Run CGMissingDataR missing glucose imputation
#'
#' @param data Standardized CGM data.
#' @param seed Integer seed.
#' @param backend Imputation backend. Use `mice` for R-native imputation or
#'   `sklearn` for the Python backend.
#'
#' @return Output from `CGMissingDataR::run_missing_glucose_imputation()`.
#' @noRd
run_cgmissingdata_imputation <- function(
  data,
  seed = 42,
  backend = "mice",
  interval_minutes = 5L,
  arima_threshold = 0.05,
  arima_min_history = 20L,
  xgb_rounds = 300L
) {
  fun <- cgmissingdata_function()
  if (is.null(fun)) {
    stop(
      "CGMissingDataR::run_missing_glucose_imputation() is not available. Install the GitHub/current CGMissingDataR version.",
      call. = FALSE
    )
  }
  if (!all(c("id", "timestamp", "glucose") %in% names(data))) {
    stop("Imputation requires standardized columns: id, timestamp, glucose.", call. = FALSE)
  }
  if (!any(is.na(data$glucose))) {
    stop("No missing glucose values are available for imputation.", call. = FALSE)
  }

  imputation_input <- prepare_cgmissingdata_input(data)
  result <- fun(
    imputation_input,
    target_col = "glucose",
    feature_cols = character(0),
    id_col = "subject_index",
    time_col = "time",
    seed = seed,
    interval_minutes = interval_minutes,
    use_arima_if_missing_leq = arima_threshold,
    arima_min_history = arima_min_history,
    xgb_nrounds = xgb_rounds,
    imputer_backend = backend,
    export = FALSE
  )
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (!"imputation_method" %in% names(result)) {
    result$imputation_method <- if (identical(backend, "sklearn")) "sklearn" else "mice"
  }
  if (!"missing_rate" %in% names(result)) {
    result$missing_rate <- mean(is.na(data$glucose))
  }
  result
}

#' Apply imputed glucose values to standardized CGM data
#'
#' @param data Standardized CGM data.
#' @param imputation_result Result from `run_cgmissingdata_imputation()`.
#'
#' @return Standardized CGM data with imputed glucose values and flags.
#' @noRd
apply_imputed_glucose <- function(data, imputation_result) {
  if (!is.data.frame(imputation_result)) {
    stop("CGMissingDataR imputation result must be a data frame.", call. = FALSE)
  }
  imputed <- as.data.frame(imputation_result, stringsAsFactors = FALSE)
  row_id_col <- ".row_id"
  needed <- c(row_id_col, "imputed_glucose_value")
  missing_cols <- setdiff(needed, names(imputed))
  if (length(missing_cols)) {
    stop("CGMissingDataR imputed data missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  out <- data
  row_id <- as.integer(imputed[[row_id_col]])
  valid <- !is.na(row_id) & row_id >= 1L & row_id <= nrow(out)
  imputed <- imputed[valid, , drop = FALSE]
  row_id <- row_id[valid]

  originally_missing <- is.na(out$glucose[row_id])
  fill_rows <- row_id[originally_missing]
  fill_values <- as.numeric(imputed$imputed_glucose_value[originally_missing])
  fill_ok <- is.finite(fill_values)

  if (any(fill_ok)) {
    out$glucose[fill_rows[fill_ok]] <- fill_values[fill_ok]
    out$imputed_flag[fill_rows[fill_ok]] <- TRUE
  }
  if ("imputation_method" %in% names(imputed)) {
    attr(out, "imputation_method") <- first_non_missing(unique(as.character(imputed$imputation_method)), NA_character_)
  }
  if ("missing_rate" %in% names(imputed)) {
    attr(out, "imputation_missing_rate") <- first_non_missing(unique(as.numeric(imputed$missing_rate)), NA_real_)
  }
  out
}

analysis_data_from_settings <- function(data, settings) {
  method <- settings()$imputation_method %||% "none"
  if (!identical(method, "mice_only")) {
    return(data())
  }

  result <- run_cgmissingdata_imputation(
    data(),
    seed = settings()$imputation_seed %||% 42,
    backend = settings()$imputation_backend %||% "mice",
    interval_minutes = settings()$imputation_interval_minutes %||% 5L,
    arima_threshold = settings()$imputation_arima_threshold %||% 0.05,
    arima_min_history = settings()$imputation_arima_min_history %||% 20L,
    xgb_rounds = settings()$imputation_xgb_rounds %||% 300L
  )
  apply_imputed_glucose(data(), result)
}
