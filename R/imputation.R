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

imputation_candidate_summary <- function(data, interval_minutes = 5L) {
  if (!is.data.frame(data) || !all(c("id", "timestamp", "glucose") %in% names(data))) {
    return(data.frame(
      explicit_missing_glucose = 0L,
      inferred_timestamp_gap_rows = 0L,
      missing_candidates = 0L,
      expanded_rows = 0L,
      missing_rate = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  grid_summary <- missingness_grid_summary_by_id(data, interval_minutes = interval_minutes)
  expanded_rows <- sum(grid_summary$expanded_rows, na.rm = TRUE)
  explicit_missing <- sum(grid_summary$explicit_missing_glucose, na.rm = TRUE)
  inferred_gaps <- sum(grid_summary$estimated_missing_readings, na.rm = TRUE)
  missing_candidates <- sum(grid_summary$missing_glucose, na.rm = TRUE)
  data.frame(
    explicit_missing_glucose = explicit_missing,
    inferred_timestamp_gap_rows = inferred_gaps,
    missing_candidates = missing_candidates,
    expanded_rows = expanded_rows,
    missing_rate = if (expanded_rows > 0L) missing_candidates / expanded_rows else NA_real_,
    stringsAsFactors = FALSE
  )
}

has_imputation_candidates <- function(data, interval_minutes = 5L) {
  summary <- imputation_candidate_summary(data, interval_minutes = interval_minutes)
  isTRUE(summary$missing_candidates[[1L]] > 0L)
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
  candidate_summary <- imputation_candidate_summary(data, interval_minutes = interval_minutes)
  missing_candidates <- candidate_summary$missing_candidates[[1L]]
  if (missing_candidates == 0L) {
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
    result$missing_rate <- if ("glucose" %in% names(result)) {
      mean(is.na(result$glucose))
    } else {
      candidate_summary$missing_rate[[1L]]
    }
  }
  result
}

row_id_lookup <- function(data) {
  seq_len(nrow(data))
}

subject_index_lookup <- function(data) {
  subject_levels <- unique(as.character(data$id))
  stats::setNames(subject_levels, seq_along(subject_levels))
}

append_inserted_imputation_rows <- function(out, data, imputed, row_id) {
  inserted <- is.na(row_id)
  if (!any(inserted)) {
    return(out)
  }
  inserted_rows <- imputed[inserted, , drop = FALSE]
  if (!all(c("subject_index", "time", "imputed_glucose_value") %in% names(inserted_rows))) {
    return(out)
  }

  subject_lookup <- subject_index_lookup(data)
  subject_id <- unname(subject_lookup[as.character(as.integer(inserted_rows$subject_index))])
  timestamp <- parse_cgm_timestamp(inserted_rows$time, tz = "UTC")
  keep <- !is.na(subject_id) & is_finite_cgm_timestamp(timestamp)
  if (!any(keep)) {
    return(out)
  }
  inserted_rows <- inserted_rows[keep, , drop = FALSE]
  subject_id <- subject_id[keep]
  timestamp <- timestamp[keep]
  fill_values <- as.numeric(inserted_rows$imputed_glucose_value)

  template <- out[rep(NA_integer_, length(subject_id)), , drop = FALSE]
  template$id <- subject_id
  template$timestamp <- timestamp
  template$glucose <- ifelse(is.finite(fill_values), fill_values, NA_real_)
  template$imputed_flag <- is.finite(fill_values)
  template$missing_source <- missing_source_gap()
  template$inserted_timestamp_gap <- TRUE
  template$explicit_missing_glucose <- FALSE

  for (i in seq_len(nrow(template))) {
    subject_rows <- data[as.character(data$id) == subject_id[[i]], , drop = FALSE]
    template[i, ] <- fill_unique_subject_metadata(template[i, , drop = FALSE], subject_rows)
  }
  if ("units" %in% names(template)) {
    template$units[is.na(template$units)] <- "mg/dL"
  }

  combined <- rbind(out, template)
  combined <- combined[order(combined$id, combined$timestamp), , drop = FALSE]
  row.names(combined) <- NULL
  combined
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

  out <- ensure_missingness_source_columns(data)
  row_id <- as.integer(imputed[[row_id_col]])
  valid <- !is.na(row_id) & row_id >= 1L & row_id <= nrow(out)
  matched_imputed <- imputed[valid, , drop = FALSE]
  matched_row_id <- row_id[valid]

  originally_missing <- is.na(out$glucose[matched_row_id])
  fill_rows <- matched_row_id[originally_missing]
  fill_values <- as.numeric(matched_imputed$imputed_glucose_value[originally_missing])
  fill_ok <- is.finite(fill_values)

  if (any(fill_ok)) {
    out$glucose[fill_rows[fill_ok]] <- fill_values[fill_ok]
    out$imputed_flag[fill_rows[fill_ok]] <- TRUE
  }

  out <- append_inserted_imputation_rows(out, data, imputed, row_id)

  if ("imputation_method" %in% names(imputed)) {
    attr(out, "imputation_method") <- first_non_missing(unique(as.character(imputed$imputation_method)), NA_character_)
  }
  if ("missing_rate" %in% names(imputed)) {
    attr(out, "imputation_missing_rate") <- first_non_missing(unique(as.numeric(imputed$missing_rate)), NA_real_)
  }
  out
}

apply_imputation_settings <- function(data, settings) {
  method <- settings$imputation_method %||% "none"
  if (!identical(method, "mice_only")) {
    return(data)
  }
  interval_minutes <- settings$imputation_interval_minutes %||% 5L
  if (!isTRUE(settings$imputation_available) || !has_imputation_candidates(data, interval_minutes = interval_minutes)) {
    return(data)
  }

  tryCatch(
    {
      result <- run_cgmissingdata_imputation(
        data,
        seed = settings$imputation_seed %||% 42,
        backend = settings$imputation_backend %||% "mice",
        interval_minutes = interval_minutes,
        arima_threshold = settings$imputation_arima_threshold %||% 0.05,
        arima_min_history = settings$imputation_arima_min_history %||% 20L,
        xgb_rounds = settings$imputation_xgb_rounds %||% 300L
      )
      apply_imputed_glucose(data, result)
    },
    error = function(error) {
      attr(data, "imputation_error") <- conditionMessage(error)
      data
    }
  )
}

analysis_data_from_settings <- function(data, settings) {
  apply_imputation_settings(data(), settings())
}
