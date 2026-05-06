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
#' @export
cgmissingdata_available <- function() {
  !is.null(cgmissingdata_function())
}

prepare_cgmissingdata_input <- function(data) {
  out <- data.frame(
    .row_id = seq_len(nrow(data)),
    id = as.character(data$id),
    time = format_cgm_timestamp_iso(data$timestamp, tz = "UTC"),
    glucose = as.numeric(data$glucose),
    stringsAsFactors = FALSE
  )
  out
}

#' Run CGMissingDataR missing glucose imputation
#'
#' @param data Standardized CGM data.
#' @param model Imputation model. Defaults to `mice_only`.
#' @param seed Integer seed.
#'
#' @return Output from `CGMissingDataR::run_missing_glucose_imputation()`.
#' @export
run_cgmissingdata_imputation <- function(data, model = "mice_only", seed = 42) {
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
  fun(
    imputation_input,
    target_col = "glucose",
    feature_cols = character(0),
    id_col = "id",
    time_col = "time",
    models = model,
    seed = seed
  )
}

select_imputed_table <- function(imputation_result, model = "mice_only") {
  if (is.null(imputation_result$imputed_data)) {
    stop("CGMissingDataR output did not include imputed_data.", call. = FALSE)
  }
  if (!model %in% names(imputation_result$imputed_data)) {
    stop("CGMissingDataR output did not include model: ", model, call. = FALSE)
  }
  as.data.frame(imputation_result$imputed_data[[model]], stringsAsFactors = FALSE)
}

#' Apply imputed glucose values to standardized CGM data
#'
#' @param data Standardized CGM data.
#' @param imputation_result Result from `run_cgmissingdata_imputation()`.
#' @param model Imputation model name.
#'
#' @return Standardized CGM data with imputed glucose values and flags.
#' @export
apply_imputed_glucose <- function(data, imputation_result, model = "mice_only") {
  imputed <- select_imputed_table(imputation_result, model = model)
  row_id_col <- if (".RowID" %in% names(imputed)) ".RowID" else ".row_id"
  needed <- c(row_id_col, ".Missing", "ImputedValue")
  missing_cols <- setdiff(needed, names(imputed))
  if (length(missing_cols)) {
    stop("CGMissingDataR imputed data missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  out <- data
  row_id <- as.integer(imputed[[row_id_col]])
  valid <- !is.na(row_id) & row_id >= 1L & row_id <= nrow(out)
  imputed <- imputed[valid, , drop = FALSE]
  row_id <- row_id[valid]

  originally_missing <- is.na(out$glucose[row_id]) & (imputed$.Missing %in% TRUE)
  fill_rows <- row_id[originally_missing]
  fill_values <- as.numeric(imputed$ImputedValue[originally_missing])
  fill_ok <- !is.na(fill_values)

  if (any(fill_ok)) {
    out$glucose[fill_rows[fill_ok]] <- fill_values[fill_ok]
    out$imputed_flag[fill_rows[fill_ok]] <- TRUE
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
    model = settings()$imputation_model %||% "mice_only",
    seed = settings()$imputation_seed %||% 42
  )
  apply_imputed_glucose(data(), result, model = settings()$imputation_model %||% "mice_only")
}
