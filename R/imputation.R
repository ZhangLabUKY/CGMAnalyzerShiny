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

cgmissingdata_version <- function() {
  tryCatch(
    utils::packageVersion("CGMissingDataR"),
    error = function(error) numeric_version("0.0.0")
  )
}

#' Check whether CGMissingDataR imputation is available
#'
#' @return Logical.
#' @noRd
cgmissingdata_available <- function() {
  !is.null(cgmissingdata_function()) && cgmissingdata_version() >= numeric_version("0.0.2")
}

first_non_missing <- function(x, default = NA) {
  x <- x[!is.na(x)]
  if (!length(x)) default else x[[1L]]
}

imputation_metadata_columns <- function(data) {
  excluded <- c(
    "id", "id_source", "timestamp", "glucose", "units", "device", "source_file",
    "imputed_flag", "missing_source", "inserted_timestamp_gap", "explicit_missing_glucose",
    ".original_order"
  )
  candidates <- setdiff(names(data), excluded)
  candidates <- candidates[!grepl("^\\.", candidates)]
  candidates[vapply(
    candidates,
    function(col) {
      values <- data[[col]]
      any(!is.na(values) & nzchar(trimws(as.character(values))))
    },
    logical(1)
  )]
}

coerce_imputation_feature <- function(x) {
  values <- trimws(as.character(x))
  values[!nzchar(values)] <- NA_character_
  numeric_values <- suppressWarnings(as.numeric(values))
  non_missing <- !is.na(values)
  if (any(non_missing) && all(!is.na(numeric_values[non_missing]))) {
    return(numeric_values)
  }
  values
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
  for (col in imputation_metadata_columns(data)) {
    out[[col]] <- coerce_imputation_feature(data[[col]])
  }
  out
}

cgmissingdata_imputation_models <- function() {
  c("auto", "arima", "xgboost", "rf", "knn", "lightgbm")
}

normalize_cgmissingdata_model <- function(model = "auto") {
  if (length(model) == 0L || is.na(model[[1L]]) || !nzchar(as.character(model[[1L]]))) {
    model <- "auto"
  }
  model <- tolower(trimws(as.character(model[[1L]] %||% "auto")))
  if (!model %in% cgmissingdata_imputation_models()) {
    return("auto")
  }
  model
}

format_cgmissingdata_model <- function(model, missing_rate = NA_real_, arima_threshold = 0.05) {
  model <- normalize_cgmissingdata_model(model)
  if (identical(model, "auto")) {
    model <- if (is.finite(missing_rate) && missing_rate <= arima_threshold) "arima" else "xgboost"
  }
  labels <- c(
    auto = "Auto",
    arima = "MICE+ARIMA",
    xgboost = "MICE+XGBoost",
    rf = "MICE+RF",
    knn = "MICE+kNN",
    lightgbm = "MICE+LightGBM"
  )
  unname(labels[[model]] %||% labels[["auto"]])
}

cgmissingdata_feature_cols <- function(imputation_input) {
  candidates <- setdiff(names(imputation_input), c(".row_id", "subject_index", "time", "glucose"))
  candidates[vapply(
    candidates,
    function(col) {
      values <- imputation_input[[col]]
      any(!is.na(values) & nzchar(trimws(as.character(values))))
    },
    logical(1)
  )]
}

call_cgmissingdata_imputation <- function(fun, args) {
  supported <- names(formals(fun))
  if ("..." %in% supported) {
    return(do.call(fun, args))
  }
  do.call(fun, args[intersect(names(args), supported)])
}

imputation_candidate_summary <- function(data, interval_minutes = 5L, precomputed = NULL) {
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
  grid_summary <- missingness_grid_summary_by_id(
    data,
    interval_minutes = interval_minutes,
    precomputed = precomputed
  )
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

has_imputation_candidates <- function(data, interval_minutes = 5L, precomputed = NULL) {
  summary <- imputation_candidate_summary(data, interval_minutes = interval_minutes, precomputed = precomputed)
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
  model = "auto",
  interval_minutes = 5L,
  missing_warning_threshold = 0.20,
  arima_threshold = 0.05,
  arima_order = c(4L, 1L, 0L),
  arima_min_history = 20L,
  xgb_rounds = 300L,
  rf_trees = 200L,
  knn_k = 7L,
  lgb_rounds = 400L,
  lag_values = c(1L, 2L, 3L),
  add_rollmean = TRUE,
  roll_window = 3L,
  study_start = NULL,
  study_end = NULL,
  precomputed = NULL,
  candidate_summary = NULL
) {
  fun <- cgmissingdata_function()
  if (is.null(fun)) {
    stop(
      "CGMissingDataR::run_missing_glucose_imputation() is not available. Install CGMissingDataR 0.0.2 or newer.",
      call. = FALSE
    )
  }
  if (cgmissingdata_version() < numeric_version("0.0.2")) {
    stop("CGMissingDataR 0.0.2 or newer is required for app imputation.", call. = FALSE)
  }
  if (!all(c("id", "timestamp", "glucose") %in% names(data))) {
    stop("Imputation requires standardized columns: id, timestamp, glucose.", call. = FALSE)
  }
  model <- normalize_cgmissingdata_model(model)
  candidate_summary <- candidate_summary %||% cgm_timed(
    "analysis_data_imputation_candidate_summary",
    imputation_candidate_summary(data, interval_minutes = interval_minutes, precomputed = precomputed)
  )
  missing_candidates <- candidate_summary$missing_candidates[[1L]]
  if (missing_candidates == 0L) {
    stop("No missing glucose values are available for imputation.", call. = FALSE)
  }

  imputation_input <- cgm_timed(
    "analysis_data_imputation_prepare_input",
    prepare_cgmissingdata_input(data)
  )
  feature_cols <- cgmissingdata_feature_cols(imputation_input)
  warning_messages <- character()
  call_args <- list(
    data = imputation_input,
    target_col = "glucose",
    feature_cols = feature_cols,
    id_col = "subject_index",
    time_col = "time",
    models = model,
    seed = seed,
    interval_minutes = interval_minutes,
    missing_warning_threshold = missing_warning_threshold,
    study_start = study_start,
    study_end = study_end,
    use_arima_if_missing_leq = arima_threshold,
    arima_order = arima_order,
    arima_min_history = arima_min_history,
    xgb_nrounds = xgb_rounds,
    rf_n_estimators = rf_trees,
    knn_k = knn_k,
    lgb_nrounds = lgb_rounds,
    n_threads = 1L,
    lag_k = lag_values,
    add_rollmean = add_rollmean,
    roll_window = roll_window,
    imputer_backend = backend,
    prefer_cgmanalyzer_equal_interval = FALSE,
    export = FALSE
  )
  result <- cgm_timed(
    "analysis_data_imputation_external_call",
    withCallingHandlers(
      call_cgmissingdata_imputation(fun, call_args),
      warning = function(warning) {
        warning_messages <<- c(warning_messages, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    ),
    context = list(backend = backend, model = model)
  )
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (!"imputation_method" %in% names(result)) {
    result$imputation_method <- format_cgmissingdata_model(
      model,
      missing_rate = candidate_summary$missing_rate[[1L]],
      arima_threshold = arima_threshold
    )
  }
  if (!"missing_rate" %in% names(result)) {
    result$missing_rate <- if ("glucose" %in% names(result)) {
      mean(is.na(result$glucose))
    } else {
      candidate_summary$missing_rate[[1L]]
    }
  }
  attr(result, "cgmissingdata_model") <- model
  attr(result, "cgmissingdata_backend") <- backend
  attr(result, "cgmissingdata_warning_threshold") <- missing_warning_threshold
  attr(result, "cgmissingdata_warnings") <- unique(warning_messages)
  result
}

row_id_lookup <- function(data) {
  seq_len(nrow(data))
}

subject_index_lookup <- function(data) {
  subject_levels <- unique(as.character(data$id))
  stats::setNames(subject_levels, seq_along(subject_levels))
}

subject_metadata_lookup <- function(data, template) {
  metadata_cols <- setdiff(
    names(template),
    c(
      "id", "timestamp", "glucose", "imputed_flag", "missing_source",
      "inserted_timestamp_gap", "explicit_missing_glucose", ".original_order"
    )
  )
  metadata_cols <- intersect(metadata_cols, names(data))
  if (!length(metadata_cols) || !nrow(template)) {
    return(template)
  }

  subject_dt <- data.table::as.data.table(data[, c("id", metadata_cols), drop = FALSE])
  template_id <- as.character(template$id)
  for (col in metadata_cols) {
    meta <- subject_dt[
      !is.na(get(col)),
      .(value = get(col)[1L], unique_values = data.table::uniqueN(get(col))),
      by = id
    ][unique_values == 1L]
    if (!nrow(meta)) {
      next
    }
    template[[col]] <- meta$value[match(template_id, as.character(meta$id))]
  }
  template
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
  template <- subject_metadata_lookup(data, template)
  if ("units" %in% names(template)) {
    template$units[is.na(template$units)] <- "mg/dL"
  }

  combined <- data.table::rbindlist(
    list(data.table::as.data.table(out), data.table::as.data.table(template)),
    fill = TRUE
  )
  data.table::setorder(combined, id, timestamp)
  as.data.frame(combined)
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
  attr(out, "imputation_model") <- attr(imputation_result, "cgmissingdata_model", exact = TRUE) %||% NA_character_
  attr(out, "imputation_backend") <- attr(imputation_result, "cgmissingdata_backend", exact = TRUE) %||% NA_character_
  attr(out, "imputation_warning_threshold") <- attr(imputation_result, "cgmissingdata_warning_threshold", exact = TRUE) %||% NA_real_
  attr(out, "imputation_warnings") <- attr(imputation_result, "cgmissingdata_warnings", exact = TRUE) %||% character()
  out
}

apply_imputation_settings <- function(data, settings, precomputed = NULL) {
  method <- settings$imputation_method %||% "none"
  if (!identical(method, "mice_only")) {
    return(data)
  }
  interval_minutes <- settings$imputation_interval_minutes %||% 5L
  candidate_summary <- cgm_timed(
    "analysis_data_imputation_candidate_summary",
    imputation_candidate_summary(data, interval_minutes = interval_minutes, precomputed = precomputed),
    context = list(method = method)
  )
  if (!isTRUE(settings$imputation_available) || !isTRUE(candidate_summary$missing_candidates[[1L]] > 0L)) {
    return(data)
  }

  cgm_timed(
    "analysis_data_imputation_total",
    tryCatch(
      {
      result <- run_cgmissingdata_imputation(
        data,
        seed = settings$imputation_seed %||% 42,
        backend = settings$imputation_backend %||% "mice",
        model = settings$imputation_model %||% "auto",
        interval_minutes = interval_minutes,
        missing_warning_threshold = settings$imputation_missing_warning_threshold %||% 0.20,
        arima_threshold = settings$imputation_arima_threshold %||% 0.05,
        arima_order = settings$imputation_arima_order %||% c(4L, 1L, 0L),
        arima_min_history = settings$imputation_arima_min_history %||% 20L,
        xgb_rounds = settings$imputation_xgb_rounds %||% 300L,
        rf_trees = settings$imputation_rf_trees %||% 200L,
        knn_k = settings$imputation_knn_k %||% 7L,
        lgb_rounds = settings$imputation_lgb_rounds %||% 400L,
        lag_values = settings$imputation_lag_values %||% c(1L, 2L, 3L),
        add_rollmean = settings$imputation_add_rollmean %||% TRUE,
        roll_window = settings$imputation_roll_window %||% 3L,
        study_start = settings$imputation_study_start,
        study_end = settings$imputation_study_end,
        precomputed = precomputed,
        candidate_summary = candidate_summary
      )
        cgm_timed(
          "analysis_data_imputation_apply_result",
          apply_imputed_glucose(data, result)
        )
      },
      error = function(error) {
        attr(data, "imputation_error") <- conditionMessage(error)
        data
      }
    ),
    context = list(method = method, model = settings$imputation_model %||% "auto", backend = settings$imputation_backend %||% "mice")
  )
}

analysis_data_from_settings <- function(data, settings) {
  apply_imputation_settings(data(), settings())
}
