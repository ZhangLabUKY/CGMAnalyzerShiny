#' Convert standardized CGM data for iglu
#'
#' @param data Standardized CGM data.
#'
#' @return A data frame with `id`, `time`, and `gl` columns.
#' @noRd
to_iglu_data <- function(data) {
  if (!all(c("id", "timestamp", "glucose") %in% names(data))) {
    stop("iglu conversion requires standardized columns: id, timestamp, glucose.", call. = FALSE)
  }

  out <- data.frame(
    id = as.character(data$id),
    time = data$timestamp,
    gl = as.numeric(data$glucose),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$id) & !is.na(out$time) & !is.na(out$gl), , drop = FALSE]
  out <- out[order(out$id, out$time), , drop = FALSE]
  row.names(out) <- NULL
  out
}

safe_adapter_number <- function(expr) {
  value <- tryCatch(suppressWarnings(expr), error = function(e) NA_real_)
  value <- as.numeric(value)
  if (length(value) == 0L) NA_real_ else value[[1L]]
}

extract_adapter_value <- function(result, column) {
  if (inherits(result, "try-error") || is.null(result) || !column %in% names(result)) {
    return(NA_real_)
  }
  value <- as.numeric(result[[column]])
  if (length(value) == 0L) NA_real_ else value[[1L]]
}

extract_iglu_metric_table <- function(result, value_column, output_column) {
  if (inherits(result, "try-error") || is.null(result) || !"id" %in% names(result) || !value_column %in% names(result)) {
    return(data.frame(.adapter_group = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    .adapter_group = as.character(result$id),
    value = as.numeric(result[[value_column]]),
    stringsAsFactors = FALSE
  ) |>
    stats::setNames(c(".adapter_group", output_column))
}

merge_iglu_metric <- function(base, metric) {
  if (!nrow(metric)) {
    return(base)
  }
  out <- data.table::as.data.table(base)
  metric_dt <- data.table::as.data.table(metric)
  value_cols <- setdiff(names(metric_dt), ".adapter_group")
  out[metric_dt, (value_cols) := mget(paste0("i.", value_cols)), on = ".adapter_group"]
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Regularize one CGM time series
#'
#' @param data Standardized CGM data for one participant/group.
#' @param interval_minutes Target interval. Defaults to the median observed
#'   interval.
#' @param max_gap_intervals Maximum gap, in intervals, eligible for linear
#'   interpolation.
#'
#' @return A data frame with regular timestamps and glucose values. The selected
#'   interval is stored in the `interval_minutes` attribute.
#' @noRd
regularize_cgm_series <- function(data, interval_minutes = NULL, max_gap_intervals = 4) {
  if (!all(c("timestamp", "glucose") %in% names(data))) {
    stop("Regularization requires timestamp and glucose columns.", call. = FALSE)
  }

  x <- data[!is.na(data$timestamp) & !is.na(data$glucose), c("timestamp", "glucose"), drop = FALSE]
  x <- x[order(x$timestamp), , drop = FALSE]
  if (!nrow(x)) {
    out <- data.frame(timestamp = as.POSIXct(character()), glucose = numeric())
    attr(out, "interval_minutes") <- NA_real_
    return(out)
  }

  x <- stats::aggregate(glucose ~ timestamp, data = x, FUN = mean)
  x <- x[order(x$timestamp), , drop = FALSE]

  if (is.null(interval_minutes) || is.na(interval_minutes)) {
    interval_minutes <- median_sampling_interval(x$timestamp)
  }

  if (is.na(interval_minutes) || interval_minutes <= 0 || nrow(x) < 2L) {
    attr(x, "interval_minutes") <- interval_minutes
    row.names(x) <- NULL
    return(x)
  }

  grid <- seq(min(x$timestamp), max(x$timestamp), by = interval_minutes * 60)
  out <- data.frame(timestamp = grid, glucose = NA_real_)
  grid_numeric <- as.numeric(out$timestamp)
  obs_numeric <- as.numeric(x$timestamp)
  max_gap_seconds <- interval_minutes * 60 * max_gap_intervals

  for (i in seq_len(nrow(x) - 1L)) {
    left <- obs_numeric[[i]]
    right <- obs_numeric[[i + 1L]]
    if (!is.na(left) && !is.na(right) && right >= left && (right - left) <= max_gap_seconds) {
      idx <- grid_numeric >= left & grid_numeric <= right
      out$glucose[idx] <- stats::approx(
        x = c(left, right),
        y = c(x$glucose[[i]], x$glucose[[i + 1L]]),
        xout = grid_numeric[idx],
        ties = "ordered"
      )$y
    }
  }

  exact_idx <- match(obs_numeric, grid_numeric)
  exact_idx <- exact_idx[!is.na(exact_idx)]
  out$glucose[exact_idx] <- x$glucose[seq_along(exact_idx)]

  attr(out, "interval_minutes") <- interval_minutes
  row.names(out) <- NULL
  out
}

adapter_group_values <- function(x, group_columns) {
  x[1L, group_columns, drop = FALSE]
}

internal_optional_metric_values <- function(glucose, interval_minutes = NA_real_) {
  metric_names <- c("conga_12h", "conga_24h", "modd", "lbgi", "hbgi", "j_index", "mage")
  empty <- stats::setNames(as.list(rep(NA_real_, length(metric_names))), metric_names)
  values <- tryCatch(
    optional_metrics_cpp(as.numeric(glucose), as.numeric(interval_minutes %||% NA_real_)[[1L]]),
    error = function(error) NULL
  )
  if (is.null(values)) {
    return(empty)
  }
  for (name in metric_names) {
    value <- suppressWarnings(as.numeric(values[[name]]))
    empty[[name]] <- if (length(value)) value[[1L]] else NA_real_
  }
  empty
}

internal_lag_metric_values <- function(timestamp, glucose, interval_minutes = NA_real_) {
  metric_names <- c("conga_12h", "conga_24h", "modd")
  empty <- stats::setNames(as.list(rep(NA_real_, length(metric_names))), metric_names)
  timestamp_numeric <- as.numeric(timestamp)
  interval_seconds <- as.numeric(interval_minutes %||% NA_real_) * 60
  finite_times <- sort(timestamp_numeric[is.finite(timestamp_numeric)])
  regular_series <- FALSE
  if (is.finite(interval_seconds) && interval_seconds > 0 && length(finite_times) >= 2L) {
    diffs <- diff(finite_times)
    diffs <- diffs[is.finite(diffs) & diffs > 0]
    regular_series <- length(diffs) > 0L &&
      max(abs(diffs - interval_seconds), na.rm = TRUE) <= max(1, interval_seconds * 1e-6)
  }
  tolerance_seconds <- if (isTRUE(regular_series)) 0 else NA_real_
  values <- tryCatch(
    optional_lag_metrics_by_time_cpp(timestamp_numeric, as.numeric(glucose), tolerance_seconds),
    error = function(error) NULL
  )
  if (is.null(values)) {
    return(empty)
  }
  for (name in metric_names) {
    value <- suppressWarnings(as.numeric(values[[name]]))
    empty[[name]] <- if (length(value)) value[[1L]] else NA_real_
  }
  empty
}

#' Compute selected internal lag metrics
#'
#' @param data Standardized CGM data.
#' @param by Grouping columns.
#' @param max_gap_intervals Compatibility argument retained for callers that
#'   previously controlled regularization. Optional lag metrics now use observed
#'   timestamp pairs from the current analysis data without expanding a grid.
#'
#' @return A data frame with optional lag metric columns.
#' @noRd
compute_cgmanalyzer_metrics <- function(
  data,
  by = default_metric_groups(data),
  max_gap_intervals = 4
) {
  by <- by[by %in% names(data)]
  if (!length(by)) {
    by <- "id"
  }

  split_data <- split_by_columns(data, by)

  out <- do.call(rbind, lapply(split_data, function(x) {
    group_values <- adapter_group_values(x, by)
    y <- x[!is.na(x$timestamp) & !is.na(x$glucose), c("timestamp", "glucose"), drop = FALSE]
    y <- y[order(y$timestamp), , drop = FALSE]
    interval <- median_sampling_interval(y$timestamp)
    metrics <- internal_lag_metric_values(y$timestamp, y$glucose, interval)

    data.frame(
      group_values,
      conga_12h = metrics$conga_12h,
      conga_24h = metrics$conga_24h,
      modd = metrics$modd,
      cgmanalyzer_status = if (!is.na(interval) && any(is.finite(y$glucose))) "internal_observed_pairs" else "insufficient_data",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))

  row.names(out) <- NULL
  out
}

#' Compute selected iglu-backed metrics
#'
#' @param data Standardized CGM data.
#' @param by Grouping columns.
#'
#' @return A data frame with iglu metric columns.
#' @noRd
compute_iglu_metrics <- function(data, by = default_metric_groups(data)) {
  by <- by[by %in% names(data)]
  if (!length(by)) {
    by <- "id"
  }

  split_data <- split_by_columns(data, by)
  out <- do.call(rbind, lapply(split_data, function(x) {
    group_values <- adapter_group_values(x, by)
    y <- x[!is.na(x$timestamp) & !is.na(x$glucose), c("timestamp", "glucose"), drop = FALSE]
    y <- y[order(y$timestamp), , drop = FALSE]
    metrics <- internal_optional_metric_values(y$glucose, NA_real_)
    return(data.frame(
      group_values,
      lbgi = metrics$lbgi,
      hbgi = metrics$hbgi,
      j_index = metrics$j_index,
      mage = metrics$mage,
      iglu_status = if (any(is.finite(y$glucose))) "internal" else "insufficient_data",
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }))
  row.names(out) <- NULL
  out
}

merge_metric_adapter <- function(base, adapter, by) {
  if (is.null(adapter) || !nrow(adapter)) {
    return(base)
  }
  out <- data.table::as.data.table(base)
  adapter_dt <- data.table::as.data.table(adapter)
  value_cols <- setdiff(names(adapter_dt), by)
  out[adapter_dt, (value_cols) := mget(paste0("i.", value_cols)), on = by]
  as.data.frame(out, stringsAsFactors = FALSE)
}
