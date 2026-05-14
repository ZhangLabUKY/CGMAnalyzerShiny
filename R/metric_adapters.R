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
  merge(base, metric, by = ".adapter_group", all.x = TRUE, sort = FALSE)
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

#' Compute selected CGManalyzer-backed metrics
#'
#' @param data Standardized CGM data.
#' @param by Grouping columns.
#' @param conga_hours Lag in hours for CONGA.
#' @param max_gap_intervals Maximum gap to interpolate during regularization.
#'
#' @return A data frame with CGManalyzer metric columns.
#' @noRd
compute_cgmanalyzer_metrics <- function(
  data,
  by = default_metric_groups(data),
  conga_hours = 2,
  max_gap_intervals = 4
) {
  by <- by[by %in% names(data)]
  if (!length(by)) {
    by <- "id"
  }

  split_data <- split_by_columns(data, by)
  installed <- requireNamespace("CGManalyzer", quietly = TRUE)

  out <- do.call(rbind, lapply(split_data, function(x) {
    group_values <- adapter_group_values(x, by)
    if (!installed) {
      return(data.frame(
        group_values,
        conga_2h = NA_real_,
        modd = NA_real_,
        cgmanalyzer_status = "not_installed",
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    regular <- regularize_cgm_series(x, max_gap_intervals = max_gap_intervals)
    interval <- attr(regular, "interval_minutes")
    y <- regular$glucose
    enough_conga <- !is.na(interval) && sum(!is.na(y)) > (60 / interval * conga_hours)
    enough_modd <- !is.na(interval) && length(y) >= (2 * 24 * 60 / interval)

    data.frame(
      group_values,
      conga_2h = if (enough_conga) {
        safe_adapter_number(CGManalyzer::CONGA.fn(y, Interval = interval, n = conga_hours))
      } else {
        NA_real_
      },
      modd = if (enough_modd) {
        safe_adapter_number(CGManalyzer::MODD.fn(y, Interval = interval / 60))
      } else {
        NA_real_
      },
      cgmanalyzer_status = if (!is.na(interval)) "ok" else "insufficient_data",
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

  installed <- requireNamespace("iglu", quietly = TRUE)
  group_map <- unique(data[, by, drop = FALSE])
  group_map$.adapter_group <- do.call(paste, c(group_map[by], sep = "\r"))

  if (!installed) {
    return(data.frame(
      group_map[, by, drop = FALSE],
      lbgi = NA_real_,
      hbgi = NA_real_,
      j_index = NA_real_,
      mage = NA_real_,
      iglu_status = "not_installed",
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  lookup <- group_map[, c(by, ".adapter_group"), drop = FALSE]
  data_with_group <- merge(data, lookup, by = by, all.x = TRUE, sort = FALSE)
  iglu_data <- data.frame(
    id = as.character(data_with_group$.adapter_group),
    time = data_with_group$timestamp,
    gl = as.numeric(data_with_group$glucose),
    stringsAsFactors = FALSE
  )
  iglu_data <- iglu_data[!is.na(iglu_data$id) & !is.na(iglu_data$time) & !is.na(iglu_data$gl), , drop = FALSE]

  out <- group_map
  out <- merge_iglu_metric(out, extract_iglu_metric_table(try(iglu::lbgi(iglu_data), silent = TRUE), "LBGI", "lbgi"))
  out <- merge_iglu_metric(out, extract_iglu_metric_table(try(iglu::hbgi(iglu_data), silent = TRUE), "HBGI", "hbgi"))
  out <- merge_iglu_metric(out, extract_iglu_metric_table(try(iglu::j_index(iglu_data), silent = TRUE), "J_index", "j_index"))
  out <- merge_iglu_metric(out, extract_iglu_metric_table(
    try(suppressWarnings(iglu::mage(iglu_data)), silent = TRUE),
    "MAGE",
    "mage"
  ))
  out$iglu_status <- if (nrow(iglu_data)) "ok" else "insufficient_data"
  out$.adapter_group <- NULL
  row.names(out) <- NULL
  out
}

merge_metric_adapter <- function(base, adapter, by) {
  if (is.null(adapter) || !nrow(adapter)) {
    return(base)
  }
  merge(base, adapter, by = by, all.x = TRUE, sort = FALSE)
}
