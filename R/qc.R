median_sampling_interval <- function(timestamp) {
  timestamp <- sort(timestamp[!is.na(timestamp)])
  if (length(timestamp) < 2L) {
    return(NA_real_)
  }
  diffs <- as.numeric(diff(timestamp), units = "mins")
  diffs <- diffs[diffs > 0]
  if (length(diffs) == 0L) {
    return(NA_real_)
  }
  stats::median(diffs, na.rm = TRUE)
}

summarize_one_id_qc <- function(x, valid_day_hours, glucose_min, glucose_max) {
  timestamps <- sort(x$timestamp[!is.na(x$timestamp)])
  interval <- median_sampling_interval(timestamps)
  diffs <- if (length(timestamps) >= 2L) as.numeric(diff(timestamps), units = "mins") else numeric()
  gap_limit <- if (is.na(interval)) NA_real_ else interval * 1.5

  expected <- NA_real_
  if (length(timestamps) >= 2L && !is.na(interval) && interval > 0) {
    expected <- floor(as.numeric(max(timestamps) - min(timestamps), units = "mins") / interval) + 1
  }

  wear_time_percent <- if (!is.na(expected) && expected > 0) {
    100 * sum(!is.na(x$glucose)) / expected
  } else {
    NA_real_
  }

  valid_days <- NA_integer_
  if (!is.na(interval)) {
    day_counts <- table(as.Date(x$timestamp[!is.na(x$timestamp)]))
    day_hours <- as.numeric(day_counts) * interval / 60
    valid_days <- sum(day_hours >= valid_day_hours)
  }

  data.frame(
    id = x$id[[1L]],
    readings = nrow(x),
    first_timestamp = if (length(timestamps)) min(timestamps) else as.POSIXct(NA),
    last_timestamp = if (length(timestamps)) max(timestamps) else as.POSIXct(NA),
    observed_days = length(unique(as.Date(timestamps))),
    valid_days = valid_days,
    median_interval_minutes = interval,
    wear_time_percent = wear_time_percent,
    missing_glucose = sum(is.na(x$glucose)),
    duplicate_timestamps = sum(duplicated(x$timestamp[!is.na(x$timestamp)])),
    implausible_values = sum(!is.na(x$glucose) & (x$glucose < glucose_min | x$glucose > glucose_max)),
    gap_count = if (!is.na(gap_limit)) sum(diffs > gap_limit, na.rm = TRUE) else NA_integer_,
    max_gap_minutes = if (length(diffs)) max(diffs, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

summarize_qc_dt <- function(data, valid_day_hours, glucose_min, glucose_max) {
  dt <- data.table::as.data.table(data)
  result <- dt[, {
    timestamp_values <- get("timestamp")
    glucose_values <- get("glucose")
    timestamps <- sort(timestamp_values[!is.na(timestamp_values)])
    interval <- median_sampling_interval(timestamps)
    diffs <- if (length(timestamps) >= 2L) as.numeric(diff(timestamps), units = "mins") else numeric()
    gap_limit <- if (is.na(interval)) NA_real_ else interval * 1.5

    expected <- NA_real_
    if (length(timestamps) >= 2L && !is.na(interval) && interval > 0) {
      expected <- floor(as.numeric(max(timestamps) - min(timestamps), units = "mins") / interval) + 1
    }

    wear_time_percent <- if (!is.na(expected) && expected > 0) {
      100 * sum(!is.na(glucose_values)) / expected
    } else {
      NA_real_
    }

    valid_days <- NA_integer_
    if (!is.na(interval)) {
      day_counts <- table(as.Date(timestamp_values[!is.na(timestamp_values)]))
      day_hours <- as.numeric(day_counts) * interval / 60
      valid_days <- sum(day_hours >= valid_day_hours)
    }

    list(
      readings = length(glucose_values),
      first_timestamp = if (length(timestamps)) min(timestamps) else as.POSIXct(NA),
      last_timestamp = if (length(timestamps)) max(timestamps) else as.POSIXct(NA),
      observed_days = length(unique(as.Date(timestamps))),
      valid_days = valid_days,
      median_interval_minutes = interval,
      wear_time_percent = wear_time_percent,
      missing_glucose = sum(is.na(glucose_values)),
      duplicate_timestamps = sum(duplicated(timestamp_values[!is.na(timestamp_values)])),
      implausible_values = sum(!is.na(glucose_values) & (glucose_values < glucose_min | glucose_values > glucose_max)),
      gap_count = if (!is.na(gap_limit)) sum(diffs > gap_limit, na.rm = TRUE) else NA_integer_,
      max_gap_minutes = if (length(diffs)) max(diffs, na.rm = TRUE) else NA_real_
    )
  }, by = id]

  as.data.frame(result, stringsAsFactors = FALSE)
}

#' Compute CGM quality-control summary
#'
#' @param data Standardized CGM data.
#' @param valid_day_hours Minimum observed hours for a valid CGM day.
#' @param glucose_min Minimum plausible glucose value in mg/dL.
#' @param glucose_max Maximum plausible glucose value in mg/dL.
#'
#' @return Data frame with one QC row per participant.
#' @export
compute_qc_summary <- function(
  data,
  valid_day_hours = 14,
  glucose_min = 40,
  glucose_max = 400
) {
  if (!all(c("id", "timestamp", "glucose") %in% names(data))) {
    stop("QC requires standardized columns: id, timestamp, glucose.", call. = FALSE)
  }

  out <- summarize_qc_dt(
    data,
    valid_day_hours = valid_day_hours,
    glucose_min = glucose_min,
    glucose_max = glucose_max
  )
  row.names(out) <- NULL
  out
}
