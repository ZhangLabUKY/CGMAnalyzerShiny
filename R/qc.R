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
#' @noRd
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

qc_review_notes <- function(qc_summary, low_coverage_percent = 80) {
  if (!is.data.frame(qc_summary) || !nrow(qc_summary)) {
    return(character())
  }

  vapply(seq_len(nrow(qc_summary)), function(i) {
    row <- qc_summary[i, , drop = FALSE]
    notes <- character()
    if (!is.na(row$missing_glucose) && row$missing_glucose > 0L) {
      notes <- c(notes, paste(row$missing_glucose, "missing glucose row(s)"))
    }
    if (!is.na(row$gap_count) && row$gap_count > 0L) {
      notes <- c(notes, paste(row$gap_count, "timestamp gap(s)"))
    }
    if (!is.na(row$duplicate_timestamps) && row$duplicate_timestamps > 0L) {
      notes <- c(notes, paste(row$duplicate_timestamps, "duplicate timestamp(s)"))
    }
    if (!is.na(row$implausible_values) && row$implausible_values > 0L) {
      notes <- c(notes, paste(row$implausible_values, "glucose value(s) outside review range"))
    }
    if (!is.na(row$valid_days) && row$valid_days == 0L) {
      notes <- c(notes, "No valid CGM days")
    }
    if (
      !is.na(row$wear_time_percent) &&
        row$wear_time_percent < low_coverage_percent
    ) {
      notes <- c(notes, paste0("Coverage below ", low_coverage_percent, "%"))
    }
    if (length(notes)) {
      paste(notes, collapse = "; ")
    } else {
      "No review flags"
    }
  }, character(1))
}

add_qc_review_columns <- function(qc_summary, low_coverage_percent = 80) {
  out <- as.data.frame(qc_summary, stringsAsFactors = FALSE)
  if (!nrow(out)) {
    out$`QC status` <- character()
    out$`Review notes` <- character()
    return(out)
  }
  notes <- qc_review_notes(out, low_coverage_percent = low_coverage_percent)
  out$`QC status` <- ifelse(notes == "No review flags", "OK", "Review")
  out$`Review notes` <- notes

  front <- intersect(c("id", "QC status", "Review notes"), names(out))
  out[, c(front, setdiff(names(out), front)), drop = FALSE]
}

qc_display_column_labels <- function() {
  c(
    readings = "Readings",
    first_timestamp = "First timestamp",
    last_timestamp = "Last timestamp",
    observed_days = "Observed days",
    valid_days = "Valid days",
    median_interval_minutes = "Median interval (minutes)",
    wear_time_percent = "Coverage (%)",
    missing_glucose = "Missing glucose",
    duplicate_timestamps = "Duplicate timestamps",
    implausible_values = "Implausible glucose values",
    gap_count = "Timestamp gaps",
    max_gap_minutes = "Max gap (minutes)"
  )
}

apply_qc_display_labels <- function(qc_display) {
  labels <- qc_display_column_labels()
  matched <- intersect(names(qc_display), names(labels))
  names(qc_display)[match(matched, names(qc_display))] <- unname(labels[matched])
  qc_display
}

prepare_qc_display <- function(qc_summary, source_data = NULL, show_subject_id = NULL) {
  out <- add_qc_review_columns(qc_summary)
  if (!nrow(out)) {
    return(apply_qc_display_labels(out))
  }
  if ("id" %in% names(out)) {
    if (show_subject_id_for_display(source_data %||% out, show_subject_id)) {
      names(out)[names(out) == "id"] <- "Subject ID"
    } else {
      out$id <- NULL
    }
  }
  apply_qc_display_labels(out)
}

duplicate_timestamp_note <- function(qc_summary, source_data = NULL, show_subject_id = NULL) {
  if (
    !is.data.frame(qc_summary) ||
      !nrow(qc_summary) ||
      !"duplicate_timestamps" %in% names(qc_summary)
  ) {
    return(NULL)
  }

  duplicate_counts <- qc_summary$duplicate_timestamps
  duplicate_counts[is.na(duplicate_counts)] <- 0L
  total <- sum(duplicate_counts)
  if (total <= 0L) {
    return(NULL)
  }

  affected_subjects <- sum(duplicate_counts > 0L)
  show_subject_id <- show_subject_id_for_display(source_data %||% qc_summary, show_subject_id)
  message <- if (show_subject_id) {
    paste(
      format_count(total),
      "duplicate timestamp(s) detected across",
      format_count(affected_subjects),
      "Subject ID(s). Review duplicate timestamps before final analysis if they are unexpected."
    )
  } else {
    paste(
      format_count(total),
      "duplicate timestamp(s) detected. Review duplicate timestamps before final analysis if they are unexpected."
    )
  }

  list(
    total_duplicate_timestamps = total,
    affected_subjects = affected_subjects,
    message = message
  )
}
