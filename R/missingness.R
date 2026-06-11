empty_gap_periods <- function() {
  data.frame(
    id = character(),
    gap_start = as.POSIXct(character()),
    gap_end = as.POSIXct(character()),
    gap_minutes = numeric(),
    expected_interval_minutes = numeric(),
    estimated_missing_readings = integer(),
    stringsAsFactors = FALSE
  )
}

normalize_interval_minutes <- function(interval_minutes = 5L) {
  interval_minutes <- suppressWarnings(as.numeric(interval_minutes[[1L]] %||% 5L))
  if (is.na(interval_minutes) || interval_minutes <= 0) {
    return(5)
  }
  interval_minutes
}

missing_source_observed <- function() "observed"
missing_source_explicit <- function() "explicit_na"
missing_source_gap <- function() "timestamp_gap"

ensure_missingness_source_columns <- function(data) {
  out <- data
  if (!"missing_source" %in% names(out)) {
    out$missing_source <- if ("glucose" %in% names(out)) {
      ifelse(is.na(out$glucose), missing_source_explicit(), missing_source_observed())
    } else {
      missing_source_observed()
    }
  } else {
    out$missing_source <- as.character(out$missing_source)
    if ("glucose" %in% names(out)) {
      out$missing_source[is.na(out$missing_source) & is.na(out$glucose)] <- missing_source_explicit()
      out$missing_source[is.na(out$missing_source) & !is.na(out$glucose)] <- missing_source_observed()
    }
    out$missing_source[is.na(out$missing_source)] <- missing_source_observed()
  }

  if (!"inserted_timestamp_gap" %in% names(out)) {
    out$inserted_timestamp_gap <- out$missing_source == missing_source_gap()
  } else {
    out$inserted_timestamp_gap <- out$inserted_timestamp_gap %in% TRUE |
      out$missing_source == missing_source_gap()
  }

  if (!"explicit_missing_glucose" %in% names(out)) {
    out$explicit_missing_glucose <- if ("glucose" %in% names(out)) {
      is.na(out$glucose) & !out$inserted_timestamp_gap
    } else {
      FALSE
    }
  } else {
    out$explicit_missing_glucose <- out$explicit_missing_glucose %in% TRUE
  }
  out$missing_source[out$inserted_timestamp_gap] <- missing_source_gap()
  if ("glucose" %in% names(out)) {
    explicit <- is.na(out$glucose) & !out$inserted_timestamp_gap
    out$explicit_missing_glucose[explicit] <- TRUE
    out$missing_source[explicit] <- missing_source_explicit()
  }
  if (!"imputed_flag" %in% names(out)) {
    out$imputed_flag <- FALSE
  }
  out
}

fill_unique_subject_metadata <- function(rows, subject_rows) {
  metadata_cols <- setdiff(
    names(rows),
    c(
      "id", "timestamp", "glucose", "imputed_flag", "missing_source",
      "inserted_timestamp_gap", "explicit_missing_glucose", ".original_order"
    )
  )
  for (col in metadata_cols) {
    values <- subject_rows[[col]]
    values <- values[!is.na(values)]
    unique_values <- unique(values)
    if (length(unique_values) == 1L) {
      rows[[col]] <- unique_values[[1L]]
    }
  }
  rows
}

regularize_subject_timestamp_grid <- function(subject_data, interval_minutes) {
  subject_data <- subject_data[order(subject_data$timestamp, subject_data$.original_order), , drop = FALSE]
  start_time <- min(subject_data$timestamp, na.rm = TRUE)
  minute_offset <- as.numeric(difftime(subject_data$timestamp, start_time, units = "mins"))
  subject_data$timestamp <- start_time + round(minute_offset / interval_minutes) * interval_minutes * 60
  subject_data <- subject_data[
    order(subject_data$timestamp, is.na(subject_data$glucose), subject_data$.original_order),
    ,
    drop = FALSE
  ]
  subject_data <- subject_data[!duplicated(subject_data$timestamp), , drop = FALSE]

  grid <- data.frame(
    id = subject_data$id[[1L]],
    timestamp = seq(min(subject_data$timestamp), max(subject_data$timestamp), by = paste(interval_minutes, "mins")),
    stringsAsFactors = FALSE
  )
  expanded_dt <- data.table::as.data.table(subject_data)[
    data.table::as.data.table(grid),
    on = .(id, timestamp)
  ]
  expanded <- as.data.frame(expanded_dt, stringsAsFactors = FALSE)
  inserted <- is.na(expanded$.original_order)
  expanded$inserted_timestamp_gap <- expanded$inserted_timestamp_gap %in% TRUE
  expanded$explicit_missing_glucose <- expanded$explicit_missing_glucose %in% TRUE
  expanded$missing_source <- as.character(expanded$missing_source)

  if (any(inserted)) {
    expanded$inserted_timestamp_gap[inserted] <- TRUE
    expanded$explicit_missing_glucose[inserted] <- FALSE
    expanded$missing_source[inserted] <- missing_source_gap()
    expanded$glucose[inserted] <- NA_real_
    if ("imputed_flag" %in% names(expanded)) {
      expanded$imputed_flag[inserted] <- FALSE
    }
    expanded[inserted, ] <- fill_unique_subject_metadata(expanded[inserted, , drop = FALSE], subject_data)
  }

  explicit <- is.na(expanded$glucose) & !expanded$inserted_timestamp_gap
  expanded$explicit_missing_glucose[explicit] <- TRUE
  expanded$missing_source[explicit] <- missing_source_explicit()
  expanded$missing_source[is.na(expanded$missing_source)] <- missing_source_observed()
  if ("units" %in% names(expanded)) {
    expanded$units[is.na(expanded$units)] <- "mg/dL"
  }
  expanded
}

#' Regularize standardized CGM data to the expected timestamp grid
#'
#' @param data Standardized CGM data.
#' @param interval_minutes Expected sampling interval in minutes.
#'
#' @return Standardized CGM data with inserted timestamp-gap rows.
#' @noRd
regularize_cgm_timestamp_grid <- function(data, interval_minutes = 5L) {
  if (!all(c("id", "timestamp", "glucose") %in% names(data))) {
    stop("Grid missingness requires standardized columns: id, timestamp, glucose.", call. = FALSE)
  }
  interval_minutes <- normalize_interval_minutes(interval_minutes)
  out <- ensure_missingness_source_columns(data)
  out$.original_order <- seq_len(nrow(out))
  finite <- !is.na(out$id) & nzchar(as.character(out$id)) & is_finite_cgm_timestamp(out$timestamp)
  if (!any(finite)) {
    out$.original_order <- NULL
    return(out)
  }

  dt <- data.table::as.data.table(out[finite, , drop = FALSE])
  dt[, id := as.character(id)]
  data.table::setorder(dt, id, timestamp, .original_order)
  dt[, .anchor_timestamp := min(timestamp, na.rm = TRUE), by = id]
  dt[, timestamp := .anchor_timestamp +
    round(as.numeric(difftime(timestamp, .anchor_timestamp, units = "mins")) / interval_minutes) *
      interval_minutes * 60]
  dt[, .anchor_timestamp := NULL]
  data.table::setorder(dt, id, timestamp, na.last = TRUE)
  dt[, .missing_sort := is.na(glucose)]
  data.table::setorder(dt, id, timestamp, .missing_sort, .original_order)
  dt[, .missing_sort := NULL]
  dt <- dt[!duplicated(dt, by = c("id", "timestamp"))]

  grid <- dt[, list(
    timestamp = seq(min(timestamp), max(timestamp), by = interval_minutes * 60)
  ), by = id]
  expanded_dt <- dt[grid, on = .(id, timestamp)]
  inserted <- is.na(expanded_dt$.original_order)
  expanded_dt[, inserted_timestamp_gap := inserted_timestamp_gap %in% TRUE]
  expanded_dt[, explicit_missing_glucose := explicit_missing_glucose %in% TRUE]
  expanded_dt[, missing_source := as.character(missing_source)]

  if (any(inserted)) {
    expanded_dt[inserted, inserted_timestamp_gap := TRUE]
    expanded_dt[inserted, explicit_missing_glucose := FALSE]
    expanded_dt[inserted, missing_source := missing_source_gap()]
    expanded_dt[inserted, glucose := NA_real_]
    if ("imputed_flag" %in% names(expanded_dt)) {
      expanded_dt[inserted, imputed_flag := FALSE]
    }

    metadata_cols <- setdiff(
      names(expanded_dt),
      c(
        "id", "timestamp", "glucose", "imputed_flag", "missing_source",
        "inserted_timestamp_gap", "explicit_missing_glucose", ".original_order"
      )
    )
    for (col in metadata_cols) {
      lookup <- dt[!is.na(get(col)), {
        values <- unique(get(col))
        list(.fill_value = if (length(values) == 1L) values[[1L]] else NA)
      }, by = id]
      if (nrow(lookup)) {
        fill_values <- lookup$.fill_value[match(expanded_dt$id, lookup$id)]
        fill <- inserted & is.na(expanded_dt[[col]]) & !is.na(fill_values)
        if (any(fill)) {
          data.table::set(expanded_dt, which(fill), col, fill_values[fill])
        }
      }
    }
  }

  explicit <- is.na(expanded_dt$glucose) & !(expanded_dt$inserted_timestamp_gap %in% TRUE)
  expanded_dt[explicit, explicit_missing_glucose := TRUE]
  expanded_dt[explicit, missing_source := missing_source_explicit()]
  expanded_dt[is.na(missing_source), missing_source := missing_source_observed()]
  if ("units" %in% names(expanded_dt)) {
    expanded_dt[is.na(units), units := "mg/dL"]
  }
  remainder <- out[!finite, , drop = FALSE]
  combined <- data.table::rbindlist(
    list(expanded_dt, data.table::as.data.table(remainder)),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setorder(combined, id, timestamp, .original_order)
  combined[, .original_order := NULL]
  data.table::setDF(combined)
  row.names(combined) <- NULL
  combined
}

empty_missingness_grid_summary <- function() {
  data.frame(
    id = character(),
    expanded_rows = integer(),
    explicit_missing_glucose = integer(),
    estimated_missing_readings = integer(),
    missing_glucose = integer(),
    missing_glucose_rate = numeric(),
    stringsAsFactors = FALSE
  )
}

missingness_grid_summary_from_expanded <- function(expanded) {
  if (!nrow(expanded)) {
    return(empty_missingness_grid_summary())
  }
  dt <- data.table::as.data.table(expanded)
  out <- dt[, {
    explicit <- (get("missing_source") == missing_source_explicit()) | (get("explicit_missing_glucose") %in% TRUE)
    gaps <- (get("missing_source") == missing_source_gap()) | (get("inserted_timestamp_gap") %in% TRUE)
    missing <- is.na(get("glucose"))
    list(
      expanded_rows = .N,
      explicit_missing_glucose = sum(explicit, na.rm = TRUE),
      estimated_missing_readings = sum(gaps, na.rm = TRUE),
      missing_glucose = sum(missing, na.rm = TRUE),
      missing_glucose_rate = if (.N > 0L) round(100 * sum(missing, na.rm = TRUE) / .N, 2) else NA_real_
    )
  }, by = id]
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out
}

fast_missingness_grid_summary_by_id <- function(data, interval_minutes = 5L) {
  if (!is.data.frame(data) || !nrow(data) || !all(c("id", "timestamp", "glucose") %in% names(data))) {
    return(empty_missingness_grid_summary())
  }
  interval_minutes <- normalize_interval_minutes(interval_minutes)
  out <- ensure_missingness_source_columns(data)
  out$.original_order <- seq_len(nrow(out))
  dt <- data.table::as.data.table(out)
  dt[, id := as.character(id)]
  finite <- !is.na(dt$id) & nzchar(dt$id) & is_finite_cgm_timestamp(dt$timestamp)

  summarize_retained <- function(x) {
    if (!nrow(x)) {
      return(empty_missingness_grid_summary())
    }
    x[, {
      explicit <- (get("missing_source") == missing_source_explicit()) |
        (get("explicit_missing_glucose") %in% TRUE)
      existing_gaps <- (get("missing_source") == missing_source_gap()) |
        (get("inserted_timestamp_gap") %in% TRUE)
      missing <- is.na(get("glucose"))
      list(
        expanded_rows = .N,
        explicit_missing_glucose = sum(explicit, na.rm = TRUE),
        estimated_missing_readings = sum(existing_gaps, na.rm = TRUE),
        missing_glucose = sum(missing, na.rm = TRUE),
        missing_glucose_rate = if (.N > 0L) round(100 * sum(missing, na.rm = TRUE) / .N, 2) else NA_real_
      )
    }, by = id]
  }

  finite_summary <- empty_missingness_grid_summary()
  if (any(finite)) {
    finite_dt <- data.table::copy(dt[finite])
    data.table::setorder(finite_dt, id, timestamp, .original_order)
    finite_dt[, .anchor_timestamp := min(timestamp, na.rm = TRUE), by = id]
    finite_dt[, timestamp := .anchor_timestamp +
      round(as.numeric(difftime(timestamp, .anchor_timestamp, units = "mins")) / interval_minutes) *
        interval_minutes * 60]
    finite_dt[, .anchor_timestamp := NULL]
    finite_dt[, .missing_sort := is.na(glucose)]
    data.table::setorder(finite_dt, id, timestamp, .missing_sort, .original_order)
    finite_dt[, .missing_sort := NULL]
    finite_dt <- finite_dt[!duplicated(finite_dt, by = c("id", "timestamp"))]
    data.table::setorder(finite_dt, id, timestamp, .original_order)
    finite_dt[, .previous_timestamp := data.table::shift(timestamp), by = id]
    finite_dt[, .inferred_gap_readings := data.table::fifelse(
      is.na(.previous_timestamp),
      0L,
      pmax(
        as.integer(round(as.numeric(difftime(timestamp, .previous_timestamp, units = "mins")) / interval_minutes)) - 1L,
        0L
      )
    )]
    finite_summary <- finite_dt[, {
      explicit <- (get("missing_source") == missing_source_explicit()) |
        (get("explicit_missing_glucose") %in% TRUE)
      existing_gaps <- (get("missing_source") == missing_source_gap()) |
        (get("inserted_timestamp_gap") %in% TRUE)
      missing <- is.na(get("glucose"))
      inferred <- sum(.inferred_gap_readings, na.rm = TRUE)
      missing_count <- sum(missing, na.rm = TRUE) + inferred
      expanded_count <- .N + inferred
      list(
        expanded_rows = expanded_count,
        explicit_missing_glucose = sum(explicit, na.rm = TRUE),
        estimated_missing_readings = sum(existing_gaps, na.rm = TRUE) + inferred,
        missing_glucose = missing_count,
        missing_glucose_rate = if (expanded_count > 0L) round(100 * missing_count / expanded_count, 2) else NA_real_
      )
    }, by = id]
  }

  remainder_summary <- summarize_retained(dt[!finite])
  combined <- data.table::rbindlist(
    list(data.table::as.data.table(finite_summary), data.table::as.data.table(remainder_summary)),
    use.names = TRUE,
    fill = TRUE
  )
  if (!nrow(combined)) {
    return(empty_missingness_grid_summary())
  }
  combined <- combined[, {
    expanded <- sum(expanded_rows, na.rm = TRUE)
    missing <- sum(missing_glucose, na.rm = TRUE)
    list(
      expanded_rows = expanded,
      explicit_missing_glucose = sum(explicit_missing_glucose, na.rm = TRUE),
      estimated_missing_readings = sum(estimated_missing_readings, na.rm = TRUE),
      missing_glucose = missing,
      missing_glucose_rate = if (expanded > 0L) round(100 * missing / expanded, 2) else NA_real_
    )
  }, by = id]
  out <- as.data.frame(combined, stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out
}

gap_periods_from_expanded <- function(expanded, interval_minutes = 5L) {
  interval_minutes <- normalize_interval_minutes(interval_minutes)
  gap_rows <- expanded[
    expanded$inserted_timestamp_gap %in% TRUE | expanded$missing_source == missing_source_gap(),
    ,
    drop = FALSE
  ]
  if (!nrow(gap_rows)) {
    return(empty_gap_periods())
  }

  dt <- data.table::as.data.table(gap_rows)
  data.table::setorder(dt, id, timestamp)
  dt[, .previous_timestamp := data.table::shift(timestamp), by = id]
  dt[, gap_group := cumsum(
    is.na(.previous_timestamp) |
      as.numeric(difftime(timestamp, .previous_timestamp, units = "mins")) > interval_minutes
  ), by = id]
  dt[, .previous_timestamp := NULL]
  out <- dt[, list(
    gap_start = min(timestamp),
    gap_end = max(timestamp),
    gap_minutes = .N * interval_minutes,
    expected_interval_minutes = interval_minutes,
    estimated_missing_readings = .N
  ), by = .(id, gap_group)]
  out$gap_group <- NULL
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out
}

missingness_precompute <- function(data, interval_minutes = 5L) {
  interval_minutes <- normalize_interval_minutes(interval_minutes)
  expanded <- cgm_timed(
    "missingness_regularize_grid",
    regularize_cgm_timestamp_grid(data, interval_minutes = interval_minutes),
    rows = nrow(data)
  )
  grid_summary <- cgm_timed(
    "missingness_grid_summary",
    missingness_grid_summary_from_expanded(expanded),
    rows = nrow(expanded)
  )
  gaps <- cgm_timed(
    "missingness_gap_grouping",
    gap_periods_from_expanded(expanded, interval_minutes = interval_minutes),
    rows = nrow(expanded)
  )
  structure(
    list(
      interval_minutes = interval_minutes,
      expanded = expanded,
      grid_summary = grid_summary,
      gaps = gaps
    ),
    class = "cgm_missingness_precompute"
  )
}

missingness_precompute_or_new <- function(data, interval_minutes = 5L, precomputed = NULL) {
  if (
    inherits(precomputed, "cgm_missingness_precompute") &&
      identical(normalize_interval_minutes(interval_minutes), precomputed$interval_minutes)
  ) {
    return(precomputed)
  }
  missingness_precompute(data, interval_minutes = interval_minutes)
}

missingness_grid_summary_by_id <- function(data, interval_minutes = 5L, precomputed = NULL) {
  precomputed <- missingness_precompute_or_new(data, interval_minutes = interval_minutes, precomputed = precomputed)
  precomputed$grid_summary
}

missingness_grid_summary_for_review <- function(data, interval_minutes = 5L, precomputed = NULL) {
  if (
    is.data.frame(precomputed) &&
      all(names(empty_missingness_grid_summary()) %in% names(precomputed))
  ) {
    return(precomputed)
  }
  if (
    inherits(precomputed, "cgm_missingness_precompute") &&
      identical(normalize_interval_minutes(interval_minutes), precomputed$interval_minutes)
  ) {
    return(precomputed$grid_summary)
  }
  fast_missingness_grid_summary_by_id(data, interval_minutes = interval_minutes)
}

#' Detect timestamp gaps in standardized CGM data
#'
#' @param data Standardized CGM data.
#' @param interval_minutes Expected sampling interval in minutes.
#'
#' @return A data frame of detected gap periods.
#' @noRd
detect_gap_periods <- function(data, interval_minutes = 5L, precomputed = NULL) {
  if (!all(c("id", "timestamp", "glucose") %in% names(data))) {
    stop("Gap detection requires standardized columns: id, timestamp, and glucose.", call. = FALSE)
  }

  precomputed <- missingness_precompute_or_new(data, interval_minutes = interval_minutes, precomputed = precomputed)
  precomputed$gaps
}

#' Compute user-facing missingness summary
#'
#' @param data Standardized CGM data.
#' @param valid_day_hours Minimum observed hours for a valid CGM day.
#' @param interval_minutes Expected sampling interval in minutes.
#'
#' @return A data frame with per-participant missingness summaries.
#' @noRd
compute_missingness_summary <- function(data, valid_day_hours = 14, interval_minutes = 5L, precomputed = NULL) {
  qc <- compute_qc_summary(data, valid_day_hours = valid_day_hours)
  precomputed <- missingness_precompute_or_new(data, interval_minutes = interval_minutes, precomputed = precomputed)
  gaps <- precomputed$gaps
  grid_summary <- precomputed$grid_summary

  if (!nrow(qc)) {
    return(data.frame())
  }

  gap_counts <- table(gaps$id)
  if (nrow(gaps)) {
    estimated_missing <- stats::aggregate(estimated_missing_readings ~ id, data = gaps, FUN = sum)
  } else {
    estimated_missing <- data.frame(id = character(), estimated_missing_readings = integer())
  }

  out <- data.frame(
    id = qc$id,
    readings = qc$readings,
    expanded_rows = grid_summary$expanded_rows[match(qc$id, grid_summary$id)],
    missing_glucose = grid_summary$missing_glucose[match(qc$id, grid_summary$id)],
    missing_glucose_rate = grid_summary$missing_glucose_rate[match(qc$id, grid_summary$id)],
    explicit_missing_glucose = grid_summary$explicit_missing_glucose[match(qc$id, grid_summary$id)],
    gap_count = as.integer(gap_counts[qc$id]),
    max_gap_minutes = qc$max_gap_minutes,
    valid_days = qc$valid_days,
    observed_days = qc$observed_days,
    median_interval_minutes = qc$median_interval_minutes,
    duplicate_timestamps = qc$duplicate_timestamps,
    stringsAsFactors = FALSE
  )
  out$expanded_rows[is.na(out$expanded_rows)] <- out$readings[is.na(out$expanded_rows)]
  out$missing_glucose[is.na(out$missing_glucose)] <- qc$missing_glucose[is.na(out$missing_glucose)]
  out$explicit_missing_glucose[is.na(out$explicit_missing_glucose)] <- qc$missing_glucose[is.na(out$explicit_missing_glucose)]
  out$gap_count[is.na(out$gap_count)] <- 0L
  out$estimated_missing_readings <- estimated_missing$estimated_missing_readings[match(out$id, estimated_missing$id)]
  out$estimated_missing_readings[is.na(out$estimated_missing_readings)] <- 0L
  out
}

compute_missingness_heatmap_data <- function(
  data,
  gaps = NULL,
  interval_minutes = 5L,
  precomputed = NULL,
  show_subject_id = NULL
) {
  if (!nrow(data) || !any(is_finite_cgm_timestamp(data$timestamp))) {
    return(data.frame(
      id = character(),
      date = as.Date(character()),
      readings = integer(),
      missing_glucose = integer(),
      missing_glucose_rate = numeric(),
      timestamp_gaps = integer(),
      estimated_missing_readings = integer(),
      imputed_rows = integer(),
      tooltip = character(),
      stringsAsFactors = FALSE
    ))
  }

  observed <- data[is_finite_cgm_timestamp(data$timestamp), , drop = FALSE]
  observed <- ensure_missingness_source_columns(observed)
  observed_dt <- data.table::as.data.table(observed)
  observed_dt[, date := as.Date(timestamp)]
  daily <- observed_dt[, {
    glucose_values <- get("glucose")
    list(
      readings = length(glucose_values)
    )
  }, by = .(id, date)]

  precomputed <- missingness_precompute_or_new(data, interval_minutes = interval_minutes, precomputed = precomputed)
  expanded <- precomputed$expanded
  expanded <- expanded[is_finite_cgm_timestamp(expanded$timestamp), , drop = FALSE]
  expanded_dt <- data.table::as.data.table(expanded)
  expanded_dt[, date := as.Date(timestamp)]
  expanded_daily <- expanded_dt[, {
    glucose_values <- get("glucose")
    imputed_values <- get("imputed_flag")
    gap_values <- get("inserted_timestamp_gap") %in% TRUE | get("missing_source") == missing_source_gap()
    missing <- sum(is.na(glucose_values))
    expanded_rows <- length(glucose_values)
    list(
      expanded_rows = expanded_rows,
      missing_glucose = missing,
      missing_glucose_rate = if (expanded_rows > 0L) round(100 * missing / expanded_rows, 2) else NA_real_,
      imputed_rows = sum(imputed_values %in% TRUE, na.rm = TRUE),
      estimated_missing_readings = sum(gap_values, na.rm = TRUE)
    )
  }, by = .(id, date)]
  expanded_daily[daily, readings := i.readings, on = .(id, date)]
  daily <- expanded_daily
  daily[is.na(readings), readings := 0L]

  if (is.null(gaps)) {
    gaps <- precomputed$gaps
  }
  if (nrow(gaps)) {
    gap_dt <- data.table::as.data.table(gaps)
    gap_dt[, date := as.Date(gap_start)]
    gap_summary <- gap_dt[, list(
      timestamp_gaps = .N
    ), by = .(id, date)]
    daily[gap_summary, timestamp_gaps := i.timestamp_gaps, on = .(id, date)]
  } else {
    daily[, timestamp_gaps := 0L]
  }
  daily[is.na(timestamp_gaps), timestamp_gaps := 0L]
  daily[is.na(estimated_missing_readings), estimated_missing_readings := 0L]

  data.table::setorder(daily, id, date)
  subject_prefix <- if (show_subject_id_for_display(data, show_subject_id)) {
    paste0("Subject ID: ", daily$id, "<br>")
  } else {
    ""
  }
  daily$tooltip <- paste0(
    subject_prefix,
    "Date: ", daily$date,
    "<br>Readings: ", daily$readings,
    "<br>Missing glucose or inferred gap readings: ", daily$missing_glucose,
    "<br>Missing glucose rate: ", daily$missing_glucose_rate, "%",
    "<br>Timestamp gaps: ", daily$timestamp_gaps,
    "<br>Inferred missing readings from timestamp gaps: ", daily$estimated_missing_readings,
    "<br>Imputed rows: ", daily$imputed_rows
  )
  as.data.frame(daily, stringsAsFactors = FALSE)
}

coverage_status <- function(coverage_percent, readings) {
  out <- rep("No data", length(coverage_percent))
  has_data <- !is.na(readings) & readings > 0L
  out[has_data & coverage_percent < 50] <- "Low coverage (<50%)"
  out[has_data & coverage_percent >= 50 & coverage_percent < 80] <- "Partial coverage (50-79%)"
  out[has_data & coverage_percent >= 80] <- "High coverage (>=80%)"
  factor(
    out,
    levels = c(
      "No data",
      "Low coverage (<50%)",
      "Partial coverage (50-79%)",
      "High coverage (>=80%)"
    ),
    ordered = TRUE
  )
}

coverage_status_color <- function(status) {
  colors <- c(
    "No data" = "#D0D5DD",
    "Low coverage (<50%)" = "#D92D20",
    "Partial coverage (50-79%)" = "#FDB022",
    "High coverage (>=80%)" = "#2E90FA"
  )
  unname(colors[as.character(status)])
}

week_start_date <- function(date) {
  date <- as.Date(date)
  date - as.integer(strftime(date, "%u")) + 1L
}

weekday_label <- function(date) {
  factor(
    strftime(as.Date(date), "%a"),
    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
    ordered = TRUE
  )
}

empty_missingness_calendar_data <- function() {
  data.frame(
    id = character(),
    date = as.Date(character()),
    month = character(),
    week_start = as.Date(character()),
    week_index = integer(),
    weekday = weekday_label(as.Date(character())),
    weekday_index = integer(),
    plot_y = numeric(),
    readings = integer(),
    missing_glucose = integer(),
    missing_glucose_rate = numeric(),
    timestamp_gaps = integer(),
    estimated_missing_readings = integer(),
    imputed_rows = integer(),
    expected_readings = numeric(),
    coverage_percent = numeric(),
    coverage_status = coverage_status(numeric(), integer()),
    tooltip = character(),
    stringsAsFactors = FALSE
  )
}

daily_coverage_date_span <- function(data, date_range = NULL) {
  if (!is.null(date_range)) {
    range <- normalize_analysis_date_range(date_range, data)
    start <- as.Date(range[["start"]])
    end <- as.Date(range[["end"]])
    if (!is.na(start) && !is.na(end)) {
      return(c(start = start, end = end))
    }
  }

  if (!nrow(data) || !"timestamp" %in% names(data) || !any(is_finite_cgm_timestamp(data$timestamp))) {
    return(c(start = as.Date(NA), end = as.Date(NA)))
  }

  dates <- as.Date(data$timestamp[is_finite_cgm_timestamp(data$timestamp)])
  dates <- dates[!is.na(dates)]
  c(start = min(dates), end = max(dates))
}

subject_active_date_spans <- function(data, date_range = NULL) {
  if (!nrow(data) || !"id" %in% names(data) || !"timestamp" %in% names(data)) {
    return(data.frame(id = character(), start = as.Date(character()), end = as.Date(character())))
  }

  finite <- is_finite_cgm_timestamp(data$timestamp)
  if (!any(finite)) {
    return(data.frame(id = character(), start = as.Date(character()), end = as.Date(character())))
  }

  dt <- data.table::as.data.table(data[finite, c("id", "timestamp"), drop = FALSE])
  dt[, id := as.character(id)]
  dt <- dt[!is.na(id) & nzchar(id)]
  if (!nrow(dt)) {
    return(data.frame(id = character(), start = as.Date(character()), end = as.Date(character())))
  }

  dt[, date := as.Date(timestamp)]
  spans <- dt[, list(start = min(date, na.rm = TRUE), end = max(date, na.rm = TRUE)), by = id]

  if (!is.null(date_range)) {
    range <- normalize_analysis_date_range(date_range, data)
    range_start <- as.Date(range[["start"]])
    range_end <- as.Date(range[["end"]])
    if (!is.na(range_start)) {
      spans[, start := pmax(start, range_start)]
    }
    if (!is.na(range_end)) {
      spans[, end := pmin(end, range_end)]
    }
  }

  spans <- spans[!is.na(start) & !is.na(end) & start <= end]
  spans <- spans[order(id)]
  as.data.frame(spans, stringsAsFactors = FALSE)
}

expand_subject_active_dates <- function(spans) {
  if (!nrow(spans)) {
    return(data.frame(id = character(), date = as.Date(character()), stringsAsFactors = FALSE))
  }

  rows <- lapply(seq_len(nrow(spans)), function(i) {
    data.frame(
      id = spans$id[[i]],
      date = seq(spans$start[[i]], spans$end[[i]], by = "day"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

add_compact_calendar_positions <- function(daily, ids) {
  if (!nrow(daily)) {
    return(daily)
  }

  daily$month_start <- as.Date(format(daily$date, "%Y-%m-01"))
  daily$month <- format(daily$date, "%b %Y")
  daily$week_start <- week_start_date(daily$date)
  daily$weekday <- weekday_label(daily$date)
  daily$weekday_index <- match(as.character(daily$weekday), c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

  week_map <- unique(daily[, c("month_start", "week_start"), drop = FALSE])
  week_map <- week_map[order(week_map$month_start, week_map$week_start), , drop = FALSE]
  week_map$week_of_month <- stats::ave(seq_len(nrow(week_map)), week_map$month_start, FUN = seq_along)

  month_width <- stats::aggregate(week_of_month ~ month_start, data = week_map, FUN = max)
  month_width <- month_width[order(month_width$month_start), , drop = FALSE]
  month_width$month_index <- seq_len(nrow(month_width))
  month_width$month_offset <- c(0L, cumsum(utils::head(month_width$week_of_month + 1L, -1L)))

  daily_dt <- data.table::as.data.table(daily)
  week_map_dt <- data.table::as.data.table(week_map)
  month_width_dt <- data.table::as.data.table(month_width[, c("month_start", "month_index", "month_offset"), drop = FALSE])
  daily_dt[week_map_dt, week_of_month := i.week_of_month, on = .(month_start, week_start)]
  daily_dt[month_width_dt, `:=`(
    month_index = i.month_index,
    month_offset = i.month_offset
  ), on = .(month_start)]
  daily <- as.data.frame(daily_dt, stringsAsFactors = FALSE)
  daily$calendar_x <- daily$month_offset + daily$week_of_month
  daily$week_index <- daily$calendar_x

  id_index <- match(as.character(daily$id), ids)
  daily$plot_y <- (id_index - 1L) * 8L + daily$weekday_index
  daily <- daily[order(daily$id, daily$calendar_x, daily$weekday_index), , drop = FALSE]
  daily$month <- factor(daily$month, levels = unique(daily$month[order(daily$month_start)]), ordered = TRUE)
  daily
}

compute_missingness_calendar_data <- function(
  data,
  gaps = NULL,
  date_range = NULL,
  interval_minutes = 5L,
  precomputed = NULL,
  show_subject_id = NULL
) {
  interval_minutes <- normalize_interval_minutes(interval_minutes)
  precomputed <- if (!nrow(data) || !"id" %in% names(data)) {
    NULL
  } else {
    missingness_precompute_or_new(data, interval_minutes = interval_minutes, precomputed = precomputed)
  }
  daily <- compute_missingness_heatmap_data(
    data,
    gaps = gaps,
    interval_minutes = interval_minutes,
    precomputed = precomputed,
    show_subject_id = show_subject_id
  )
  if (!nrow(data) || !"id" %in% names(data)) {
    return(empty_missingness_calendar_data())
  }

  spans <- subject_active_date_spans(data, date_range = date_range)
  ids <- spans$id
  if (!length(ids)) {
    return(empty_missingness_calendar_data())
  }

  if (!nrow(daily)) {
    daily <- data.frame(
      id = character(),
      date = as.Date(character()),
      readings = integer(),
      missing_glucose = integer(),
      missing_glucose_rate = numeric(),
      timestamp_gaps = integer(),
      estimated_missing_readings = integer(),
      imputed_rows = integer(),
      tooltip = character(),
      stringsAsFactors = FALSE
    )
  }
  daily$date <- as.Date(daily$date)
  expected_lookup <- stats::setNames(
    rep(round(24 * 60 / interval_minutes), length(ids)),
    ids
  )
  complete <- expand_subject_active_dates(spans)
  complete$date <- as.Date(complete$date)
  daily <- as.data.frame(
    data.table::as.data.table(daily)[
      data.table::as.data.table(complete),
      on = .(id, date)
    ],
    stringsAsFactors = FALSE
  )
  daily$readings[is.na(daily$readings)] <- 0L
  daily$missing_glucose[is.na(daily$missing_glucose)] <- 0L
  daily$timestamp_gaps[is.na(daily$timestamp_gaps)] <- 0L
  daily$estimated_missing_readings[is.na(daily$estimated_missing_readings)] <- 0L
  daily$imputed_rows[is.na(daily$imputed_rows)] <- 0L
  daily$expected_readings <- as.numeric(expected_lookup[as.character(daily$id)])
  daily$coverage_percent <- ifelse(
    !is.na(daily$expected_readings) & daily$expected_readings > 0,
    pmin(100, round(100 * daily$readings / daily$expected_readings, 1)),
    NA_real_
  )
  daily$coverage_status <- coverage_status(daily$coverage_percent, daily$readings)
  daily$missing_glucose_rate[is.na(daily$missing_glucose_rate) & daily$readings == 0L] <- NA_real_
  subject_prefix <- if (show_subject_id_for_display(data, show_subject_id)) {
    paste0("Subject ID: ", daily$id, "<br>")
  } else {
    ""
  }
  daily$tooltip <- paste0(
    subject_prefix,
    "Date: ", daily$date,
    "<br>Status: ", daily$coverage_status,
    "<br>Readings: ", daily$readings,
    "<br>Expected readings: ", ifelse(is.na(daily$expected_readings), "unknown", daily$expected_readings),
    "<br>Coverage: ", ifelse(is.na(daily$coverage_percent), "unknown", paste0(daily$coverage_percent, "%")),
    "<br>Missing glucose or inferred gap readings: ", daily$missing_glucose,
    "<br>Missing glucose rate: ", ifelse(is.na(daily$missing_glucose_rate), "no readings", paste0(daily$missing_glucose_rate, "%")),
    "<br>Timestamp gaps: ", daily$timestamp_gaps,
    "<br>Inferred missing readings from timestamp gaps: ", daily$estimated_missing_readings,
    "<br>Imputed rows: ", daily$imputed_rows
  )
  add_compact_calendar_positions(daily, ids)
}

day_coverage_warning_summary <- function(calendar_data, data = NULL, show_subject_id = NULL) {
  if (!is.data.frame(calendar_data) || !nrow(calendar_data)) {
    return(data.frame(
      `Subject ID` = character(),
      `Full missing days` = integer(),
      `Half-day or worse coverage days` = integer(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  dt <- data.table::as.data.table(calendar_data)
  summary <- dt[, list(
    `Full missing days` = sum(!is.na(readings) & readings == 0L, na.rm = TRUE),
    `Half-day or worse coverage days` = sum(!is.na(readings) & readings > 0L & !is.na(coverage_percent) & coverage_percent < 50, na.rm = TRUE)
  ), by = id]
  data.table::setnames(summary, "id", "Subject ID")
  out <- as.data.frame(summary[order(`Subject ID`)], stringsAsFactors = FALSE)
  if (!is.null(data) && is.data.frame(data) && !show_subject_id_for_display(data, show_subject_id)) {
    out <- out[, setdiff(names(out), "Subject ID"), drop = FALSE]
  }
  row.names(out) <- NULL
  out
}

append_day_coverage_warnings <- function(missingness, calendar_data, data = NULL) {
  if (!is.data.frame(missingness) || !nrow(missingness)) {
    return(missingness)
  }
  warnings <- day_coverage_warning_summary(calendar_data, data = data)
  if (!nrow(warnings)) {
    missingness[["Full missing days"]] <- 0L
    missingness[["Half-day or worse coverage days"]] <- 0L
    return(missingness)
  }

  if ("Subject ID" %in% names(missingness) && "Subject ID" %in% names(warnings)) {
    out_dt <- data.table::as.data.table(missingness)
    warning_dt <- data.table::as.data.table(warnings)
    data.table::setnames(
      warning_dt,
      c("Full missing days", "Half-day or worse coverage days"),
      c(".full_missing_days", ".half_day_missing_days")
    )
    out_dt[warning_dt, `:=`(
      `Full missing days` = i..full_missing_days,
      `Half-day or worse coverage days` = i..half_day_missing_days
    ), on = "Subject ID"]
    out <- as.data.frame(out_dt, stringsAsFactors = FALSE)
  } else {
    out <- missingness
    out[["Full missing days"]] <- sum(warnings[["Full missing days"]], na.rm = TRUE)
    out[["Half-day or worse coverage days"]] <- sum(warnings[["Half-day or worse coverage days"]], na.rm = TRUE)
  }
  out[["Full missing days"]][is.na(out[["Full missing days"]])] <- 0L
  out[["Half-day or worse coverage days"]][is.na(out[["Half-day or worse coverage days"]])] <- 0L
  out
}

day_coverage_warning_note <- function(day_summary) {
  if (!is.data.frame(day_summary) || !nrow(day_summary)) {
    return("")
  }
  full_days <- sum(day_summary[["Full missing days"]], na.rm = TRUE)
  half_days <- sum(day_summary[["Half-day or worse coverage days"]], na.rm = TRUE)
  if (full_days <= 0L && half_days <= 0L) {
    return("")
  }
  paste(
    "Daily coverage review:",
    paste0(full_days, " full missing day(s)"),
    "and",
    paste0(half_days, " day(s) with less than half of expected readings were found in the current analysis data.")
  )
}

filter_missingness_calendar_participant <- function(
  calendar_data,
  participant = ""
) {
  filter_data_by_subject_selection(
    calendar_data,
    participant,
    id_col = "id"
  )
}

filled_glucose_by_id <- function(original_data, analysis_data, ids) {
  if (nrow(original_data) != nrow(analysis_data)) {
    filled <- analysis_data$imputed_flag %in% TRUE
    counts <- table(analysis_data$id[filled])
    out <- as.integer(counts[ids])
    out[is.na(out)] <- 0L
    return(stats::setNames(out, ids))
  }

  filled <- is.na(original_data$glucose) &
    !is.na(analysis_data$glucose) &
    (analysis_data$imputed_flag %in% TRUE)

  counts <- table(analysis_data$id[filled])
  out <- as.integer(counts[ids])
  out[is.na(out)] <- 0L
  stats::setNames(out, ids)
}

#' Compare original and analysis-data missingness
#'
#' @param original_data Standardized CGM data before optional imputation.
#' @param analysis_data Current analysis CGM data after preprocessing settings.
#' @param valid_day_hours Minimum observed hours for a valid CGM day.
#' @param include_preprocessing Whether to include before/after preprocessing columns.
#' @param interval_minutes Expected sampling interval in minutes.
#'
#' @return A compact participant-level before/after table.
#' @noRd
compare_missingness_summaries <- function(
  original_data,
  analysis_data,
  valid_day_hours = 14,
  include_preprocessing = FALSE,
  interval_minutes = 5L,
  original_precomputed = NULL,
  analysis_precomputed = NULL,
  show_subject_id = NULL
) {
  original <- compute_missingness_summary(
    original_data,
    valid_day_hours = valid_day_hours,
    interval_minutes = interval_minutes,
    precomputed = original_precomputed
  )
  analysis <- compute_missingness_summary(
    analysis_data,
    valid_day_hours = valid_day_hours,
    interval_minutes = interval_minutes,
    precomputed = analysis_precomputed
  )
  ids <- sort(unique(c(original$id, analysis$id)))

  if (!length(ids)) {
    return(data.frame())
  }

  original_match <- match(ids, original$id)
  analysis_match <- match(ids, analysis$id)
  filled <- filled_glucose_by_id(original_data, analysis_data, ids)

  if (isTRUE(include_preprocessing)) {
    out <- data.frame(
      `Subject ID` = ids,
      `Missing glucose rows before preprocessing` = original$missing_glucose[original_match],
      `Missing glucose (%) before preprocessing` = original$missing_glucose_rate[original_match],
      `Missing glucose rows after preprocessing` = analysis$missing_glucose[analysis_match],
      `Missing glucose (%) after preprocessing` = analysis$missing_glucose_rate[analysis_match],
      `Filled glucose rows` = as.integer(filled[ids]),
      `Timestamp gaps` = original$gap_count[original_match],
      `Estimated missing readings from gaps` = original$estimated_missing_readings[original_match],
      `Valid days` = analysis$valid_days[analysis_match],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    out <- data.frame(
      `Subject ID` = ids,
      `Missing glucose rows` = analysis$missing_glucose[analysis_match],
      `Missing glucose (%)` = analysis$missing_glucose_rate[analysis_match],
      `Timestamp gaps` = analysis$gap_count[analysis_match],
      `Estimated missing readings from gaps` = analysis$estimated_missing_readings[analysis_match],
      `Valid days` = analysis$valid_days[analysis_match],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  if (!show_subject_id_for_display(analysis_data, show_subject_id)) {
    out <- out[, setdiff(names(out), "Subject ID"), drop = FALSE]
  }
  out
}

should_show_analysis_missingness <- function(settings) {
  identical(settings$imputation_method %||% "none", "mice_only")
}

format_imputation_method <- function(method) {
  if (identical(method, "mice_only")) {
    "MICE imputation"
  } else {
    "None"
  }
}

format_imputation_backend <- function(backend) {
  if (identical(backend, "sklearn")) {
    "Python/sklearn"
  } else {
    "R/mice"
  }
}

format_imputation_model_choice <- function(model) {
  if (length(model) == 0L || is.na(model[[1L]]) || !nzchar(as.character(model[[1L]]))) {
    model <- "auto"
  }
  model <- tolower(trimws(as.character(model[[1L]] %||% "auto")))
  labels <- c(
    auto = "Auto",
    arima = "ARIMA",
    xgboost = "XGBoost",
    rf = "Random Forest",
    knn = "kNN",
    lightgbm = "LightGBM"
  )
  unname(labels[[model]] %||% labels[["auto"]])
}

normalize_imputation_method_label <- function(method) {
  method <- trimws(as.character(method))
  method <- gsub("\\+", " + ", method)
  method <- gsub("\\s+", " ", method)
  method
}

compact_imputation_method_labels <- function(method) {
  method <- normalize_imputation_method_label(method)
  if (length(method) > 1L && all(grepl("^MICE \\+ ", method))) {
    return(paste0("MICE + ", paste(sub("^MICE \\+ ", "", method), collapse = " / ")))
  }
  paste(method, collapse = " / ")
}

format_returned_imputation_method <- function(method, fallback = "MICE imputation") {
  method <- unique(trimws(as.character(method %||% character())))
  method <- method[nzchar(method) & !is.na(method)]
  if (!length(method)) {
    return(fallback)
  }
  method <- normalize_imputation_method_label(method)
  known_order <- c(
    "MICE + ARIMA",
    "MICE + XGBoost",
    "MICE + RF",
    "MICE + kNN",
    "MICE + LightGBM"
  )
  method <- c(intersect(known_order, method), setdiff(method, known_order))
  compact_imputation_method_labels(method)
}

imputation_method_details <- function(method_by_subject) {
  if (
    !is.data.frame(method_by_subject) ||
      !nrow(method_by_subject) ||
      !"Method" %in% names(method_by_subject)
  ) {
    return("")
  }
  method <- normalize_imputation_method_label(method_by_subject$Method)
  method <- method[nzchar(method) & !is.na(method)]
  if (!length(method)) {
    return("")
  }
  counts <- sort(table(method), decreasing = TRUE)
  labels <- sub("^MICE \\+ ", "", names(counts))
  paste(
    paste0(labels, ": ", as.integer(counts), " Subject ID(s)"),
    collapse = "; "
  )
}

imputation_numeric_status_value <- function(value, summary = c("first", "max")) {
  summary <- match.arg(summary)
  value <- suppressWarnings(as.numeric(value))
  value <- value[is.finite(value)]
  if (!length(value)) {
    return(NA_real_)
  }
  if (identical(summary, "max")) {
    max(value)
  } else {
    value[[1L]]
  }
}

#' Summarize imputation status for user-facing QC
#'
#' @param original_data Standardized CGM data before optional imputation.
#' @param analysis_data Current analysis CGM data after preprocessing settings.
#' @param settings Reproducibility settings list.
#'
#' @return A one-row data frame with imputation status details.
#' @noRd
summarize_imputation_status <- function(original_data, analysis_data, settings, original_precomputed = NULL, analysis_precomputed = NULL) {
  method <- settings$imputation_method %||% "none"
  available <- isTRUE(settings$imputation_available %||% cgmissingdata_available())
  seed <- settings$imputation_seed %||% NA_integer_
  interval_minutes <- settings$imputation_interval_minutes %||% 5L
  imputation_error <- attr(analysis_data, "imputation_error", exact = TRUE)
  imputation_pending <- attr(analysis_data, "imputation_pending", exact = TRUE)
  returned_method <- attr(analysis_data, "imputation_method", exact = TRUE)
  returned_method_by_subject <- attr(analysis_data, "imputation_method_by_subject", exact = TRUE)
  returned_missing_rate <- attr(analysis_data, "imputation_missing_rate", exact = TRUE)
  returned_model <- attr(analysis_data, "imputation_model", exact = TRUE) %||% settings$imputation_model %||% "auto"
  returned_warning_threshold <- attr(analysis_data, "imputation_warning_threshold", exact = TRUE) %||%
    settings$imputation_missing_warning_threshold %||% 0.20
  if (length(returned_model) == 0L || is.na(returned_model[[1L]]) || !nzchar(as.character(returned_model[[1L]]))) {
    returned_model <- settings$imputation_model %||% "auto"
  }
  if (!is.numeric(returned_warning_threshold) || !is.finite(returned_warning_threshold[[1L]])) {
    returned_warning_threshold <- settings$imputation_missing_warning_threshold %||% 0.20
  }
  returned_missing_rate_value <- imputation_numeric_status_value(returned_missing_rate, "max")
  returned_warning_threshold_value <- imputation_numeric_status_value(returned_warning_threshold, "first")
  imputation_warnings <- unique(as.character(attr(analysis_data, "imputation_warnings", exact = TRUE) %||% character()))
  imputation_warnings <- imputation_warnings[nzchar(imputation_warnings)]
  original_summary <- tryCatch(
    missingness_grid_summary_by_id(original_data, interval_minutes = interval_minutes, precomputed = original_precomputed),
    error = function(error) data.frame()
  )
  analysis_summary <- tryCatch(
    missingness_grid_summary_by_id(analysis_data, interval_minutes = interval_minutes, precomputed = analysis_precomputed),
    error = function(error) data.frame()
  )
  original_missing <- if (nrow(original_summary)) sum(original_summary$missing_glucose, na.rm = TRUE) else sum(is.na(original_data$glucose))
  analysis_missing <- if (nrow(analysis_summary)) sum(analysis_summary$missing_glucose, na.rm = TRUE) else sum(is.na(analysis_data$glucose))
  original_rows <- if (nrow(original_summary)) sum(original_summary$expanded_rows, na.rm = TRUE) else nrow(original_data)
  analysis_rows <- if (nrow(analysis_summary)) sum(analysis_summary$expanded_rows, na.rm = TRUE) else nrow(analysis_data)
  original_missing_percent <- if (original_rows > 0L) round(100 * original_missing / original_rows, 2) else NA_real_
  analysis_missing_percent <- if (analysis_rows > 0L) round(100 * analysis_missing / analysis_rows, 2) else NA_real_
  gaps <- tryCatch(
    detect_gap_periods(original_data, interval_minutes = interval_minutes, precomputed = original_precomputed),
    error = function(error) empty_gap_periods()
  )
  estimated_gap_readings <- if (is.data.frame(gaps) && "estimated_missing_readings" %in% names(gaps)) {
    sum(gaps$estimated_missing_readings, na.rm = TRUE)
  } else {
    NA_integer_
  }
  rows_filled <- sum(analysis_data$imputed_flag %in% TRUE, na.rm = TRUE)

  if (!identical(method, "mice_only")) {
    status <- "Not applied"
    message <- "Imputation is off. Analysis uses the original standardized data."
  } else if (!available) {
    status <- "Unavailable"
    message <- "Imputation is selected, but the selected imputation option is not available in this R session. Analysis uses the original standardized data."
  } else if (!is.null(imputation_error) && nzchar(imputation_error)) {
    status <- "Could not apply"
    message <- paste(
      "Imputation was selected, but analysis is using the original standardized data because imputation could not be completed.",
      "Details:",
      imputation_error
    )
  } else if (identical(imputation_pending, "not_run")) {
    status <- "Not run"
    message <- "Imputation is selected, but it has not been run for the current dataset. Analysis uses the original standardized data."
  } else if (identical(imputation_pending, "stale")) {
    status <- "Stale"
    message <- "Imputation settings or analysis data changed. Run imputation again to refresh imputed analysis data."
  } else if (original_missing == 0L) {
    status <- "Not needed"
    message <- "No explicit missing glucose rows or inferred timestamp-gap candidates were detected, so imputation was not needed. Analysis uses the original standardized data."
  } else if (rows_filled > 0L) {
    status <- "Applied"
    message <- paste("Imputation filled", rows_filled, "missing glucose row(s). Analysis uses imputed glucose values for filled rows.")
  } else {
    status <- "No rows filled"
    message <- "Imputation ran, but it returned no finite imputed glucose values. Analysis uses the original standardized data."
  }
  if (length(imputation_warnings)) {
    message <- paste(message, "Warnings:", paste(imputation_warnings, collapse = " | "))
  }

  data.frame(
    Method = if (identical(method, "mice_only")) {
      format_returned_imputation_method(returned_method, format_imputation_method(method))
    } else {
      format_imputation_method(method)
    },
    Model = if (identical(method, "mice_only")) format_imputation_model_choice(returned_model) else "",
    Status = status,
    Seed = seed,
    `Method details` = imputation_method_details(returned_method_by_subject),
    `Missing warning threshold (%)` = if (is.finite(returned_warning_threshold_value)) round(100 * returned_warning_threshold_value, 2) else NA_real_,
    `Missing rate used by imputation (%)` = if (is.finite(returned_missing_rate_value)) round(100 * returned_missing_rate_value, 2) else NA_real_,
    `Original missing glucose` = original_missing,
    `Original missing glucose (%)` = original_missing_percent,
    `Analysis missing glucose` = analysis_missing,
    `Analysis missing glucose (%)` = analysis_missing_percent,
    `Filled glucose rows` = rows_filled,
    `Estimated missing readings from gaps` = estimated_gap_readings,
    Warnings = if (length(imputation_warnings)) paste(imputation_warnings, collapse = " | ") else "",
    Message = message,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

preprocessing_comparison_summary <- function(imputation_status) {
  if (
    !is.data.frame(imputation_status) ||
      !nrow(imputation_status) ||
      !"Method" %in% names(imputation_status) ||
      identical(imputation_status$Method[[1L]], "None")
  ) {
    return(data.frame(
      Label = character(),
      Value = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    Label = c(
      "Original missing glucose rows",
      "Original missing glucose (%)",
      "Missing glucose rows after preprocessing",
      "Missing glucose (%) after preprocessing",
      "Filled glucose rows",
      "Estimated missing readings from gaps"
    ),
    Value = c(
      format_count(imputation_status[["Original missing glucose"]][[1L]]),
      paste0(format(round(imputation_status[["Original missing glucose (%)"]][[1L]], 2), trim = TRUE), "%"),
      format_count(imputation_status[["Analysis missing glucose"]][[1L]]),
      paste0(format(round(imputation_status[["Analysis missing glucose (%)"]][[1L]], 2), trim = TRUE), "%"),
      format_count(imputation_status[["Filled glucose rows"]][[1L]]),
      format_count(imputation_status[["Estimated missing readings from gaps"]][[1L]])
    ),
    stringsAsFactors = FALSE
  )
}

#' Create a missingness timeline plot
#'
#' @param data Standardized CGM data.
#'
#' @return A ggplot object.
#' @noRd
create_missingness_timeline_plot <- function(data) {
  if (!nrow(data) || !any(is_finite_cgm_timestamp(data$timestamp))) {
    return(empty_plot("No valid timestamps available"))
  }
  data <- data[is_finite_cgm_timestamp(data$timestamp), , drop = FALSE]
  gaps <- detect_gap_periods(data)
  point_data <- data
  point_data$status <- ifelse(
    point_data$imputed_flag %in% TRUE,
    "Imputed",
    ifelse(is.na(point_data$glucose), "Missing glucose", "Observed")
  )

  plot <- ggplot2::ggplot(point_data, ggplot2::aes(x = timestamp, y = id)) +
    ggplot2::geom_point(ggplot2::aes(color = status), alpha = 0.8, size = 1.7, na.rm = TRUE) +
    ggplot2::scale_color_manual(
      values = c("Observed" = "#219653", "Missing glucose" = "#EB5757", "Imputed" = "#2F80ED")
    ) +
    ggplot2::labs(x = NULL, y = "Subject ID", color = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  if (nrow(gaps)) {
    plot <- plot +
      ggplot2::geom_segment(
        data = gaps,
        ggplot2::aes(x = gap_start, xend = gap_end, y = id, yend = id),
        inherit.aes = FALSE,
        linewidth = 2.2,
        alpha = 0.35,
        color = "#F2994A"
      )
  }

  plot
}

empty_missingness_calendar_plot <- function(message = "No valid timestamps available") {
  plotly::layout(
    plotly::plot_ly(type = "scatter", mode = "markers"),
    annotations = list(list(
      text = message,
      x = 0.5,
      y = 0.5,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE
    )),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE)
  )
}

missingness_calendar_axis_labels <- function(calendar_data, show_subject_id = TRUE) {
  weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  ids <- unique(as.character(calendar_data$id))
  ticks <- data.frame(
    id = rep(ids, each = length(weekdays)),
    weekday = rep(weekdays, times = length(ids)),
    stringsAsFactors = FALSE
  )
  ticks$weekday_index <- match(ticks$weekday, weekdays)
  ticks$id_index <- match(ticks$id, ids)
  ticks$plot_y <- (ticks$id_index - 1L) * 8L + ticks$weekday_index
  ticks$label <- if (isTRUE(show_subject_id)) {
    ifelse(ticks$weekday == "Mon", paste0(ticks$id, "  ", ticks$weekday), paste0("      ", ticks$weekday))
  } else {
    as.character(ticks$weekday)
  }
  ticks
}

missingness_calendar_month_ticks <- function(calendar_data) {
  months <- unique(calendar_data[, c("month", "calendar_x"), drop = FALSE])
  months[!duplicated(months$month), , drop = FALSE]
}

missingness_calendar_dimensions <- function(calendar_data, show_subject_id = TRUE) {
  ids <- if (is.data.frame(calendar_data) && "id" %in% names(calendar_data)) {
    unique(as.character(calendar_data$id))
  } else {
    character()
  }
  ids <- ids[!is.na(ids) & nzchar(ids)]
  subject_count <- max(1L, length(ids))
  month_count <- if (is.data.frame(calendar_data) && "month" %in% names(calendar_data)) {
    length(unique(as.character(calendar_data$month)))
  } else {
    1L
  }
  max_id_chars <- if (isTRUE(show_subject_id) && length(ids)) max(nchar(ids), na.rm = TRUE) else 0L
  label_margin <- if (isTRUE(show_subject_id)) max(130L, min(280L, 82L + max_id_chars * 7L)) else 70L
  bottom_margin <- if (month_count > 10L) 125L else 95L

  list(
    subject_count = subject_count,
    month_count = max(1L, month_count),
    height = max(340L, min(1400L, 185L + subject_count * 82L)),
    marker_size = if (subject_count > 12L) 9L else 11L,
    margin = list(l = label_margin, r = 30L, t = 45L, b = bottom_margin),
    x_tick_angle = if (month_count > 10L) -35L else 0L
  )
}

create_missingness_heatmap_plot <- function(
  data,
  gaps = NULL,
  participant = "",
  date_range = NULL,
  calendar_data = NULL,
  interval_minutes = 5L,
  show_subject_id = NULL
) {
  show_subject_id <- show_subject_id_for_display(data, show_subject_id)
  if (is.null(calendar_data)) {
    participant <- normalize_filter_value(participant)
    if (nzchar(participant)) {
      data <- data[data$id == participant, , drop = FALSE]
      if (!is.null(gaps) && nrow(gaps) && "id" %in% names(gaps)) {
        gaps <- gaps[gaps$id == participant, , drop = FALSE]
      }
    }
    calendar_data <- compute_missingness_calendar_data(
      data,
      gaps = gaps,
      date_range = date_range,
      interval_minutes = interval_minutes,
      show_subject_id = show_subject_id
    )
  }

  if (!nrow(calendar_data)) {
    return(empty_missingness_calendar_plot())
  }

  y_labels <- missingness_calendar_axis_labels(calendar_data, show_subject_id = show_subject_id)
  month_ticks <- missingness_calendar_month_ticks(calendar_data)
  dimensions <- missingness_calendar_dimensions(calendar_data, show_subject_id = show_subject_id)
  plot <- plotly::plot_ly(height = dimensions$height)
  statuses <- levels(calendar_data$coverage_status)

  for (status in statuses) {
    status_data <- calendar_data[as.character(calendar_data$coverage_status) == status, , drop = FALSE]
    if (!nrow(status_data)) {
      next
    }
    plot <- plotly::add_markers(
      plot,
      data = status_data,
      x = ~calendar_x,
      y = ~plot_y,
      name = status,
      text = ~tooltip,
      hoverinfo = "text",
      showlegend = TRUE,
      marker = list(
        symbol = "square",
        size = dimensions$marker_size,
        color = coverage_status_color(status),
        line = list(color = "white", width = 0.8)
      ),
      inherit = FALSE
    )
  }

  plotly::layout(
    plot,
    title = list(text = "Daily data coverage", x = 0),
    xaxis = list(
      title = "",
      tickmode = "array",
      tickvals = month_ticks$calendar_x,
      ticktext = as.character(month_ticks$month),
      tickangle = dimensions$x_tick_angle,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis = list(
      title = "",
      tickmode = "array",
      tickvals = y_labels$plot_y,
      ticktext = y_labels$label,
      autorange = "reversed",
      automargin = TRUE,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    legend = list(
      title = list(text = "Daily data coverage"),
      orientation = "h",
      x = 0,
      y = -0.16
    ),
    margin = dimensions$margin,
    hovermode = "closest"
  )
}

create_missingness_coverage_calendar <- create_missingness_heatmap_plot
