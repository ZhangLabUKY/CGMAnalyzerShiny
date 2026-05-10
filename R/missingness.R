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

#' Detect timestamp gaps in standardized CGM data
#'
#' @param data Standardized CGM data.
#' @param gap_multiplier Gap threshold as a multiple of median interval.
#'
#' @return A data frame of detected gap periods.
#' @export
detect_gap_periods <- function(data, gap_multiplier = 1.5) {
  if (!all(c("id", "timestamp") %in% names(data))) {
    stop("Gap detection requires standardized columns: id and timestamp.", call. = FALSE)
  }

  dt <- data.table::as.data.table(data)
  out <- dt[, {
    timestamp_values <- get("timestamp")
    timestamps <- sort(unique(timestamp_values[!is.na(timestamp_values)]))
    interval <- median_sampling_interval(timestamps)
    if (length(timestamps) < 2L || is.na(interval) || interval <= 0) {
      list(
        gap_start = as.POSIXct(character()),
        gap_end = as.POSIXct(character()),
        gap_minutes = numeric(),
        expected_interval_minutes = numeric(),
        estimated_missing_readings = integer()
      )
    } else {
      diffs <- as.numeric(diff(timestamps), units = "mins")
      gap_idx <- which(diffs > interval * gap_multiplier)
      if (!length(gap_idx)) {
        list(
          gap_start = as.POSIXct(character()),
          gap_end = as.POSIXct(character()),
          gap_minutes = numeric(),
          expected_interval_minutes = numeric(),
          estimated_missing_readings = integer()
        )
      } else {
        list(
          gap_start = timestamps[gap_idx],
          gap_end = timestamps[gap_idx + 1L],
          gap_minutes = diffs[gap_idx],
          expected_interval_minutes = interval,
          estimated_missing_readings = pmax(0L, as.integer(round(diffs[gap_idx] / interval)) - 1L)
        )
      }
    }
  }, by = id]

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out
}

#' Compute user-facing missingness summary
#'
#' @param data Standardized CGM data.
#' @param valid_day_hours Minimum observed hours for a valid CGM day.
#'
#' @return A data frame with per-participant missingness summaries.
#' @export
compute_missingness_summary <- function(data, valid_day_hours = 14) {
  qc <- compute_qc_summary(data, valid_day_hours = valid_day_hours)
  gaps <- detect_gap_periods(data)

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
    missing_glucose = qc$missing_glucose,
    missing_glucose_rate = ifelse(qc$readings > 0, round(100 * qc$missing_glucose / qc$readings, 2), NA_real_),
    gap_count = as.integer(gap_counts[qc$id]),
    max_gap_minutes = qc$max_gap_minutes,
    valid_days = qc$valid_days,
    observed_days = qc$observed_days,
    median_interval_minutes = qc$median_interval_minutes,
    duplicate_timestamps = qc$duplicate_timestamps,
    stringsAsFactors = FALSE
  )
  out$gap_count[is.na(out$gap_count)] <- 0L
  out$estimated_missing_readings <- estimated_missing$estimated_missing_readings[match(out$id, estimated_missing$id)]
  out$estimated_missing_readings[is.na(out$estimated_missing_readings)] <- 0L
  out
}

compute_missingness_heatmap_data <- function(data, gaps = NULL) {
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

  dt <- data.table::as.data.table(data[is_finite_cgm_timestamp(data$timestamp), , drop = FALSE])
  dt[, date := as.Date(timestamp)]
  daily <- dt[, {
    glucose_values <- get("glucose")
    imputed_values <- get("imputed_flag")
    missing <- sum(is.na(glucose_values))
    readings <- length(glucose_values)
    list(
      readings = readings,
      missing_glucose = missing,
      missing_glucose_rate = if (readings > 0L) round(100 * missing / readings, 2) else NA_real_,
      imputed_rows = sum(imputed_values %in% TRUE, na.rm = TRUE)
    )
  }, by = .(id, date)]

  if (is.null(gaps)) {
    gaps <- detect_gap_periods(data)
  }
  if (nrow(gaps)) {
    gap_dt <- data.table::as.data.table(gaps)
    gap_dt[, date := as.Date(gap_start)]
    gap_summary <- gap_dt[, list(
      timestamp_gaps = .N,
      estimated_missing_readings = sum(estimated_missing_readings, na.rm = TRUE)
    ), by = .(id, date)]
    daily <- merge(daily, gap_summary, by = c("id", "date"), all.x = TRUE, sort = FALSE)
  } else {
    daily$timestamp_gaps <- 0L
    daily$estimated_missing_readings <- 0L
  }
  daily$timestamp_gaps[is.na(daily$timestamp_gaps)] <- 0L
  daily$estimated_missing_readings[is.na(daily$estimated_missing_readings)] <- 0L

  daily <- daily[order(id, date)]
  subject_prefix <- if (subject_id_filter_available(data)) {
    paste0("Subject ID: ", daily$id, "<br>")
  } else {
    ""
  }
  daily$tooltip <- paste0(
    subject_prefix,
    "Date: ", daily$date,
    "<br>Readings: ", daily$readings,
    "<br>Missing glucose: ", daily$missing_glucose,
    "<br>Missing glucose rate: ", daily$missing_glucose_rate, "%",
    "<br>Timestamp gaps: ", daily$timestamp_gaps,
    "<br>Estimated missing readings: ", daily$estimated_missing_readings,
    "<br>Imputed rows: ", daily$imputed_rows
  )
  as.data.frame(daily, stringsAsFactors = FALSE)
}

coverage_status <- function(coverage_percent, readings) {
  out <- rep("No data", length(coverage_percent))
  has_data <- !is.na(readings) & readings > 0L
  out[has_data & coverage_percent < 50] <- "Low coverage"
  out[has_data & coverage_percent >= 50 & coverage_percent < 80] <- "Partial coverage"
  out[has_data & coverage_percent >= 80] <- "High coverage"
  factor(out, levels = c("No data", "Low coverage", "Partial coverage", "High coverage"), ordered = TRUE)
}

coverage_status_color <- function(status) {
  colors <- c(
    "No data" = "#D0D5DD",
    "Low coverage" = "#D92D20",
    "Partial coverage" = "#FDB022",
    "High coverage" = "#2E90FA"
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
  week_map$week_of_month <- ave(seq_len(nrow(week_map)), week_map$month_start, FUN = seq_along)

  month_width <- stats::aggregate(week_of_month ~ month_start, data = week_map, FUN = max)
  month_width <- month_width[order(month_width$month_start), , drop = FALSE]
  month_width$month_index <- seq_len(nrow(month_width))
  month_width$month_offset <- c(0L, cumsum(head(month_width$week_of_month + 1L, -1L)))

  daily <- merge(daily, week_map, by = c("month_start", "week_start"), all.x = TRUE, sort = FALSE)
  daily <- merge(
    daily,
    month_width[, c("month_start", "month_index", "month_offset"), drop = FALSE],
    by = "month_start",
    all.x = TRUE,
    sort = FALSE
  )
  daily$calendar_x <- daily$month_offset + daily$week_of_month
  daily$week_index <- daily$calendar_x

  id_index <- match(as.character(daily$id), ids)
  daily$plot_y <- (id_index - 1L) * 8L + daily$weekday_index
  daily <- daily[order(daily$id, daily$calendar_x, daily$weekday_index), , drop = FALSE]
  daily$month <- factor(daily$month, levels = unique(daily$month[order(daily$month_start)]), ordered = TRUE)
  daily
}

compute_missingness_calendar_data <- function(data, gaps = NULL, date_range = NULL) {
  daily <- compute_missingness_heatmap_data(data, gaps = gaps)
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
  qc <- compute_qc_summary(data)
  intervals <- qc$median_interval_minutes
  names(intervals) <- qc$id
  expected_lookup <- ifelse(!is.na(intervals) & intervals > 0, round(24 * 60 / intervals), NA_real_)
  complete <- expand_subject_active_dates(spans)
  complete$date <- as.Date(complete$date)
  daily <- merge(complete, daily, by = c("id", "date"), all.x = TRUE, sort = FALSE)
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
  subject_prefix <- if (subject_id_filter_available(data)) {
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
    "<br>Missing glucose: ", daily$missing_glucose,
    "<br>Missing glucose rate: ", ifelse(is.na(daily$missing_glucose_rate), "no readings", paste0(daily$missing_glucose_rate, "%")),
    "<br>Timestamp gaps: ", daily$timestamp_gaps,
    "<br>Estimated missing readings: ", daily$estimated_missing_readings,
    "<br>Imputed rows: ", daily$imputed_rows
  )
  add_compact_calendar_positions(daily, ids)
}

filter_missingness_calendar_participant <- function(calendar_data, participant = "") {
  participant <- normalize_filter_value(participant)
  if (nzchar(participant)) {
    calendar_data <- calendar_data[calendar_data$id == participant, , drop = FALSE]
  }
  calendar_data
}

filled_glucose_by_id <- function(original_data, analysis_data, ids) {
  if (nrow(original_data) != nrow(analysis_data)) {
    return(stats::setNames(rep(NA_integer_, length(ids)), ids))
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
#'
#' @return A compact participant-level before/after table.
#' @export
compare_missingness_summaries <- function(original_data, analysis_data, valid_day_hours = 14) {
  original <- compute_missingness_summary(original_data, valid_day_hours = valid_day_hours)
  analysis <- compute_missingness_summary(analysis_data, valid_day_hours = valid_day_hours)
  ids <- sort(unique(c(original$id, analysis$id)))

  if (!length(ids)) {
    return(data.frame())
  }

  original_match <- match(ids, original$id)
  analysis_match <- match(ids, analysis$id)
  filled <- filled_glucose_by_id(original_data, analysis_data, ids)

  out <- data.frame(
    `Subject ID` = ids,
    `Missing glucose` = original$missing_glucose[original_match],
    `Missing glucose rate (%)` = original$missing_glucose_rate[original_match],
    `Missing glucose after preprocessing` = analysis$missing_glucose[analysis_match],
    `Missing glucose rate after preprocessing (%)` = analysis$missing_glucose_rate[analysis_match],
    `Filled glucose rows` = as.integer(filled[ids]),
    `Timestamp gaps` = original$gap_count[original_match],
    `Estimated missing readings` = original$estimated_missing_readings[original_match],
    `Valid days` = analysis$valid_days[analysis_match],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!subject_id_filter_available(analysis_data)) {
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

#' Summarize imputation status for user-facing QC
#'
#' @param original_data Standardized CGM data before optional imputation.
#' @param analysis_data Current analysis CGM data after preprocessing settings.
#' @param settings Reproducibility settings list.
#'
#' @return A one-row data frame with imputation status details.
#' @export
summarize_imputation_status <- function(original_data, analysis_data, settings) {
  method <- settings$imputation_method %||% "none"
  available <- isTRUE(settings$imputation_available %||% cgmissingdata_available())
  seed <- settings$imputation_seed %||% NA_integer_
  original_missing <- sum(is.na(original_data$glucose))
  analysis_missing <- sum(is.na(analysis_data$glucose))
  rows_filled <- sum(
    is.na(original_data$glucose) &
      !is.na(analysis_data$glucose) &
      (analysis_data$imputed_flag %in% TRUE),
    na.rm = TRUE
  )

  if (!identical(method, "mice_only")) {
    status <- "Not applied"
    message <- "Imputation is off. Analysis uses the original standardized data."
  } else if (!available) {
    status <- "Unavailable"
    message <- "MICE imputation is selected, but the imputation feature is not available in this R session."
  } else if (original_missing == 0L) {
    status <- "Not needed"
    message <- "No missing glucose values are available for imputation."
  } else if (rows_filled > 0L) {
    status <- "Applied"
    message <- paste("MICE imputation filled", rows_filled, "missing glucose row(s).")
  } else {
    status <- "No rows filled"
    message <- "MICE imputation was selected, but no glucose values were filled."
  }

  data.frame(
    Method = format_imputation_method(method),
    Status = status,
    Seed = seed,
    `Original missing glucose` = original_missing,
    `Analysis missing glucose` = analysis_missing,
    `Filled glucose rows` = rows_filled,
    Message = message,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Create a missingness timeline plot
#'
#' @param data Standardized CGM data.
#'
#' @return A ggplot object.
#' @export
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

create_missingness_heatmap_plot <- function(data, gaps = NULL, participant = "", date_range = NULL) {
  show_subject_id <- subject_id_filter_available(data)
  participant <- normalize_filter_value(participant)
  if (nzchar(participant)) {
    data <- data[data$id == participant, , drop = FALSE]
    if (!is.null(gaps) && nrow(gaps) && "id" %in% names(gaps)) {
      gaps <- gaps[gaps$id == participant, , drop = FALSE]
    }
  }

  calendar_data <- compute_missingness_calendar_data(data, gaps = gaps, date_range = date_range)
  if (!nrow(calendar_data)) {
    return(empty_missingness_calendar_plot())
  }

  y_labels <- missingness_calendar_axis_labels(calendar_data, show_subject_id = show_subject_id)
  month_ticks <- missingness_calendar_month_ticks(calendar_data)
  plot <- plotly::plot_ly()
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
        size = 11,
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
    margin = list(l = 130, r = 30, t = 45, b = 90),
    hovermode = "closest"
  )
}

create_missingness_coverage_calendar <- create_missingness_heatmap_plot
