#' Create a CGM trace plot
#'
#' @param data Standardized CGM data.
#' @param thresholds Named glucose thresholds in mg/dL.
#' @param participant_id Optional participant ID filter.
#'
#' @return A ggplot object.
#' @noRd
clinical_plot_theme <- function(legend_position = "bottom") {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(color = "#263642"),
      axis.title = ggplot2::element_text(color = "#263642", face = "bold"),
      axis.text = ggplot2::element_text(color = "#3c4e5c"),
      panel.grid.major = ggplot2::element_line(color = "#dfe8e4", linewidth = 0.35),
      panel.grid.minor = ggplot2::element_line(color = "#edf3f0", linewidth = 0.25),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(color = "#263642", face = "bold"),
      legend.text = ggplot2::element_text(color = "#3c4e5c"),
      plot.margin = ggplot2::margin(t = 8, r = 12, b = 12, l = 8)
    )
}

clinical_discrete_palette <- function(values) {
  values <- sort(unique(as.character(values)))
  values <- values[!is.na(values) & nzchar(values)]
  base <- c("#1F8A70", "#2F80ED", "#7C3AED", "#D99013", "#C95142", "#2A9D8F", "#5B6770", "#22B883")
  if (length(values) > length(base)) {
    base <- grDevices::colorRampPalette(base)(length(values))
  }
  stats::setNames(base[seq_along(values)], values)
}

format_plot_timestamp <- function(timestamp) {
  out <- format(timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  out[is.na(timestamp)] <- "Not available"
  out
}

format_plot_time_of_day <- function(minutes) {
  minutes <- suppressWarnings(as.numeric(minutes))
  hours <- floor(minutes / 60)
  mins <- floor(minutes %% 60)
  ifelse(is.na(minutes), "Not available", sprintf("%02d:%02d", hours, mins))
}

format_plot_glucose <- function(glucose) {
  glucose <- suppressWarnings(as.numeric(glucose))
  ifelse(is.na(glucose), "Not available", paste0(round(glucose, 1), " mg/dL"))
}

plot_imputed_label <- function(imputed_flag) {
  ifelse(imputed_flag %in% TRUE, "Yes", "No")
}

ensure_plot_imputed_flag <- function(data) {
  if (!"imputed_flag" %in% names(data)) {
    data$imputed_flag <- FALSE
  }
  data
}

trace_hover_text <- function(data) {
  paste0(
    "Subject ID: ", data$id,
    "<br>Timestamp: ", format_plot_timestamp(data$timestamp),
    "<br>Glucose: ", format_plot_glucose(data$glucose),
    "<br>Imputed: ", plot_imputed_label(data$imputed_flag)
  )
}

daily_overlay_hover_text <- function(data) {
  paste0(
    "Subject ID: ", data$id,
    "<br>Date: ", data$date,
    "<br>Time: ", format_plot_time_of_day(data$time_minutes),
    "<br>Glucose: ", format_plot_glucose(data$glucose),
    "<br>Imputed: ", plot_imputed_label(data$imputed_flag)
  )
}

agp_hover_text <- function(data, value = "median") {
  value <- value %||% "median"
  label <- switch(
    value,
    q05 = "5th percentile",
    q25 = "25th percentile",
    q75 = "75th percentile",
    q95 = "95th percentile",
    median = "Median glucose",
    "Glucose"
  )
  values <- data[[value]] %||% rep(NA_real_, nrow(data))
  paste0(
    "Time of day: ", format_plot_time_of_day(data$time_minutes),
    "<br>", label, ": ", format_plot_glucose(values),
    "<br>Readings: ", data$n
  )
}

create_trace_plot <- function(
  data,
  thresholds = default_cgm_thresholds(),
  participant_id = NULL,
  time_window = default_time_window(),
  max_points_per_participant = Inf
) {
  if (!is.null(participant_id) && nzchar(participant_id)) {
    data <- data[data$id == participant_id, , drop = FALSE]
  }
  time_window <- normalize_time_window(time_window)
  data <- filter_time_window_data(data, time_window)
  data <- data[is_finite_cgm_timestamp(data$timestamp) & !is.na(data$glucose), , drop = FALSE]
  data <- prepare_plot_display_data(data, max_points_per_participant = max_points_per_participant)
  if (!nrow(data)) {
    return(empty_plot(plot_empty_message("trace")))
  }
  data <- ensure_plot_imputed_flag(data)
  data$plot_group <- if (identical(time_window, "full_day")) {
    as.character(data$id)
  } else {
    interaction(data$id, as.Date(data$timestamp), drop = TRUE, lex.order = TRUE)
  }
  data$Tooltip <- trace_hover_text(data)

  ggplot2::ggplot(data, ggplot2::aes(x = timestamp, y = glucose, color = id, group = plot_group, text = Tooltip)) +
    ggplot2::geom_line(alpha = 0.8, linewidth = 0.35, na.rm = TRUE) +
    suppressWarnings(ggplot2::geom_point(
      data = data[data$imputed_flag %in% TRUE, , drop = FALSE],
      ggplot2::aes(x = timestamp, y = glucose, text = Tooltip),
      inherit.aes = FALSE,
      size = 1.6,
      alpha = 0.9,
      na.rm = TRUE
    )) +
    ggplot2::geom_hline(yintercept = thresholds$tir_lower, linetype = "dashed", color = "#C95142") +
    ggplot2::geom_hline(yintercept = thresholds$tir_upper, linetype = "dashed", color = "#C95142") +
    ggplot2::scale_color_manual(values = clinical_discrete_palette(data$id)) +
    ggplot2::labs(x = NULL, y = "Glucose (mg/dL)", color = "Subject ID") +
    clinical_plot_theme(legend_position = "bottom")
}

plot_detail_choices <- function() {
  c("Fast" = "fast", "Balanced" = "balanced", "Full detail" = "full")
}

plot_detail_max_points <- function(detail = "balanced") {
  detail <- detail %||% "balanced"
  switch(
    detail,
    fast = 500L,
    balanced = 2000L,
    full = Inf,
    2000L
  )
}

adaptive_plot_target_points <- function() {
  25000L
}

adaptive_plot_max_points_per_subject <- function(
  data,
  target_total_points = adaptive_plot_target_points(),
  minimum_per_subject = 250L
) {
  if (!is.data.frame(data) || !nrow(data) || !"id" %in% names(data)) {
    return(Inf)
  }
  target_total_points <- suppressWarnings(as.integer(target_total_points[[1L]]))
  if (is.na(target_total_points) || target_total_points <= 0L) {
    target_total_points <- adaptive_plot_target_points()
  }
  total <- nrow(data)
  if (total <= target_total_points) {
    return(Inf)
  }
  ids <- clean_filter_values(data$id)
  subject_count <- max(1L, length(ids))
  per_subject <- floor(target_total_points / subject_count)
  max(as.integer(minimum_per_subject), as.integer(per_subject))
}

plot_display_row_count <- function(data, max_points_per_participant = Inf) {
  if (!is.data.frame(data) || !nrow(data)) {
    return(0L)
  }
  if (!is.finite(max_points_per_participant)) {
    return(nrow(data))
  }
  nrow(prepare_plot_display_data(data, max_points_per_participant = max_points_per_participant))
}

select_evenly_spaced_rows <- function(idx, n) {
  if (!length(idx) || n <= 0L) {
    return(integer())
  }
  if (length(idx) <= n) {
    return(idx)
  }
  idx[unique(round(seq(1, length(idx), length.out = n)))]
}

select_lttb_rows <- function(data, idx, n) {
  if (!length(idx) || n <= 0L || length(idx) <= n) {
    return(select_evenly_spaced_rows(idx, n))
  }
  candidate <- data.frame(
    x = as.numeric(data$timestamp[idx]),
    y = as.numeric(data$glucose[idx]),
    .row_id = idx,
    stringsAsFactors = FALSE
  )
  candidate <- candidate[is.finite(candidate$x) & is.finite(candidate$y), , drop = FALSE]
  if (nrow(candidate) <= n) {
    return(candidate$.row_id)
  }
  selected <- tryCatch(
    lttb_indices_cpp(candidate$x, candidate$y, as.integer(n)),
    error = function(error) NULL
  )
  if (is.integer(selected) && length(selected)) {
    selected <- selected[selected >= 1L & selected <= nrow(candidate)]
    if (length(selected)) {
      return(as.integer(candidate$.row_id[selected]))
    }
  }
  select_evenly_spaced_rows(idx, n)
}

protected_plot_rows <- function(data, row_idx) {
  protected <- row_idx[data$imputed_flag %in% TRUE]
  glucose <- suppressWarnings(as.numeric(data$glucose))
  finite <- row_idx[is.finite(glucose)]
  if (length(finite)) {
    protected <- c(
      protected,
      finite[which.min(glucose[finite])],
      finite[which.max(glucose[finite])]
    )
  }
  unique(protected)
}

downsample_one_plot_group <- function(data, max_points) {
  if (!is.finite(max_points) || nrow(data) <= max_points) {
    return(data)
  }

  max_points <- as.integer(max_points)
  data <- data[order(data$timestamp), , drop = FALSE]
  row_idx <- seq_len(nrow(data))
  keep_protected <- select_evenly_spaced_rows(protected_plot_rows(data, row_idx), max_points)
  remaining_slots <- max_points - length(keep_protected)
  candidate_idx <- setdiff(row_idx, keep_protected)
  keep_candidates <- select_lttb_rows(data, candidate_idx, remaining_slots)
  keep <- sort(unique(c(keep_protected, keep_candidates)))
  data[keep, , drop = FALSE]
}

prepare_plot_display_data <- function(data, max_points_per_participant = Inf) {
  if (!nrow(data) || !is.finite(max_points_per_participant)) {
    return(data)
  }

  rows <- lapply(split(data, data$id, drop = TRUE), downsample_one_plot_group, max_points = max_points_per_participant)
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

plot_filter_available <- function(data, column, min_values = 2L) {
  if (!column %in% names(data)) {
    return(FALSE)
  }
  values <- clean_filter_values(data[[column]])
  length(values) >= min_values
}

plot_subject_filter_choices <- function(data) {
  values <- if ("id" %in% names(data)) data$id else character()
  subject_filter_choices(values, all_label = "All")
}

normalize_plot_days <- function(day) {
  day <- day %||% character()
  day <- as.character(day)
  day <- trimws(day)
  day <- day[!is.na(day) & nzchar(day)]
  if (!length(day) || all_filter_value() %in% day) {
    return(all_filter_value())
  }
  sort(unique(day))
}

normalize_plot_day <- normalize_plot_days

plot_day_cache_key <- function(day) {
  paste(normalize_plot_days(day), collapse = "|")
}

preserve_plot_day_selection <- function(selected, choices, previous = all_filter_value()) {
  selected_raw <- selected %||% character()
  selected_raw <- as.character(selected_raw)
  selected_raw <- trimws(selected_raw)
  selected_raw <- selected_raw[!is.na(selected_raw) & nzchar(selected_raw)]
  choice_values <- unname(choices)

  if (!length(selected_raw)) {
    previous <- normalize_plot_days(previous)
    if (!identical(previous, all_filter_value()) && all(previous %in% choice_values)) {
      return(previous)
    }
    return(all_filter_value())
  }

  selected <- normalize_plot_days(selected_raw)
  if (identical(selected, all_filter_value()) || all_filter_value() %in% selected) {
    return(all_filter_value())
  }
  selected <- selected[selected %in% choice_values]
  if (!length(selected)) {
    return(all_filter_value())
  }
  choice_values[choice_values %in% selected & choice_values != all_filter_value()]
}

filter_plot_data <- function(
  data,
  participant = "",
  group = "",
  day = "",
  time_window = default_time_window()
) {
  group <- normalize_filter_value(group)
  day <- normalize_plot_days(day)
  time_window <- normalize_time_window(time_window)

  data <- filter_data_by_subject_selection(data, participant)
  if ("group" %in% names(data) && nzchar(group)) {
    data <- data[data$group == group, , drop = FALSE]
  }
  if (!identical(day, all_filter_value())) {
    data <- data[as.character(as.Date(data$timestamp)) %in% day, , drop = FALSE]
  }
  filter_time_window_data(data, time_window)
}

available_plot_days <- function(
  data,
  participant = "",
  group = "",
  time_window = default_time_window()
) {
  data <- filter_plot_data(
    data,
    participant = participant,
    group = group,
    time_window = time_window
  )
  sort(unique(as.character(as.Date(data$timestamp[!is.na(data$timestamp)]))))
}

plot_day_filter_choices <- function(
  data,
  participant = "",
  group = "",
  time_window = default_time_window()
) {
  filter_select_choices(
    available_plot_days(
      data,
      participant = participant,
      group = group,
      time_window = time_window
    ),
    all_label = "All days"
  )
}

is_finite_cgm_timestamp <- function(timestamp) {
  !is.na(timestamp) & is.finite(as.numeric(timestamp))
}

plot_empty_message <- function(plot_type = "trace") {
  switch(
    plot_type %||% "trace",
    trace = "No glucose readings are available for the selected plot filters.",
    daily_overlay = "No daily overlay data are available for the selected day/filter choices.",
    agp = "Not enough glucose readings are available to build an AGP summary.",
    "No data available"
  )
}

empty_plot <- function(message = "No data available") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message) +
    ggplot2::theme_void()
}

plot_filtered_data <- function(
  data,
  plot_type = "trace",
  participant = "",
  group = "",
  day = "",
  time_window = default_time_window()
) {
  day <- if (identical(plot_type, "daily_overlay")) normalize_plot_days(day) else all_filter_value()
  filtered <- filter_plot_data(
    data,
    participant = participant,
    group = group,
    day = day,
    time_window = time_window
  )
  filtered[is_finite_cgm_timestamp(filtered$timestamp), , drop = FALSE]
}

daily_overlay_date_legend_limit <- function() {
  14L
}

daily_overlay_legend_visible <- function(day_count, date_legend_limit = daily_overlay_date_legend_limit()) {
  is.finite(day_count) && day_count > 0L && day_count <= date_legend_limit
}

daily_overlay_day_mode <- function(day, day_count = NA_integer_) {
  day <- normalize_plot_days(day)
  if (!is.na(day_count) && day_count == 0L) {
    return("No days")
  }
  if (identical(day, all_filter_value())) {
    "All days"
  } else {
    "Selected days"
  }
}

daily_overlay_summary_rows <- function(
  plot_data,
  day = "",
  date_legend_limit = daily_overlay_date_legend_limit()
) {
  day_count <- length(unique(as.Date(plot_data$timestamp[is_finite_cgm_timestamp(plot_data$timestamp)])))
  legend <- if (daily_overlay_legend_visible(day_count, date_legend_limit)) {
    "Shown"
  } else if (day_count > date_legend_limit) {
    "Hidden for readability"
  } else {
    "Not available"
  }
  data.frame(
    Label = c("Day selection", "Days shown", "Date legend"),
    Value = c(
      daily_overlay_day_mode(day, day_count = day_count),
      format_count(day_count),
      legend
    ),
    stringsAsFactors = FALSE
  )
}

daily_overlay_legend_note <- function(
  plot_data,
  date_legend_limit = daily_overlay_date_legend_limit()
) {
  day_count <- length(unique(as.Date(plot_data$timestamp[is_finite_cgm_timestamp(plot_data$timestamp)])))
  if (day_count > date_legend_limit) {
    "Many days are shown, so the date legend is hidden for readability. Select fewer days to show the legend."
  } else {
    ""
  }
}

sanitize_plot_filename_part <- function(x) {
  x <- x %||% ""
  x <- paste(as.character(x), collapse = "_")
  x <- trimws(tolower(x))
  x <- gsub("[^a-z0-9_-]+", "-", x)
  x <- gsub("-+", "-", x)
  x <- gsub("^-|-$", "", x)
  if (!nzchar(x)) "all" else x
}

plot_type_filename_label <- function(plot_type = "trace") {
  switch(
    plot_type %||% "trace",
    daily_overlay = "daily-overlay",
    agp = "agp-summary",
    trace = "trace",
    sanitize_plot_filename_part(plot_type)
  )
}

plot_day_filename_label <- function(day = "") {
  day <- normalize_plot_days(day)
  if (identical(day, all_filter_value())) {
    return("all-days")
  }
  day <- sort(unique(as.character(day)))
  if (length(day) == 1L) {
    return(paste0("day-", sanitize_plot_filename_part(day)))
  }
  paste0("days-", sanitize_plot_filename_part(paste0(day[[1L]], "_to_", day[[length(day)]])))
}

plot_subject_filename_label <- function(participant = "") {
  participant <- normalize_filter_value(participant)
  if (nzchar(participant)) {
    paste0("subject-", sanitize_plot_filename_part(participant))
  } else {
    "all-subjects"
  }
}

plot_optional_filter_filename_label <- function(prefix, value = "") {
  value <- normalize_filter_value(value)
  if (nzchar(value)) {
    paste0(prefix, "-", sanitize_plot_filename_part(value))
  } else {
    character()
  }
}

plot_download_filename <- function(
  data,
  plot_type = "trace",
  participant = "",
  group = "",
  day = "",
  time_window = default_time_window()
) {
  filtered <- plot_filtered_data(
    data,
    plot_type = plot_type,
    participant = participant,
    group = group,
    day = day,
    time_window = time_window
  )
  date_span <- if (nrow(filtered)) {
    paste0("dates-", sanitize_plot_filename_part(format_date_span(filtered$timestamp)))
  } else {
    "dates-none"
  }
  parts <- c(
    "cgm",
    plot_type_filename_label(plot_type),
    plot_subject_filename_label(participant),
    plot_optional_filter_filename_label("group", group),
    time_window_filename_label(time_window),
    if (identical(plot_type, "daily_overlay")) plot_day_filename_label(day) else date_span
  )
  paste0(paste(parts[nzchar(parts)], collapse = "_"), ".png")
}

#' Prepare CGM data for time-of-day plots
#'
#' @param data Standardized CGM data.
#' @param participant Subject ID filter.
#' @param group Group filter.
#' @param day Date filter as `YYYY-MM-DD`.
#'
#' @return A data frame with time-of-day plotting columns.
#' @noRd
prepare_time_of_day_plot_data <- function(
  data,
  participant = "",
  group = "",
  day = "",
  time_window = default_time_window(),
  max_points_per_participant = Inf
) {
  data <- filter_plot_data(
    data,
    participant = participant,
    group = group,
    day = day,
    time_window = time_window
  )
  data <- data[is_finite_cgm_timestamp(data$timestamp), , drop = FALSE]
  data <- prepare_plot_display_data(data, max_points_per_participant = max_points_per_participant)
  data <- ensure_plot_imputed_flag(data)
  if (!nrow(data)) {
    return(data.frame(
      id = character(),
      date = character(),
      time_minutes = numeric(),
      glucose = numeric(),
      imputed_flag = logical(),
      stringsAsFactors = FALSE
    ))
  }

  timestamp <- as.POSIXlt(data$timestamp)
  data$date <- as.character(as.Date(data$timestamp))
  data$time_minutes <- timestamp$hour * 60 + timestamp$min + timestamp$sec / 60
  data$plot_group <- interaction(data$id, data$date, drop = TRUE, lex.order = TRUE)
  data$Tooltip <- daily_overlay_hover_text(data)
  data[order(data$id, data$date, data$time_minutes), , drop = FALSE]
}

time_of_day_scale <- function() {
  ggplot2::scale_x_continuous(
    breaks = seq(0, 1440, by = 240),
    labels = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00", "24:00"),
    limits = c(0, 1440)
  )
}

target_range_label <- function(thresholds) {
  paste0("Target range (", thresholds$tir_lower, "-", thresholds$tir_upper, " mg/dL)")
}

#' Create a daily overlay CGM plot
#'
#' @param data Standardized CGM data.
#' @param thresholds Named glucose thresholds in mg/dL.
#' @param participant Subject ID filter.
#' @param group Group filter.
#' @param day Date filter as `YYYY-MM-DD`.
#'
#' @return A ggplot object.
#' @noRd
create_daily_overlay_plot <- function(
  data,
  thresholds = default_cgm_thresholds(),
  participant = "",
  group = "",
  day = "",
  time_window = default_time_window(),
  max_points_per_participant = Inf,
  date_legend_limit = daily_overlay_date_legend_limit()
) {
  day <- normalize_plot_days(day)
  plot_data <- prepare_time_of_day_plot_data(
    data,
    participant = participant,
    group = group,
    day = day,
    time_window = time_window,
    max_points_per_participant = max_points_per_participant
  )
  if (!nrow(plot_data)) {
    return(empty_plot(plot_empty_message("daily_overlay")))
  }

  show_date_legend <- daily_overlay_legend_visible(length(unique(plot_data$date)), date_legend_limit)
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = time_minutes, y = glucose, color = date, group = plot_group, text = Tooltip)
  ) +
    ggplot2::geom_line(alpha = 0.75, linewidth = 0.35, na.rm = TRUE) +
    suppressWarnings(ggplot2::geom_point(
      data = plot_data[plot_data$imputed_flag %in% TRUE, , drop = FALSE],
      ggplot2::aes(x = time_minutes, y = glucose, text = Tooltip),
      inherit.aes = FALSE,
      size = 1.6,
      alpha = 0.9,
      na.rm = TRUE
    )) +
    ggplot2::geom_hline(yintercept = thresholds$tir_lower, linetype = "dashed", color = "#C95142") +
    ggplot2::geom_hline(yintercept = thresholds$tir_upper, linetype = "dashed", color = "#C95142") +
    ggplot2::scale_color_manual(values = clinical_discrete_palette(plot_data$date)) +
    time_of_day_scale() +
    ggplot2::labs(x = "Time of day", y = "Glucose (mg/dL)", color = "Date") +
    clinical_plot_theme(legend_position = if (show_date_legend) "bottom" else "none")

  if (length(unique(plot_data$id)) > 1L) {
    plot <- plot + ggplot2::facet_wrap(~id)
  }
  plot
}

plotly_webgl_row_threshold <- function() {
  5000L
}

plotly_empty_plot <- function(message = "No data available") {
  plotly::layout(
    plotly::plot_ly(type = "scatter", mode = "markers"),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    annotations = list(list(
      text = message,
      x = 0.5,
      y = 0.5,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE
    ))
  )
}

plotly_threshold_shapes <- function(thresholds) {
  lapply(c(thresholds$tir_lower, thresholds$tir_upper), function(y) {
    list(
      type = "line",
      xref = "paper",
      x0 = 0,
      x1 = 1,
      y0 = y,
      y1 = y,
      line = list(color = "#C95142", dash = "dash", width = 1)
    )
  })
}

maybe_to_webgl <- function(plotly_obj, rows, threshold = plotly_webgl_row_threshold()) {
  rows <- suppressWarnings(as.integer(rows %||% 0L))
  threshold <- suppressWarnings(as.integer(threshold %||% plotly_webgl_row_threshold()))
  if (!is.na(rows) && !is.na(threshold) && rows >= threshold) {
    return(plotly::toWebGL(plotly_obj))
  }
  plotly_obj
}

create_trace_plotly <- function(
  data,
  thresholds = default_cgm_thresholds(),
  participant_id = NULL,
  time_window = default_time_window(),
  max_points_per_participant = Inf
) {
  if (!is.null(participant_id) && nzchar(participant_id)) {
    data <- data[data$id == participant_id, , drop = FALSE]
  }
  time_window <- normalize_time_window(time_window)
  data <- filter_time_window_data(data, time_window)
  data <- data[is_finite_cgm_timestamp(data$timestamp) & !is.na(data$glucose), , drop = FALSE]
  data <- prepare_plot_display_data(data, max_points_per_participant = max_points_per_participant)
  if (!nrow(data)) {
    return(plotly_empty_plot(plot_empty_message("trace")))
  }
  data <- ensure_plot_imputed_flag(data)
  data$plot_group <- if (identical(time_window, "full_day")) {
    as.character(data$id)
  } else {
    interaction(data$id, as.Date(data$timestamp), drop = TRUE, lex.order = TRUE)
  }
  data$Tooltip <- trace_hover_text(data)
  palette <- clinical_discrete_palette(data$id)
  shown_ids <- character()
  plot <- plotly::plot_ly()
  for (group_name in unique(as.character(data$plot_group))) {
    group_data <- data[as.character(data$plot_group) == group_name, , drop = FALSE]
    id <- as.character(group_data$id[[1L]])
    show_legend <- !id %in% shown_ids
    shown_ids <- unique(c(shown_ids, id))
    plot <- plotly::add_trace(
      plot,
      data = group_data,
      x = ~timestamp,
      y = ~glucose,
      type = "scatter",
      mode = "lines",
      line = list(color = unname(palette[[id]]), width = 1),
      name = id,
      legendgroup = id,
      showlegend = show_legend,
      text = ~Tooltip,
      hoverinfo = "text"
    )
  }
  imputed <- data[data$imputed_flag %in% TRUE, , drop = FALSE]
  if (nrow(imputed)) {
    plot <- plotly::add_trace(
      plot,
      data = imputed,
      x = ~timestamp,
      y = ~glucose,
      type = "scatter",
      mode = "markers",
      marker = list(color = "#C95142", size = 6),
      name = "Imputed",
      text = ~Tooltip,
      hoverinfo = "text"
    )
  }
  plot <- plotly::layout(
    plot,
    xaxis = list(title = ""),
    yaxis = list(title = "Glucose (mg/dL)"),
    shapes = plotly_threshold_shapes(thresholds),
    legend = list(orientation = "h", x = 0, y = -0.18),
    margin = list(t = 24, r = 24, b = 86, l = 64)
  )
  maybe_to_webgl(plot, nrow(data))
}

create_daily_overlay_plotly <- function(
  data,
  thresholds = default_cgm_thresholds(),
  participant = "",
  group = "",
  day = "",
  time_window = default_time_window(),
  max_points_per_participant = Inf,
  date_legend_limit = daily_overlay_date_legend_limit()
) {
  day <- normalize_plot_days(day)
  plot_data <- prepare_time_of_day_plot_data(
    data,
    participant = participant,
    group = group,
    day = day,
    time_window = time_window,
    max_points_per_participant = max_points_per_participant
  )
  if (!nrow(plot_data)) {
    return(plotly_empty_plot(plot_empty_message("daily_overlay")))
  }
  show_date_legend <- daily_overlay_legend_visible(length(unique(plot_data$date)), date_legend_limit)
  palette <- clinical_discrete_palette(plot_data$date)
  shown_dates <- character()
  plot <- plotly::plot_ly()
  for (group_name in unique(as.character(plot_data$plot_group))) {
    group_data <- plot_data[as.character(plot_data$plot_group) == group_name, , drop = FALSE]
    date <- as.character(group_data$date[[1L]])
    show_legend <- show_date_legend && !date %in% shown_dates
    shown_dates <- unique(c(shown_dates, date))
    plot <- plotly::add_trace(
      plot,
      data = group_data,
      x = ~time_minutes,
      y = ~glucose,
      type = "scatter",
      mode = "lines",
      line = list(color = unname(palette[[date]]), width = 1),
      name = date,
      legendgroup = date,
      showlegend = show_legend,
      text = ~Tooltip,
      hoverinfo = "text"
    )
  }
  imputed <- plot_data[plot_data$imputed_flag %in% TRUE, , drop = FALSE]
  if (nrow(imputed)) {
    plot <- plotly::add_trace(
      plot,
      data = imputed,
      x = ~time_minutes,
      y = ~glucose,
      type = "scatter",
      mode = "markers",
      marker = list(color = "#C95142", size = 6),
      name = "Imputed",
      text = ~Tooltip,
      hoverinfo = "text"
    )
  }
  plot <- plotly::layout(
    plot,
    xaxis = list(
      title = "Time of day",
      tickvals = seq(0, 1440, by = 240),
      ticktext = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00", "24:00"),
      range = c(0, 1440)
    ),
    yaxis = list(title = "Glucose (mg/dL)"),
    shapes = plotly_threshold_shapes(thresholds),
    legend = list(orientation = "h", x = 0, y = -0.18),
    margin = list(t = 24, r = 24, b = 86, l = 64)
  )
  maybe_to_webgl(plot, nrow(plot_data))
}

prepare_agp_summary_data <- function(
  data,
  participant = "",
  group = "",
  time_window = default_time_window(),
  bin_minutes = 30
) {
  plot_data <- prepare_time_of_day_plot_data(data, participant = participant, group = group, time_window = time_window)
  plot_data <- plot_data[!is.na(plot_data$glucose) & !is.na(plot_data$time_minutes), , drop = FALSE]
  if (!nrow(plot_data)) {
    return(data.frame())
  }

  plot_data$time_bin <- floor(plot_data$time_minutes / bin_minutes) * bin_minutes + bin_minutes / 2
  dt <- data.table::as.data.table(plot_data)
  out <- dt[, {
    glucose_values <- get("glucose")
    quantiles <- stats::quantile(glucose_values, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE, names = FALSE)
    list(
      q05 = quantiles[[1L]],
      q25 = quantiles[[2L]],
      median = quantiles[[3L]],
      q75 = quantiles[[4L]],
      q95 = quantiles[[5L]],
      n = length(glucose_values)
    )
  }, by = .(time_minutes = time_bin)][order(time_minutes)]
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Create an AGP-style CGM summary plot
#'
#' @param data Standardized CGM data.
#' @param thresholds Named glucose thresholds in mg/dL.
#' @param participant Subject ID filter.
#' @param group Group filter.
#' @param bin_minutes Time-of-day bin width in minutes.
#'
#' @return A ggplot object.
#' @noRd
create_agp_summary_plot <- function(
  data,
  thresholds = default_cgm_thresholds(),
  participant = "",
  group = "",
  time_window = default_time_window(),
  bin_minutes = 30
) {
  agp <- prepare_agp_summary_data(
    data,
    participant = participant,
    group = group,
    time_window = time_window,
    bin_minutes = bin_minutes
  )
  if (!nrow(agp)) {
    return(empty_plot(plot_empty_message("agp")))
  }

  target_label <- target_range_label(thresholds)
  target_data <- data.frame(
    xmin = 0,
    xmax = 1440,
    ymin = thresholds$tir_lower,
    ymax = thresholds$tir_upper,
    range = target_label,
    stringsAsFactors = FALSE
  )
  agp$Tooltip_median <- agp_hover_text(agp, "median")

  ggplot2::ggplot(agp, ggplot2::aes(x = time_minutes)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q05, ymax = q95, fill = "5th-95th percentile band"),
      alpha = 0.68
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = q25, ymax = q75, fill = "25th-75th percentile band"),
      alpha = 0.84
    ) +
    ggplot2::geom_rect(
      data = target_data,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = range
      ),
      inherit.aes = FALSE,
      alpha = 0.42
    ) +
    ggplot2::geom_line(ggplot2::aes(y = median, color = "Median glucose"), linewidth = 1.05) +
    suppressWarnings(ggplot2::geom_point(
      ggplot2::aes(y = median, text = Tooltip_median),
      inherit.aes = TRUE,
      show.legend = FALSE,
      alpha = 0.01,
      size = 1.2
    )) +
    ggplot2::geom_hline(yintercept = thresholds$tir_lower, linetype = "dotted", color = "#5F6368", alpha = 0.65) +
    ggplot2::geom_hline(yintercept = thresholds$tir_upper, linetype = "dotted", color = "#5F6368", alpha = 0.65) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = c(
        stats::setNames("#DDEDE6", target_label),
        "5th-95th percentile band" = "#86C7B5",
        "25th-75th percentile band" = "#22B883"
      ),
      breaks = c(target_label, "5th-95th percentile band", "25th-75th percentile band")
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c("Median glucose" = "#176F50"),
      breaks = "Median glucose"
    ) +
    time_of_day_scale() +
    ggplot2::labs(x = "Time of day", y = "Glucose (mg/dL)") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1, override.aes = list(alpha = c(0.42, 0.68, 0.84))),
      color = ggplot2::guide_legend(order = 2)
    ) +
    clinical_plot_theme(legend_position = "top") +
    ggplot2::theme(
      legend.box = "horizontal"
    )
}

tir_long_data <- function(metrics) {
  metrics <- filter_metrics_by_period(metrics, default_time_window())
  if (!nrow(metrics)) {
    return(data.frame())
  }
  data.frame(
    id = rep(metrics$id, each = 3L),
    range = rep(c("Below range", "In range", "Above range"), times = nrow(metrics)),
    percent = c(rbind(metrics$tbr_percent, metrics$tir_percent, metrics$tar_percent)),
    stringsAsFactors = FALSE
  )
}

#' Create a TIR stacked bar plot
#'
#' @param metrics Metric summary data frame from `compute_core_metrics()`.
#'
#' @return A ggplot object.
#' @noRd
create_tir_plot <- function(metrics) {
  plot_data <- tir_long_data(metrics)
  ggplot2::ggplot(plot_data, ggplot2::aes(x = id, y = percent, fill = range)) +
    ggplot2::geom_col(width = 0.7, color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_manual(
      values = c("Below range" = "#2F80ED", "In range" = "#219653", "Above range" = "#EB5757")
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 100)) +
    ggplot2::labs(x = "Subject ID", y = "Percent of readings", fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}
