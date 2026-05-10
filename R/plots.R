#' Create a CGM trace plot
#'
#' @param data Standardized CGM data.
#' @param thresholds Named glucose thresholds in mg/dL.
#' @param participant_id Optional participant ID filter.
#'
#' @return A ggplot object.
#' @export
create_trace_plot <- function(
  data,
  thresholds = default_cgm_thresholds(),
  participant_id = NULL,
  max_points_per_participant = Inf
) {
  if (!is.null(participant_id) && nzchar(participant_id)) {
    data <- data[data$id == participant_id, , drop = FALSE]
  }
  data <- data[is_finite_cgm_timestamp(data$timestamp) & !is.na(data$glucose), , drop = FALSE]
  data <- prepare_plot_display_data(data, max_points_per_participant = max_points_per_participant)
  if (!nrow(data)) {
    return(empty_plot("No valid timestamps available"))
  }

  ggplot2::ggplot(data, ggplot2::aes(x = timestamp, y = glucose, color = id, group = id)) +
    ggplot2::geom_line(alpha = 0.8, linewidth = 0.35, na.rm = TRUE) +
    ggplot2::geom_point(
      data = data[data$imputed_flag %in% TRUE, , drop = FALSE],
      ggplot2::aes(x = timestamp, y = glucose),
      inherit.aes = FALSE,
      size = 1.6,
      alpha = 0.9,
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = thresholds$tir_lower, linetype = "dashed", color = "#B42318") +
    ggplot2::geom_hline(yintercept = thresholds$tir_upper, linetype = "dashed", color = "#B42318") +
    ggplot2::labs(x = NULL, y = "Glucose (mg/dL)", color = "Subject ID") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
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

select_evenly_spaced_rows <- function(idx, n) {
  if (!length(idx) || n <= 0L) {
    return(integer())
  }
  if (length(idx) <= n) {
    return(idx)
  }
  idx[unique(round(seq(1, length(idx), length.out = n)))]
}

downsample_one_plot_group <- function(data, max_points) {
  if (!is.finite(max_points) || nrow(data) <= max_points) {
    return(data)
  }

  max_points <- as.integer(max_points)
  data <- data[order(data$timestamp), , drop = FALSE]
  row_idx <- seq_len(nrow(data))
  imputed_idx <- row_idx[data$imputed_flag %in% TRUE]
  keep_imputed <- select_evenly_spaced_rows(imputed_idx, max_points)
  remaining_slots <- max_points - length(keep_imputed)
  candidate_idx <- setdiff(row_idx, keep_imputed)
  keep_candidates <- select_evenly_spaced_rows(candidate_idx, remaining_slots)
  keep <- sort(unique(c(keep_imputed, keep_candidates)))
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
  filter_select_choices(sort(clean_filter_values(values)), all_label = "All")
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

filter_plot_data <- function(data, participant = "", group = "", visit = "", day = "") {
  participant <- normalize_filter_value(participant)
  group <- normalize_filter_value(group)
  visit <- normalize_filter_value(visit)
  day <- normalize_plot_days(day)

  if (nzchar(participant)) {
    data <- data[data$id == participant, , drop = FALSE]
  }
  if ("group" %in% names(data) && nzchar(group)) {
    data <- data[data$group == group, , drop = FALSE]
  }
  if ("visit" %in% names(data) && nzchar(visit)) {
    data <- data[data$visit == visit, , drop = FALSE]
  }
  if (!identical(day, all_filter_value())) {
    data <- data[as.character(as.Date(data$timestamp)) %in% day, , drop = FALSE]
  }
  data
}

available_plot_days <- function(data, participant = "", group = "", visit = "") {
  data <- filter_plot_data(data, participant = participant, group = group, visit = visit)
  sort(unique(as.character(as.Date(data$timestamp[!is.na(data$timestamp)]))))
}

plot_day_filter_choices <- function(data, participant = "", group = "", visit = "") {
  filter_select_choices(available_plot_days(data, participant = participant, group = group, visit = visit), all_label = "All days")
}

is_finite_cgm_timestamp <- function(timestamp) {
  !is.na(timestamp) & is.finite(as.numeric(timestamp))
}

empty_plot <- function(message = "No data available") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message) +
    ggplot2::theme_void()
}

#' Prepare CGM data for time-of-day plots
#'
#' @param data Standardized CGM data.
#' @param participant Subject ID filter.
#' @param group Group filter.
#' @param visit Visit filter.
#' @param day Date filter as `YYYY-MM-DD`.
#'
#' @return A data frame with time-of-day plotting columns.
#' @export
prepare_time_of_day_plot_data <- function(
  data,
  participant = "",
  group = "",
  visit = "",
  day = "",
  max_points_per_participant = Inf
) {
  data <- filter_plot_data(data, participant = participant, group = group, visit = visit, day = day)
  data <- data[is_finite_cgm_timestamp(data$timestamp), , drop = FALSE]
  data <- prepare_plot_display_data(data, max_points_per_participant = max_points_per_participant)
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
#' @param visit Visit filter.
#' @param day Date filter as `YYYY-MM-DD`.
#'
#' @return A ggplot object.
#' @export
create_daily_overlay_plot <- function(
  data,
  thresholds = default_cgm_thresholds(),
  participant = "",
  group = "",
  visit = "",
  day = "",
  max_points_per_participant = Inf,
  date_legend_limit = 14L
) {
  day <- normalize_plot_days(day)
  plot_data <- prepare_time_of_day_plot_data(
    data,
    participant = participant,
    group = group,
    visit = visit,
    day = day,
    max_points_per_participant = max_points_per_participant
  )
  if (!nrow(plot_data)) {
    return(empty_plot())
  }

  show_date_legend <- length(unique(plot_data$date)) <= date_legend_limit
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = time_minutes, y = glucose, color = date, group = plot_group)
  ) +
    ggplot2::geom_line(alpha = 0.75, linewidth = 0.35, na.rm = TRUE) +
    ggplot2::geom_point(
      data = plot_data[plot_data$imputed_flag %in% TRUE, , drop = FALSE],
      ggplot2::aes(x = time_minutes, y = glucose),
      inherit.aes = FALSE,
      size = 1.6,
      alpha = 0.9,
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = thresholds$tir_lower, linetype = "dashed", color = "#B42318") +
    ggplot2::geom_hline(yintercept = thresholds$tir_upper, linetype = "dashed", color = "#B42318") +
    time_of_day_scale() +
    ggplot2::labs(x = "Time of day", y = "Glucose (mg/dL)", color = "Date") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = if (show_date_legend) "bottom" else "none")

  if (length(unique(plot_data$id)) > 1L) {
    plot <- plot + ggplot2::facet_wrap(~id)
  }
  plot
}

prepare_agp_summary_data <- function(
  data,
  participant = "",
  group = "",
  visit = "",
  bin_minutes = 30
) {
  plot_data <- prepare_time_of_day_plot_data(data, participant = participant, group = group, visit = visit)
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
#' @param visit Visit filter.
#' @param bin_minutes Time-of-day bin width in minutes.
#'
#' @return A ggplot object.
#' @export
create_agp_summary_plot <- function(
  data,
  thresholds = default_cgm_thresholds(),
  participant = "",
  group = "",
  visit = "",
  bin_minutes = 30
) {
  agp <- prepare_agp_summary_data(
    data,
    participant = participant,
    group = group,
    visit = visit,
    bin_minutes = bin_minutes
  )
  if (!nrow(agp)) {
    return(empty_plot())
  }

  target_label <- target_range_label(thresholds)
  target_data <- data.frame(
    xmin = -Inf,
    xmax = Inf,
    ymin = thresholds$tir_lower,
    ymax = thresholds$tir_upper,
    range = target_label,
    stringsAsFactors = FALSE
  )

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
    ggplot2::geom_hline(yintercept = thresholds$tir_lower, linetype = "dotted", color = "#5F6368", alpha = 0.65) +
    ggplot2::geom_hline(yintercept = thresholds$tir_upper, linetype = "dotted", color = "#5F6368", alpha = 0.65) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = c(
        stats::setNames("#7B8288", target_label),
        "5th-95th percentile band" = "#7BC8F6",
        "25th-75th percentile band" = "#1F78B4"
      ),
      breaks = c(target_label, "5th-95th percentile band", "25th-75th percentile band")
    ) +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c("Median glucose" = "#111111"),
      breaks = "Median glucose"
    ) +
    time_of_day_scale() +
    ggplot2::labs(x = "Time of day", y = "Glucose (mg/dL)") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1, override.aes = list(alpha = c(0.42, 0.68, 0.84))),
      color = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      legend.box = "horizontal",
      plot.margin = ggplot2::margin(t = 8, r = 12, b = 12, l = 8)
    )
}

tir_long_data <- function(metrics) {
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
#' @export
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
