test_that("plot helpers return ggplot objects", {
  data <- data.frame(
    id = rep("A", 3),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:10:00"
    )),
    glucose = c(80, 120, 190),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  metrics <- compute_core_metrics(data)

  expect_s3_class(create_trace_plot(data), "ggplot")
  expect_s3_class(create_tir_plot(metrics), "ggplot")
  expect_s3_class(create_missingness_timeline_plot(data), "ggplot")
  expect_s3_class(create_daily_overlay_plot(data), "ggplot")
  expect_s3_class(create_agp_summary_plot(data, bin_minutes = 5), "ggplot")
})

test_that("plot helpers return safe empty plots when timestamps are invalid", {
  data <- data.frame(
    id = rep("A", 2),
    timestamp = as.POSIXct(c(NA_real_, NA_real_), origin = "1970-01-01", tz = "UTC"),
    glucose = c(80, 120),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  expect_s3_class(create_trace_plot(data), "ggplot")
  expect_s3_class(create_daily_overlay_plot(data), "ggplot")
  expect_s3_class(create_agp_summary_plot(data), "ggplot")
  expect_s3_class(create_missingness_timeline_plot(data), "ggplot")
})

test_that("plot empty states use plot-specific messages", {
  data <- data.frame(
    id = rep("A", 2),
    timestamp = as.POSIXct(c(NA_real_, NA_real_), origin = "1970-01-01", tz = "UTC"),
    glucose = c(80, 120),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  plot_label <- function(plot) ggplot2::ggplot_build(plot)$data[[1]]$label[[1]]

  expect_equal(plot_empty_message("trace"), "No glucose readings are available for the selected plot filters.")
  expect_equal(plot_label(create_trace_plot(data)), plot_empty_message("trace"))
  expect_equal(plot_label(create_daily_overlay_plot(data)), plot_empty_message("daily_overlay"))
  expect_equal(plot_label(create_agp_summary_plot(data)), plot_empty_message("agp"))
})

test_that("plot guidance and status chips render concise clinical notes", {
  expect_match(plot_guidance_text("trace"), "Timeline")
  expect_match(plot_guidance_text("daily_overlay"), "Within-day")
  expect_match(plot_guidance_text("agp"), "percentile bands")
  expect_null(plot_status_chip_ui(""))

  html <- paste(as.character(plot_note_row_ui(
    plot_status_chip_ui(plot_guidance_text("trace")),
    plot_status_chip_ui("Interactive plot optimized for display.")
  )), collapse = "\n")

  expect_true(grepl("cgm-plot-note-row", html, fixed = TRUE))
  expect_true(grepl("cgm-plot-status-chip", html, fixed = TRUE))
  expect_true(grepl("Interactive plot optimized", html, fixed = TRUE))
})

test_that("time-of-day plot data keeps imputed rows identifiable", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00",
      "2026-05-06 08:05:00"
    )),
    glucose = c(80, 120, 90, 130),
    units = "mg/dL",
    device = NA_character_,
    group = "Control",
    source_file = NA_character_,
    imputed_flag = c(FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  plot_data <- prepare_time_of_day_plot_data(data)

  expect_equal(plot_data$date, c("2026-05-05", "2026-05-05", "2026-05-06", "2026-05-06"))
  expect_equal(plot_data$time_minutes, c(480, 485, 480, 485))
  expect_equal(sum(plot_data$imputed_flag), 1)
  expect_true(all(grepl("Subject ID: A", plot_data$Tooltip, fixed = TRUE)))
  expect_true(any(grepl("Imputed: Yes", plot_data$Tooltip, fixed = TRUE)))
})

test_that("trace and daily overlay plots include Plotly hover text context", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 2),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00",
      "2026-05-06 08:05:00"
    )),
    glucose = c(80, 120, 90, 130),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = c(FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  trace_data <- ggplot2::ggplot_build(create_trace_plot(data))$data[[1]]
  daily_data <- ggplot2::ggplot_build(create_daily_overlay_plot(data))$data[[1]]

  expect_true("text" %in% names(trace_data))
  expect_true("text" %in% names(daily_data))
  expect_true(any(grepl("Subject ID: A", trace_data$text, fixed = TRUE)))
  expect_true(any(grepl("Timestamp:", trace_data$text, fixed = TRUE)))
  expect_true(any(grepl("Glucose:", trace_data$text, fixed = TRUE)))
  expect_true(any(grepl("Date:", daily_data$text, fixed = TRUE)))
  expect_true(any(grepl("Time:", daily_data$text, fixed = TRUE)))
})

test_that("plot display data respects point budgets and keeps imputed rows when possible", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 10),
    timestamp = parse_cgm_timestamp(rep(sprintf("2026-05-05 08:%02d:00", seq(0, 45, by = 5)), 2)),
    glucose = seq_len(20),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = rep(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE), 2),
    stringsAsFactors = FALSE
  )

  display <- prepare_plot_display_data(data, max_points_per_participant = 4)
  full <- prepare_plot_display_data(data, max_points_per_participant = Inf)

  expect_true(all(table(display$id) <= 4))
  expect_equal(nrow(full), nrow(data))
  expect_true(any(display$imputed_flag[display$id == "A"]))
  expect_true(any(display$imputed_flag[display$id == "B"]))
})

test_that("adaptive plot budgets reduce large displays and leave small data alone", {
  small <- data.frame(
    id = rep("A", 10),
    timestamp = parse_cgm_timestamp("2026-05-05 00:00:00") + seq(0, by = 300, length.out = 10),
    glucose = seq_len(10) + 100,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  large <- data.frame(
    id = rep(c("A", "B"), each = 200),
    timestamp = parse_cgm_timestamp("2026-05-05 00:00:00") + rep(seq(0, by = 300, length.out = 200), 2),
    glucose = seq_len(400) + 100,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  large$imputed_flag[c(50, 250)] <- TRUE

  small_budget <- adaptive_plot_max_points_per_subject(small, target_total_points = 25)
  large_budget <- adaptive_plot_max_points_per_subject(large, target_total_points = 100, minimum_per_subject = 10)
  large_display <- prepare_plot_display_data(large, max_points_per_participant = large_budget)

  expect_true(is.infinite(small_budget))
  expect_true(is.finite(large_budget))
  expect_lt(nrow(large_display), nrow(large))
  expect_true(all(table(large_display$id) <= large_budget))
  expect_true(all(large$timestamp[large$imputed_flag] %in% large_display$timestamp[large_display$imputed_flag]))
})

test_that("plot selection summary reports displayed rows only when optimized", {
  data <- data.frame(
    id = rep("A", 20),
    timestamp = parse_cgm_timestamp("2026-05-05 00:00:00") + seq(0, by = 300, length.out = 20),
    glucose = seq_len(20) + 100,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  full_summary <- plot_selection_summary(data, displayed_rows = 20)
  optimized_summary <- plot_selection_summary(data, displayed_rows = 8)

  expect_false("Rows displayed" %in% full_summary$Label)
  expect_equal(optimized_summary$Value[optimized_summary$Label == "Rows plotted"], "20")
  expect_equal(optimized_summary$Value[optimized_summary$Label == "Rows displayed"], "8")
})

test_that("plot detail choices map to point budgets", {
  expect_lt(plot_detail_max_points("fast"), plot_detail_max_points("balanced"))
  expect_true(is.infinite(plot_detail_max_points("full")))
})

test_that("plot filters support All sentinel for participant, group, and day", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 2),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-06 08:00:00",
      "2026-05-05 08:00:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(80, 120, 90, 130),
    units = "mg/dL",
    device = NA_character_,
    group = rep(c("Control", "Treatment"), each = 2),
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  all_data <- filter_plot_data(
    data,
    participant = all_filter_value(),
    group = all_filter_value(),
    day = all_filter_value()
  )
  filtered <- filter_plot_data(data, participant = "A", group = "Control", day = "2026-05-05")
  daytime <- filter_plot_data(data, time_window = "daytime")
  nighttime <- filter_plot_data(data, time_window = "nighttime")

  expect_equal(nrow(all_data), nrow(data))
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$id, "A")
  expect_equal(nrow(daytime), nrow(data))
  expect_equal(nrow(nighttime), 0)
})

test_that("plot time window filtering updates summaries and filenames", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 01:00:00",
      "2026-05-05 05:55:00",
      "2026-05-05 06:00:00",
      "2026-05-05 23:55:00"
    )),
    glucose = c(90, 110, 130, 150),
    stringsAsFactors = FALSE
  )

  summary <- plot_selection_summary(data, time_window = "nighttime")
  filename <- plot_download_filename(data, plot_type = "trace", time_window = "nighttime")

  expect_equal(summary$Value[summary$Label == "Rows plotted"], "2")
  expect_equal(summary$Value[summary$Label == "Time window"], "Nighttime (00:00-05:59)")
  expect_match(filename, "time-nighttime")
})

test_that("daily overlay day helpers normalize default All days", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-06 08:00:00",
      "2026-05-07 08:00:00",
      "2026-05-08 08:00:00"
    )),
    glucose = c(80, 120, 90, 130),
    stringsAsFactors = FALSE
  )

  choices <- plot_day_filter_choices(data)
  all_days <- filter_plot_data(data, day = normalize_plot_day(NULL))
  blank_days <- filter_plot_data(data, day = normalize_plot_day(""))
  sentinel_days <- filter_plot_data(data, day = normalize_plot_day(all_filter_value()))
  selected_day <- filter_plot_data(data, day = normalize_plot_day("2026-05-06"))
  selected_days <- filter_plot_data(data, day = normalize_plot_days(c("2026-05-06", "2026-05-08")))
  all_overrides_days <- filter_plot_data(data, day = normalize_plot_days(c(all_filter_value(), "2026-05-06")))

  expect_equal(normalize_plot_day(NULL), all_filter_value())
  expect_equal(normalize_plot_day(""), all_filter_value())
  expect_equal(normalize_plot_day(all_filter_value()), all_filter_value())
  expect_equal(normalize_plot_day("2026-05-06"), "2026-05-06")
  expect_equal(normalize_plot_days(c("2026-05-08", "2026-05-06")), c("2026-05-06", "2026-05-08"))
  expect_equal(normalize_plot_days(c(all_filter_value(), "2026-05-06")), all_filter_value())
  expect_equal(plot_day_cache_key(c("2026-05-08", "2026-05-06")), plot_day_cache_key(c("2026-05-06", "2026-05-08")))
  expect_equal(names(choices)[[1L]], "All days")
  expect_equal(unname(choices)[[1L]], all_filter_value())
  expect_equal(preserve_plot_day_selection(NULL, choices), all_filter_value())
  expect_equal(preserve_plot_day_selection(c("2026-05-08", "2026-05-06"), choices), c("2026-05-06", "2026-05-08"))
  expect_equal(preserve_plot_day_selection(c(all_filter_value(), "2026-05-06"), choices), all_filter_value())
  expect_equal(
    preserve_plot_day_selection(character(), choices, previous = c("2026-05-06", "2026-05-08")),
    c("2026-05-06", "2026-05-08")
  )
  expect_equal(nrow(all_days), nrow(data))
  expect_equal(blank_days, all_days)
  expect_equal(sentinel_days, all_days)
  expect_equal(nrow(selected_day), 1)
  expect_equal(nrow(selected_days), 2)
  expect_equal(all_overrides_days, all_days)
})

test_that("plot subject filter choices are stable and user-facing", {
  data <- data.frame(
    id = c("B", "A", NA, "", " A "),
    timestamp = parse_cgm_timestamp(rep("2026-05-05 08:00:00", 5)),
    glucose = 100,
    stringsAsFactors = FALSE
  )

  choices <- plot_subject_filter_choices(data)

  expect_equal(names(choices)[[1L]], "A")
  expect_equal(unname(choices), c("A", "B", all_filter_value()))
  expect_equal(preserve_subject_filter_selection("A", choices, data$id), "A")
  expect_equal(preserve_subject_filter_selection("Missing", choices, data$id), "A")
  expect_equal(preserve_subject_filter_selection(all_filter_value(), choices, data$id), all_filter_value())
})

test_that("daily overlay remains available for one filename-derived subject", {
  data <- data.frame(
    id = rep("one_subject", 4),
    id_source = subject_id_source_filename(),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00",
      "2026-05-06 08:05:00"
    )),
    glucose = c(80, 120, 90, 130),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = "one_subject.csv",
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  expect_false(subject_id_filter_available(data))
  expect_s3_class(create_daily_overlay_plot(data), "ggplot")
})

test_that("daily overlay hides very large date legends", {
  timestamps <- parse_cgm_timestamp(paste0("2026-05-", sprintf("%02d", 1:15), " 08:00:00"))
  data <- data.frame(
    id = rep("A", length(timestamps)),
    timestamp = timestamps,
    glucose = seq_along(timestamps) + 100,
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  many_days <- create_daily_overlay_plot(data, date_legend_limit = 14)
  few_days <- create_daily_overlay_plot(data[seq_len(2), , drop = FALSE], date_legend_limit = 14)
  selected_day <- create_daily_overlay_plot(data, day = "2026-05-02", date_legend_limit = 14)
  selected_days <- create_daily_overlay_plot(data, day = c("2026-05-02", "2026-05-03"), date_legend_limit = 14)

  expect_equal(many_days$theme$legend.position, "none")
  expect_equal(few_days$theme$legend.position, "bottom")
  expect_equal(selected_day$theme$legend.position, "bottom")
  expect_equal(selected_days$theme$legend.position, "bottom")
  expect_false(daily_overlay_legend_visible(15, date_legend_limit = 14))
  expect_true(daily_overlay_legend_visible(14, date_legend_limit = 14))
})

test_that("daily overlay summary and notes describe many-day legends", {
  timestamps <- parse_cgm_timestamp(paste0("2026-05-", sprintf("%02d", 1:15), " 08:00:00"))
  data <- data.frame(
    id = rep("A", length(timestamps)),
    timestamp = timestamps,
    glucose = seq_along(timestamps) + 100,
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  selected <- plot_filtered_data(data, plot_type = "daily_overlay", day = c("2026-05-02", "2026-05-03"))
  all_days <- plot_filtered_data(data, plot_type = "daily_overlay", day = all_filter_value())
  selected_summary <- daily_overlay_summary_rows(selected, day = c("2026-05-02", "2026-05-03"))
  all_summary <- daily_overlay_summary_rows(all_days, day = all_filter_value())

  expect_equal(selected_summary$Value[selected_summary$Label == "Day selection"], "Selected days")
  expect_equal(selected_summary$Value[selected_summary$Label == "Days shown"], "2")
  expect_equal(selected_summary$Value[selected_summary$Label == "Date legend"], "Shown")
  expect_equal(all_summary$Value[all_summary$Label == "Day selection"], "All days")
  expect_equal(all_summary$Value[all_summary$Label == "Date legend"], "Hidden for readability")
  expect_true(grepl("legend is hidden", daily_overlay_legend_note(all_days), fixed = TRUE))
  expect_equal(daily_overlay_legend_note(selected), "")
})

test_that("plot download filenames include selected filters safely", {
  data <- data.frame(
    id = rep(c("Subject A", "Subject B"), each = 2),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-06 08:00:00",
      "2026-05-05 08:00:00",
      "2026-05-07 08:00:00"
    )),
    glucose = c(80, 120, 90, 130),
    group = rep(c("Control Group", "Treatment Group"), each = 2),
    stringsAsFactors = FALSE
  )

  daily_name <- plot_download_filename(
    data,
    plot_type = "daily_overlay",
    participant = "Subject A",
    group = "Control Group",
    day = c("2026-05-06", "2026-05-05")
  )
  trace_name <- plot_download_filename(data, plot_type = "trace")

  expect_equal(
    daily_name,
    "cgm_daily-overlay_subject-subject-a_group-control-group_time-full-day_days-2026-05-05_to_2026-05-06.png"
  )
  expect_match(trace_name, "^cgm_trace_all-subjects_time-full-day_dates-2026-05-05-to-2026-05-07\\.png$")
  expect_false(grepl("[^A-Za-z0-9._-]", daily_name))
})

test_that("group plot filters require usable non-missing values", {
  data <- data.frame(
    id = c("A", "B", "C"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:10:00"
    )),
    glucose = c(80, 120, 90),
    units = "mg/dL",
    device = NA_character_,
    group = c("Control", "Treatment", NA),
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  expect_true(plot_filter_available(data, "group", min_values = 2))
  expect_false(plot_filter_available(data, "missing_column", min_values = 2))
})

test_that("AGP summary data produces time-of-day quantiles", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 2),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:30:00",
      "2026-05-06 08:00:00",
      "2026-05-06 08:30:00"
    )),
    glucose = c(80, 120, 100, 140),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  agp <- prepare_agp_summary_data(data, bin_minutes = 30)
  empty_night <- prepare_agp_summary_data(data, time_window = "nighttime", bin_minutes = 30)

  expect_equal(agp$time_minutes, c(495, 525))
  expect_true(all(c("q05", "q25", "median", "q75", "q95", "n") %in% names(agp)))
  expect_equal(agp$n, c(2, 2))
  expect_equal(nrow(empty_night), 0)
})

test_that("trace time-window plots break lines by subject and date", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 01:00:00",
      "2026-05-05 01:05:00",
      "2026-05-06 01:00:00",
      "2026-05-06 01:05:00"
    )),
    glucose = c(90, 95, 100, 105),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  full_groups <- unique(ggplot2::ggplot_build(create_trace_plot(data))$data[[1]]$group)
  night_groups <- unique(ggplot2::ggplot_build(create_trace_plot(data, time_window = "nighttime"))$data[[1]]$group)

  expect_length(full_groups, 1)
  expect_length(night_groups, 2)
})

test_that("AGP summary is unchanged by trace display downsampling helpers", {
  data <- data.frame(
    id = rep("A", 8),
    timestamp = parse_cgm_timestamp(sprintf("2026-05-05 %02d:00:00", 0:7)),
    glucose = seq(80, 150, length.out = 8),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  agp_full <- prepare_agp_summary_data(data, bin_minutes = 60)
  invisible(prepare_plot_display_data(data, max_points_per_participant = 2))
  agp_after_display <- prepare_agp_summary_data(data, bin_minutes = 60)

  expect_equal(agp_after_display, agp_full)
})

test_that("AGP plot has legend-bearing percentile bands and median line", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 2),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:30:00",
      "2026-05-06 08:00:00",
      "2026-05-06 08:30:00"
    )),
    glucose = c(80, 120, 100, 140),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  plot <- create_agp_summary_plot(data, bin_minutes = 30)
  fills <- unlist(lapply(plot$layers, function(layer) layer$mapping$fill), use.names = FALSE)
  colors <- unlist(lapply(plot$layers, function(layer) layer$mapping$colour %||% layer$mapping$color), use.names = FALSE)
  fill_scale <- plot$scales$get_scales("fill")
  color_scale <- plot$scales$get_scales("colour")
  layer_geoms <- vapply(plot$layers, function(layer) class(layer$geom)[[1L]], character(1))
  ribbon_idx <- which(layer_geoms == "GeomRibbon")
  rect_idx <- which(layer_geoms == "GeomRect")
  line_idx <- which(layer_geoms == "GeomLine")

  expect_true("5th-95th percentile band" %in% fills)
  expect_true("25th-75th percentile band" %in% fills)
  expect_true("Target range (70-180 mg/dL)" %in% fill_scale$breaks)
  expect_true("Median glucose" %in% colors)
  expect_equal(unname(fill_scale$palette(3)), c("#DDEDE6", "#86C7B5", "#22B883"))
  expect_equal(unname(color_scale$palette(1)), "#176F50")
  expect_length(ribbon_idx, 2)
  expect_length(rect_idx, 1)
  expect_true(length(line_idx) >= 1)
  expect_true(all(ribbon_idx < rect_idx))
  expect_true(all(rect_idx < line_idx))
  expect_equal(plot$layers[[rect_idx]]$aes_params$alpha, 0.42)
  expect_true(any(vapply(ggplot2::ggplot_build(plot)$data, function(layer_data) {
    "text" %in% names(layer_data) && any(grepl("Time of day:", layer_data$text, fixed = TRUE))
  }, logical(1))))
})

test_that("plots module rendering does not depend on metrics or complexity results", {
  data <- data.frame(
    id = rep("A", 3),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:10:00"
    )),
    glucose = c(100, 110, 120),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  failing_metrics <- shiny::reactive(stop("metrics should not be needed for plot rendering", call. = FALSE))

  shiny::testServer(
    plots_module_server,
    args = list(
      standardized = shiny::reactive(data),
      metrics = failing_metrics,
      settings = shiny::reactive(create_reproducibility_settings()),
      active_tab = function() "plots"
    ),
    {
      session$setInputs(plot_type = "trace")
      expect_s3_class(active_plot(), "ggplot")

      session$setInputs(plot_type = "agp")
      expect_s3_class(active_plot(), "ggplot")
    }
  )
})

test_that("plot filter helper selects relevant controls by plot type", {
  expect_equal(plot_filter_layout_order("trace"), c("time_window_filter", "subject_filter", "group_filter"))
  expect_equal(plot_filter_layout_order("agp"), c("time_window_filter", "subject_filter", "group_filter"))
  expect_equal(plot_filter_layout_order("daily_overlay"), c("time_window_filter", "day_filter", "subject_filter", "group_filter"))

  html <- paste(as.character(plot_filter_layout_ui(shiny::NS("plots"), "daily_overlay")), collapse = "\n")
  expect_true(grepl("cgm-plots-filter-bar", html, fixed = TRUE))
  expect_true(grepl("plots-plot_type", html, fixed = TRUE))
  expect_true(grepl("plots-time_window_filter", html, fixed = TRUE))
  expect_true(grepl("plots-day_filter", html, fixed = TRUE))
  expect_true(grepl("plots-subject_filter", html, fixed = TRUE))
})

test_that("plots module uses normalized day values for active plot cache", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-06 08:00:00",
      "2026-05-07 08:00:00",
      "2026-05-08 08:00:00"
    )),
    glucose = c(100, 110, 120, 130),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    plots_module_server,
    args = list(
      standardized = shiny::reactive(data),
      metrics = shiny::reactive(data.frame()),
      settings = shiny::reactive(create_reproducibility_settings()),
      active_tab = function() "plots"
    ),
    {
      session$setInputs(plot_type = "daily_overlay")
      expect_equal(normalized_day(), all_filter_value())

      session$setInputs(day = c("2026-05-08", "2026-05-06"))
      expect_equal(normalized_day(), c("2026-05-06", "2026-05-08"))
      expect_equal(plot_day_cache_key(normalized_day()), "2026-05-06|2026-05-08")

      session$setInputs(day = c(all_filter_value(), "2026-05-06"))
      expect_equal(normalized_day(), all_filter_value())
    }
  )
})

test_that("plotly legend cleanup removes ggplotly trace suffixes", {
  expect_equal(clean_plotly_trace_name("(25th-75th percentile band,1)"), "25th-75th percentile band")
  expect_equal(clean_plotly_trace_name("(Median glucose, 1)"), "Median glucose")
  expect_equal(clean_plotly_trace_name("Target range (70-180 mg/dL)"), "Target range (70-180 mg/dL)")
})

test_that("AGP plotly legend names are clean", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 2),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:30:00",
      "2026-05-06 08:00:00",
      "2026-05-06 08:30:00"
    )),
    glucose = c(80, 120, 100, 140),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  plotly_obj <- layout_agp_plotly(plotly::ggplotly(create_agp_summary_plot(data, bin_minutes = 30)))
  names <- vapply(plotly_obj$x$data, function(trace) trace$name %||% "", character(1))

  expect_false(any(grepl(",\\s*1\\)?$", names)))
  expect_true("Target range (70-180 mg/dL)" %in% names)
  expect_true("5th-95th percentile band" %in% names)
  expect_true("25th-75th percentile band" %in% names)
  expect_true("Median glucose" %in% names)
  expect_equal(plotly_obj$x$layout$legend$orientation, "h")
  expect_gt(plotly_obj$x$layout$legend$y, 1)
  expect_gte(plotly_obj$x$layout$margin$t, 80)
  expect_gte(plotly_obj$x$layout$margin$b, 70)
})

test_that("AGP plotly visual traces remain continuous and finite", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 96),
    timestamp = parse_cgm_timestamp("2026-05-05 00:00:00") + rep(seq(0, by = 900, length.out = 96), 2),
    glucose = c(
      100 + 20 * sin(seq(0, 2 * pi, length.out = 96)),
      120 + 15 * cos(seq(0, 2 * pi, length.out = 96))
    ),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  plotly_obj <- layout_agp_plotly(plotly::ggplotly(create_agp_summary_plot(data, bin_minutes = 30), tooltip = "text"))
  traces <- plotly_obj$x$data
  trace_by_name <- function(name) {
    matches <- vapply(traces, function(trace) identical(trace$name %||% "", name), logical(1))
    traces[[which(matches)[[1L]]]]
  }
  outer_band <- trace_by_name("5th-95th percentile band")
  inner_band <- trace_by_name("25th-75th percentile band")
  target_band <- trace_by_name("Target range (70-180 mg/dL)")
  median_line <- trace_by_name("Median glucose")

  expect_false(any(is.na(outer_band$x)))
  expect_false(any(is.na(inner_band$x)))
  expect_false(any(is.na(median_line$x)))
  expect_true(all(is.finite(unlist(target_band$x))))
  expect_true(all(is.finite(unlist(target_band$y))))
  expect_gt(length(outer_band$x), 50)
  expect_gt(length(inner_band$x), 50)
  expect_gt(length(median_line$x), 40)
})

test_that("trace and daily overlay native plotly paths return plotly widgets", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 4),
    timestamp = parse_cgm_timestamp(rep(sprintf("2026-05-05 %02d:00:00", 8:11), 2)),
    glucose = c(90, 100, 110, 120, 95, 105, 115, 125),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = rep(c(FALSE, FALSE, TRUE, FALSE), 2),
    stringsAsFactors = FALSE
  )

  trace <- create_trace_plotly(data, max_points_per_participant = Inf)
  daily <- create_daily_overlay_plotly(data, max_points_per_participant = Inf)
  trace_built <- plotly::plotly_build(trace)
  daily_built <- plotly::plotly_build(daily)

  expect_s3_class(trace, "plotly")
  expect_s3_class(daily, "plotly")
  expect_true(length(trace_built$x$data) >= 2L)
  expect_true(length(daily_built$x$data) >= 1L)
})

test_that("plot downsampling preserves imputed rows and glucose extremes", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-05 00:00:00") + seq(0, by = 300, length.out = 100),
    glucose = seq_len(100),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  data$imputed_flag[[50L]] <- TRUE

  downsampled <- prepare_plot_display_data(data, max_points_per_participant = 10)

  expect_lte(nrow(downsampled), 10)
  expect_true(any(downsampled$imputed_flag))
  expect_true(min(data$glucose) %in% downsampled$glucose)
  expect_true(max(data$glucose) %in% downsampled$glucose)
})
