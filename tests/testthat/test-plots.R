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
    visit = NA_character_,
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
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  expect_s3_class(create_trace_plot(data), "ggplot")
  expect_s3_class(create_daily_overlay_plot(data), "ggplot")
  expect_s3_class(create_agp_summary_plot(data), "ggplot")
  expect_s3_class(create_missingness_timeline_plot(data), "ggplot")
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
    visit = "Baseline",
    source_file = NA_character_,
    imputed_flag = c(FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  plot_data <- prepare_time_of_day_plot_data(data)

  expect_equal(plot_data$date, c("2026-05-05", "2026-05-05", "2026-05-06", "2026-05-06"))
  expect_equal(plot_data$time_minutes, c(480, 485, 480, 485))
  expect_equal(sum(plot_data$imputed_flag), 1)
})

test_that("plot display data respects point budgets and keeps imputed rows when possible", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 10),
    timestamp = parse_cgm_timestamp(rep(sprintf("2026-05-05 08:%02d:00", seq(0, 45, by = 5)), 2)),
    glucose = seq_len(20),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    visit = NA_character_,
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

test_that("plot detail choices map to point budgets", {
  expect_lt(plot_detail_max_points("fast"), plot_detail_max_points("balanced"))
  expect_true(is.infinite(plot_detail_max_points("full")))
})

test_that("plot filters support All sentinel for participant, group, visit, and day", {
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
    visit = rep(c("Baseline", "Week 1"), times = 2),
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  all_data <- filter_plot_data(
    data,
    participant = all_filter_value(),
    group = all_filter_value(),
    visit = all_filter_value(),
    day = all_filter_value()
  )
  filtered <- filter_plot_data(data, participant = "A", group = "Control", visit = "Baseline", day = "2026-05-05")

  expect_equal(nrow(all_data), nrow(data))
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$id, "A")
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

  expect_equal(names(choices)[[1L]], "All")
  expect_equal(unname(choices)[[1L]], all_filter_value())
  expect_equal(unname(choices), c(all_filter_value(), "A", "B"))
  expect_equal(preserve_filter_selection("A", choices), "A")
  expect_equal(preserve_filter_selection("Missing", choices), all_filter_value())
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
    visit = NA_character_,
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
    visit = NA_character_,
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
})

test_that("group and visit plot filters require usable non-missing values", {
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
    visit = c("Baseline", "Baseline", ""),
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  expect_true(plot_filter_available(data, "group", min_values = 2))
  expect_false(plot_filter_available(data, "visit", min_values = 2))
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
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  agp <- prepare_agp_summary_data(data, bin_minutes = 30)

  expect_equal(agp$time_minutes, c(495, 525))
  expect_true(all(c("q05", "q25", "median", "q75", "q95", "n") %in% names(agp)))
  expect_equal(agp$n, c(2, 2))
})

test_that("AGP summary is unchanged by trace display downsampling helpers", {
  data <- data.frame(
    id = rep("A", 8),
    timestamp = parse_cgm_timestamp(sprintf("2026-05-05 %02d:00:00", 0:7)),
    glucose = seq(80, 150, length.out = 8),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    visit = NA_character_,
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
    visit = NA_character_,
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
  expect_equal(unname(fill_scale$palette(3)), c("#7B8288", "#7BC8F6", "#1F78B4"))
  expect_equal(unname(color_scale$palette(1)), "#111111")
  expect_length(ribbon_idx, 2)
  expect_length(rect_idx, 1)
  expect_true(length(line_idx) >= 1)
  expect_true(all(ribbon_idx < rect_idx))
  expect_true(all(rect_idx < line_idx))
  expect_equal(plot$layers[[rect_idx]]$aes_params$alpha, 0.42)
})

test_that("plot download button is in the header action area before filters", {
  ui <- plots_module_ui("plots")
  html <- paste(as.character(ui), collapse = "\n")

  expect_lt(
    regexpr("plots-download_plot", html, fixed = TRUE)[[1L]],
    regexpr("plots-plot_type", html, fixed = TRUE)[[1L]]
  )
})

test_that("plots module uses render-time Subject ID filter container", {
  ui <- plots_module_ui("plots")
  html <- paste(as.character(ui), collapse = "\n")

  expect_true(grepl("plots-subject_filter", html, fixed = TRUE))
  expect_false(grepl("id=\"plots-participant\"", html, fixed = TRUE))
  expect_false(grepl(">Participant<", html, fixed = TRUE))
})

test_that("plots module uses normalized day values for active plot cache", {
  code <- paste(readLines(testthat::test_path("../../R/module_plots.R"), warn = FALSE), collapse = "\n")

  expect_true(grepl("normalized_day <- shiny::reactive", code, fixed = TRUE))
  expect_true(grepl("day_selection <- shiny::reactiveVal", code, fixed = TRUE))
  expect_true(grepl("day = normalized_day()", code, fixed = TRUE))
  expect_true(grepl("plot_day_cache_key(normalized_day())", code, fixed = TRUE))
  expect_true(grepl("\"remove_button\"", code, fixed = TRUE))
  expect_false(grepl("input$day\n    )", code, fixed = TRUE))
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
    visit = NA_character_,
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
