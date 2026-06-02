missingness_fixture <- function() {
  gap001_hours <- c(0:13, 16:23)
  gap002_hours <- 0:23
  data.frame(
    id = c(rep("GAP001", length(gap001_hours)), rep("GAP002", length(gap002_hours))),
    timestamp = parse_cgm_timestamp(c(
      format(as.POSIXct("2026-05-01 00:00:00", tz = "UTC") + gap001_hours * 3600, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      format(as.POSIXct("2026-05-01 00:00:00", tz = "UTC") + gap002_hours * 3600, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )),
    glucose = c(replace(100 + seq_along(gap001_hours), 4, NA), replace(130 + seq_along(gap002_hours), 7, NA)),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )
}

test_that("canonical missingness example fixture has intentional missing values and gaps", {
  standardized <- missingness_fixture()

  expect_equal(nrow(standardized), 46)
  expect_equal(sum(is.na(standardized$glucose)), 2)

  summary <- compute_missingness_summary(standardized, valid_day_hours = 14, interval_minutes = 60)
  expect_equal(summary$missing_glucose, c(3, 1))
  expect_equal(summary$missing_glucose_rate, c(round(100 * 3 / 24, 2), round(100 / 24, 2)))
  expect_equal(summary$explicit_missing_glucose, c(1, 1))
  expect_equal(summary$gap_count, c(1, 0))
  expect_equal(summary$estimated_missing_readings, c(2, 0))
})

test_that("detect_gap_periods reports known timestamp gaps", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 00:00:00",
      "2026-05-05 01:00:00",
      "2026-05-05 04:00:00",
      "2026-05-05 05:00:00"
    )),
    glucose = c(100, 110, 120, 130),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  gaps <- detect_gap_periods(data, interval_minutes = 60)
  expect_equal(nrow(gaps), 1)
  expect_equal(gaps$gap_minutes, 120)
  expect_equal(gaps$expected_interval_minutes, 60)
  expect_equal(gaps$estimated_missing_readings, 2)
})

test_that("grid missingness snaps off-grid timestamps and keeps non-missing duplicates", {
  data <- data.frame(
    id = c("A", "A", "A", "A"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 00:00:00",
      "2026-05-05 00:04:00",
      "2026-05-05 00:06:00",
      "2026-05-05 00:15:00"
    )),
    glucose = c(100, NA, 115, 130),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  expanded <- regularize_cgm_timestamp_grid(data, interval_minutes = 5)
  summary <- missingness_grid_summary_by_id(data, interval_minutes = 5)

  expect_equal(format(expanded$timestamp, "%H:%M:%S"), c("00:00:00", "00:05:00", "00:10:00", "00:15:00"))
  expect_equal(expanded$glucose[expanded$timestamp == parse_cgm_timestamp("2026-05-05 00:05:00")], 115)
  expect_equal(summary$estimated_missing_readings, 1)
  expect_equal(summary$missing_glucose, 1)
  expect_equal(summary$missing_glucose_rate, 25)
})

test_that("grid missingness handles explicit NA rows and staggered subject starts", {
  data <- data.frame(
    id = c("A", "A", "B", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 00:00:00",
      "2026-05-05 00:15:00",
      "2026-05-06 12:00:00",
      "2026-05-06 12:10:00"
    )),
    glucose = c(100, 130, NA, 145),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  summary <- missingness_grid_summary_by_id(data, interval_minutes = 5)

  expect_equal(summary$expanded_rows[summary$id == "A"], 4)
  expect_equal(summary$estimated_missing_readings[summary$id == "A"], 2)
  expect_equal(summary$explicit_missing_glucose[summary$id == "B"], 1)
  expect_equal(summary$expanded_rows[summary$id == "B"], 3)
})

test_that("missingness plot helpers return expected plot types", {
  standardized <- missingness_fixture()

  expect_s3_class(create_missingness_timeline_plot(standardized), "ggplot")
  expect_s3_class(create_missingness_heatmap_plot(standardized), "plotly")
  expect_false(inherits(create_missingness_heatmap_plot(standardized), "ggplot"))
})

test_that("missingness heatmap data is participant-day aggregated", {
  standardized <- missingness_fixture()

  heatmap <- compute_missingness_heatmap_data(standardized, interval_minutes = 60)

  expect_lt(nrow(heatmap), nrow(standardized))
  expect_true(all(c(
    "id",
    "date",
    "readings",
    "missing_glucose",
    "missing_glucose_rate",
    "timestamp_gaps",
    "estimated_missing_readings",
    "imputed_rows",
    "tooltip"
  ) %in% names(heatmap)))
  expect_equal(sum(heatmap$missing_glucose), 4)
  expect_equal(sum(heatmap$timestamp_gaps), 1)
  expect_equal(sum(heatmap$estimated_missing_readings), 2)
})

test_that("missingness precompute preserves direct helper outputs", {
  standardized <- missingness_fixture()
  fake_result <- rbind(
    data.frame(
      .row_id = seq_len(nrow(standardized)),
      subject_index = as.integer(factor(standardized$id, levels = unique(standardized$id))),
      time = format_cgm_timestamp_iso(standardized$timestamp, tz = "UTC"),
      glucose = standardized$glucose,
      imputed_glucose_value = ifelse(is.na(standardized$glucose), 123, standardized$glucose),
      imputation_method = "MICE+ARIMA",
      missing_rate = 4 / 48,
      stringsAsFactors = FALSE
    ),
    data.frame(
      .row_id = c(NA_integer_, NA_integer_),
      subject_index = c(1L, 1L),
      time = c("2026-05-01T14:00:00", "2026-05-01T15:00:00"),
      glucose = c(NA_real_, NA_real_),
      imputed_glucose_value = c(123, 123),
      imputation_method = "MICE+ARIMA",
      missing_rate = 4 / 48,
      stringsAsFactors = FALSE
    )
  )
  analysis <- apply_imputed_glucose(standardized, fake_result)
  original_precomputed <- missingness_precompute(standardized, interval_minutes = 60)
  analysis_precomputed <- missingness_precompute(analysis, interval_minutes = 60)

  expect_equal(
    missingness_grid_summary_by_id(standardized, interval_minutes = 60, precomputed = original_precomputed),
    missingness_grid_summary_by_id(standardized, interval_minutes = 60)
  )
  expect_equal(
    detect_gap_periods(standardized, interval_minutes = 60, precomputed = original_precomputed),
    detect_gap_periods(standardized, interval_minutes = 60)
  )
  expect_equal(
    compute_missingness_summary(standardized, valid_day_hours = 14, interval_minutes = 60, precomputed = original_precomputed),
    compute_missingness_summary(standardized, valid_day_hours = 14, interval_minutes = 60)
  )
  expect_equal(
    compute_missingness_heatmap_data(standardized, interval_minutes = 60, precomputed = original_precomputed),
    compute_missingness_heatmap_data(standardized, interval_minutes = 60)
  )
  expect_equal(
    compute_missingness_calendar_data(standardized, interval_minutes = 60, precomputed = original_precomputed),
    compute_missingness_calendar_data(standardized, interval_minutes = 60)
  )
  expect_equal(
    compare_missingness_summaries(
      standardized,
      analysis,
      valid_day_hours = 14,
      include_preprocessing = TRUE,
      interval_minutes = 60,
      original_precomputed = original_precomputed,
      analysis_precomputed = analysis_precomputed
    ),
    compare_missingness_summaries(
      standardized,
      analysis,
      valid_day_hours = 14,
      include_preprocessing = TRUE,
      interval_minutes = 60
    )
  )
  expect_equal(
    summarize_imputation_status(
      standardized,
      analysis,
      list(imputation_method = "mice_only", imputation_available = TRUE, imputation_interval_minutes = 60),
      original_precomputed = original_precomputed,
      analysis_precomputed = analysis_precomputed
    ),
    summarize_imputation_status(
      standardized,
      analysis,
      list(imputation_method = "mice_only", imputation_available = TRUE, imputation_interval_minutes = 60)
    )
  )
})

test_that("daily coverage calendar includes no-data days and coverage details", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c(
      "2026-05-04 00:00:00",
      "2026-05-04 01:00:00",
      "2026-05-04 02:00:00",
      "2026-05-06 00:00:00",
      "2026-05-06 01:00:00",
      "2026-05-06 02:00:00"
    )),
    glucose = c(100, NA, 120, 130, 140, 150),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  calendar <- compute_missingness_calendar_data(data, interval_minutes = 60)
  no_data <- calendar[calendar$date == as.Date("2026-05-05"), , drop = FALSE]
  may_4 <- calendar[calendar$date == as.Date("2026-05-04"), , drop = FALSE]

  expect_equal(nrow(calendar), 3)
  expect_equal(no_data$readings, 0)
  expect_equal(as.character(no_data$coverage_status), "No data")
  expect_equal(may_4$expected_readings, 24)
  expect_equal(may_4$coverage_percent, 12.5)
  expect_equal(as.character(may_4$coverage_status), "Low coverage (<50%)")
  expect_equal(may_4$missing_glucose, 22)
  expect_equal(may_4$missing_glucose_rate, round(100 * 22 / 24, 2))
  expect_true(any(grepl("Status: Low coverage (<50%)", calendar$tooltip, fixed = TRUE)))
  expect_true(all(grepl("Readings:", calendar$tooltip, fixed = TRUE)))
  expect_true(all(grepl("Expected readings:", calendar$tooltip, fixed = TRUE)))
  expect_true(all(grepl("Coverage:", calendar$tooltip, fixed = TRUE)))
  expect_true(all(c("week_index", "weekday_index", "plot_y") %in% names(calendar)))
})

test_that("daily coverage calendar uses subject active date spans", {
  data <- data.frame(
    id = c("A", "A", "B", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-01-01 00:00:00",
      "2026-01-03 00:00:00",
      "2026-03-01 00:00:00",
      "2026-03-02 00:00:00"
    )),
    glucose = c(100, 120, 130, 140),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )

  calendar <- compute_missingness_calendar_data(data, interval_minutes = 60)
  a_dates <- calendar$date[calendar$id == "A"]
  b_dates <- calendar$date[calendar$id == "B"]

  expect_equal(a_dates, as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")))
  expect_equal(b_dates, as.Date(c("2026-03-01", "2026-03-02")))
  expect_false(any(calendar$id == "A" & calendar$date >= as.Date("2026-03-01")))
  expect_false(any(calendar$id == "B" & calendar$date <= as.Date("2026-01-03")))
  expect_equal(
    as.character(calendar$coverage_status[calendar$id == "A" & calendar$date == as.Date("2026-01-02")]),
    "No data"
  )
  expect_true(all(c("month_index", "week_of_month", "calendar_x") %in% names(calendar)))
  expect_lte(
    min(calendar$calendar_x[calendar$id == "B"]) - max(calendar$calendar_x[calendar$id == "A"]),
    2
  )
  expect_lt(max(calendar$calendar_x), 5)
})

test_that("daily coverage month ticks use compact chronological positions", {
  data <- data.frame(
    id = c("A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-01-01 00:00:00",
      "2026-05-01 00:00:00"
    )),
    glucose = c(100, 120),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )

  calendar <- compute_missingness_calendar_data(data, interval_minutes = 60)
  ticks <- missingness_calendar_month_ticks(calendar)

  expect_equal(as.character(ticks$month), c("Jan 2026", "May 2026"))
  expect_lt(diff(ticks$calendar_x), 3)
})

test_that("daily coverage calendar dimensions scale with visible subjects and labels", {
  one_subject <- data.frame(
    id = "A",
    date = as.Date("2026-01-01"),
    month = "Jan 2026",
    calendar_x = 1,
    stringsAsFactors = FALSE
  )
  many_subjects <- data.frame(
    id = paste0("Subject_", seq_len(8)),
    date = as.Date("2026-01-01"),
    month = "Jan 2026",
    calendar_x = 1,
    stringsAsFactors = FALSE
  )
  long_subject <- data.frame(
    id = "VeryLongSubjectIdentifier001",
    date = as.Date("2026-01-01"),
    month = "Jan 2026",
    calendar_x = 1,
    stringsAsFactors = FALSE
  )
  many_months <- data.frame(
    id = "A",
    date = as.Date("2026-01-01") + seq_len(12),
    month = paste(month.abb, "2026"),
    calendar_x = seq_len(12),
    stringsAsFactors = FALSE
  )

  compact <- missingness_calendar_dimensions(one_subject)
  tall <- missingness_calendar_dimensions(many_subjects)
  wide_label <- missingness_calendar_dimensions(long_subject)
  wide_axis <- missingness_calendar_dimensions(many_months)

  expect_gt(tall$height, compact$height)
  expect_gte(wide_label$margin$l, compact$margin$l)
  expect_gt(wide_axis$margin$b, compact$margin$b)
  expect_lt(wide_axis$x_tick_angle, 0)
  expect_true(tall$marker_size <= compact$marker_size)
})

test_that("daily coverage plot filters subject before calendar expansion", {
  data <- data.frame(
    id = c("A", "A", "B", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-01-01 00:00:00",
      "2026-01-03 00:00:00",
      "2026-03-01 00:00:00",
      "2026-03-02 00:00:00"
    )),
    glucose = c(100, 120, 130, 140),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )

  plot <- create_missingness_heatmap_plot(data, participant = "B")
  built <- plotly::plotly_build(plot)
  hover_text <- unlist(lapply(built$x$data, `[[`, "text"), use.names = FALSE)

  expect_s3_class(plot, "plotly")
  expect_true(any(grepl("Subject ID: B", hover_text, fixed = TRUE)))
  expect_false(any(grepl("Subject ID: A", hover_text, fixed = TRUE)))
})

test_that("daily coverage is capped at 100 percent", {
  timestamps <- c(
    seq(
      parse_cgm_timestamp("2026-05-04 00:00:00"),
      parse_cgm_timestamp("2026-05-04 23:00:00"),
      by = "hour"
    ),
    parse_cgm_timestamp("2026-05-04 23:00:00")
  )
  data <- data.frame(
    id = "A",
    timestamp = timestamps,
    glucose = seq_len(length(timestamps)) + 100,
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  calendar <- compute_missingness_calendar_data(data, interval_minutes = 60)

  expect_equal(calendar$expected_readings, 24)
  expect_equal(calendar$coverage_percent, 100)
  expect_equal(as.character(calendar$coverage_status), "High coverage (>=80%)")
})

test_that("daily coverage labels include explicit thresholds", {
  status <- coverage_status(c(25, 60, 90, NA), c(10, 10, 10, 0))

  expect_equal(
    as.character(status),
    c(
      "Low coverage (<50%)",
      "Partial coverage (50-79%)",
      "High coverage (>=80%)",
      "No data"
    )
  )
  expect_equal(
    levels(status),
    c(
      "No data",
      "Low coverage (<50%)",
      "Partial coverage (50-79%)",
      "High coverage (>=80%)"
    )
  )
  expect_false(any(is.na(coverage_status_color(status))))
})

test_that("day coverage warnings count full and half-day missingness per subject", {
  data <- data.frame(
    id = c(
      rep("A", 24),
      rep("A", 11),
      rep("B", 24)
    ),
    id_source = subject_id_source_mapped(),
    timestamp = c(
      parse_cgm_timestamp(c(
        format(parse_cgm_timestamp("2026-05-01 00:00:00") + seq(0, by = 3600, length.out = 24), "%Y-%m-%d %H:%M:%S")
      )),
      parse_cgm_timestamp("2026-05-03 00:00:00") + seq(0, by = 3600, length.out = 11),
      parse_cgm_timestamp("2026-06-01 00:00:00") + seq(0, by = 3600, length.out = 24)
    ),
    glucose = 100,
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  calendar <- compute_missingness_calendar_data(data, interval_minutes = 60)
  warnings <- day_coverage_warning_summary(calendar, data = data)
  comparison <- append_day_coverage_warnings(
    compare_missingness_summaries(data, data, interval_minutes = 60),
    calendar,
    data = data
  )
  note <- day_coverage_warning_note(warnings)

  expect_equal(warnings[["Full missing days"]][warnings[["Subject ID"]] == "A"], 1L)
  expect_equal(warnings[["Half-day or worse coverage days"]][warnings[["Subject ID"]] == "A"], 1L)
  expect_equal(warnings[["Full missing days"]][warnings[["Subject ID"]] == "B"], 0L)
  expect_true(all(c("Full missing days", "Half-day or worse coverage days") %in% names(comparison)))
  expect_true(grepl("full missing day", note, fixed = TRUE))
  expect_true(grepl("less than half", note, fixed = TRUE))
})

test_that("daily coverage calendar uses fixed-color plotly traces without colorscales", {
  standardized <- missingness_fixture()

  plot <- create_missingness_heatmap_plot(standardized)
  built <- plotly::plotly_build(plot)
  traces <- built$x$data

  expect_s3_class(plot, "plotly")
  expect_true(length(traces) >= 1)
  expect_false(any(vapply(traces, function(trace) !is.null(trace$marker$colorscale), logical(1))))
  expect_true(all(vapply(traces, function(trace) length(trace$marker$color) == 1, logical(1))))
})

test_that("daily coverage plotly layout uses dynamic calendar dimensions", {
  data <- data.frame(
    id = rep(paste0("Subject_", seq_len(5)), each = 2),
    timestamp = parse_cgm_timestamp(rep(c("2026-01-01 00:00:00", "2026-01-02 00:00:00"), 5)),
    glucose = seq_len(10) + 100,
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )
  calendar <- compute_missingness_calendar_data(data)
  dimensions <- missingness_calendar_dimensions(calendar)
  built <- plotly::plotly_build(create_missingness_heatmap_plot(data, calendar_data = calendar))

  expect_equal(built$x$layout$height, dimensions$height)
  expect_equal(built$x$layout$margin$l, dimensions$margin$l)
  expect_equal(built$x$layout$margin$b, dimensions$margin$b)
  expect_equal(built$x$layout$legend$orientation, "h")
  expect_equal(length(built$x$layout$yaxis$tickvals), length(unique(calendar$id)) * 7)
})

test_that("missingness comparison reports original and analysis data counts", {
  standardized <- missingness_fixture()
  fake_result <- rbind(
    data.frame(
    .row_id = seq_len(nrow(standardized)),
      subject_index = as.integer(factor(standardized$id, levels = unique(standardized$id))),
      time = format_cgm_timestamp_iso(standardized$timestamp, tz = "UTC"),
    glucose = standardized$glucose,
    imputed_glucose_value = ifelse(is.na(standardized$glucose), 123, standardized$glucose),
    imputation_method = "MICE+ARIMA",
      missing_rate = 4 / 48,
    stringsAsFactors = FALSE
    ),
    data.frame(
      .row_id = c(NA_integer_, NA_integer_),
      subject_index = c(1L, 1L),
      time = c("2026-05-01T14:00:00", "2026-05-01T15:00:00"),
      glucose = c(NA_real_, NA_real_),
      imputed_glucose_value = c(123, 123),
      imputation_method = "MICE+ARIMA",
      missing_rate = 4 / 48,
      stringsAsFactors = FALSE
    )
  )
  analysis <- apply_imputed_glucose(standardized, fake_result)

  comparison <- compare_missingness_summaries(standardized, analysis, valid_day_hours = 14, interval_minutes = 60)

  expect_equal(comparison[["Subject ID"]], c("GAP001", "GAP002"))
  expect_equal(
    names(comparison),
    c(
      "Subject ID",
      "Missing glucose rows",
      "Missing glucose (%)",
      "Timestamp gaps",
      "Estimated missing readings from gaps",
      "Valid days"
    )
  )
  expect_equal(comparison[["Missing glucose rows"]], c(0, 0))
  expect_equal(comparison[["Timestamp gaps"]], c(1, 0))
  expect_equal(comparison[["Estimated missing readings from gaps"]], c(2, 0))

  comparison_with_imputation <- compare_missingness_summaries(
    standardized,
    analysis,
    valid_day_hours = 14,
    include_preprocessing = TRUE,
    interval_minutes = 60
  )
  expect_equal(
    names(comparison_with_imputation),
    c(
      "Subject ID",
      "Missing glucose rows before preprocessing",
      "Missing glucose (%) before preprocessing",
      "Missing glucose rows after preprocessing",
      "Missing glucose (%) after preprocessing",
      "Filled glucose rows",
      "Timestamp gaps",
      "Estimated missing readings from gaps",
      "Valid days"
    )
  )
  expect_equal(comparison_with_imputation[["Missing glucose rows before preprocessing"]], c(3, 1))
  expect_equal(comparison_with_imputation[["Missing glucose rows after preprocessing"]], c(0, 0))
  expect_equal(comparison_with_imputation[["Filled glucose rows"]], c(3, 1))
  expect_equal(comparison_with_imputation[["Timestamp gaps"]], c(1, 0))
  expect_equal(comparison_with_imputation[["Estimated missing readings from gaps"]], c(2, 0))
})

test_that("missingness comparison hides Subject ID for one filename-derived subject", {
  original <- data.frame(
    id = "FallbackA",
    id_source = subject_id_source_filename(),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-05 08:05:00")),
    glucose = c(100, NA),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = "FallbackA.csv",
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  comparison <- compare_missingness_summaries(original, original, valid_day_hours = 14)
  forced <- compare_missingness_summaries(original, original, valid_day_hours = 14, show_subject_id = TRUE)

  expect_false("Subject ID" %in% names(comparison))
  expect_true("Subject ID" %in% names(forced))
  expect_equal(forced[["Subject ID"]], "FallbackA")
})

test_that("missingness calendar tooltips can force selected Subject ID labels", {
  data <- data.frame(
    id = "FallbackA",
    id_source = subject_id_source_filename(),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-05 08:05:00")),
    glucose = c(100, NA),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = "FallbackA.csv",
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  default <- compute_missingness_calendar_data(data, interval_minutes = 5)
  forced <- compute_missingness_calendar_data(data, interval_minutes = 5, show_subject_id = TRUE)

  expect_false(any(grepl("Subject ID: FallbackA", default$tooltip, fixed = TRUE)))
  expect_true(any(grepl("Subject ID: FallbackA", forced$tooltip, fixed = TRUE)))
})

test_that("analysis missingness table visibility follows imputation setting", {
  expect_false(should_show_analysis_missingness(list(imputation_method = "none")))
  expect_true(should_show_analysis_missingness(list(imputation_method = "mice_only")))
})

test_that("imputation status reports off, unavailable, no-missing, and applied states", {
  standardized <- missingness_fixture()
  complete <- regularize_cgm_timestamp_grid(standardized, interval_minutes = 60)
  complete$glucose[is.na(complete$glucose)] <- 120
  fake_result <- rbind(
    data.frame(
      .row_id = seq_len(nrow(standardized)),
      subject_index = as.integer(factor(standardized$id, levels = unique(standardized$id))),
      time = format_cgm_timestamp_iso(standardized$timestamp, tz = "UTC"),
      glucose = standardized$glucose,
      imputed_glucose_value = ifelse(is.na(standardized$glucose), 123, standardized$glucose),
      imputation_method = "MICE+ARIMA",
      missing_rate = 4 / 48,
      stringsAsFactors = FALSE
    ),
    data.frame(
      .row_id = c(NA_integer_, NA_integer_),
      subject_index = c(1L, 1L),
      time = c("2026-05-01T14:00:00", "2026-05-01T15:00:00"),
      glucose = c(NA_real_, NA_real_),
      imputed_glucose_value = c(123, 123),
      imputation_method = "MICE+ARIMA",
      missing_rate = 4 / 48,
      stringsAsFactors = FALSE
    )
  )
  analysis <- apply_imputed_glucose(standardized, fake_result)
  gap_only <- data.frame(
    id = "A",
    id_source = subject_id_source_mapped(),
    timestamp = parse_cgm_timestamp(c("2026-05-01 00:00:00", "2026-05-01 00:10:00")),
    glucose = c(100, 120),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  no_fill_result <- data.frame(
    .row_id = c(1L, NA_integer_, 2L),
    subject_index = c(1L, 1L, 1L),
    time = c("2026-05-01T00:00:00", "2026-05-01T00:05:00", "2026-05-01T00:10:00"),
    glucose = c(100, NA, 120),
    imputed_glucose_value = c(100, NA, 120),
    imputation_method = "MICE+ARIMA",
    missing_rate = 1 / 3,
    stringsAsFactors = FALSE
  )
  no_fill_analysis <- apply_imputed_glucose(gap_only, no_fill_result)

  off <- summarize_imputation_status(standardized, standardized, list(imputation_method = "none", imputation_available = TRUE, imputation_interval_minutes = 60))
  unavailable <- summarize_imputation_status(standardized, standardized, list(imputation_method = "mice_only", imputation_available = FALSE, imputation_interval_minutes = 60))
  not_needed <- summarize_imputation_status(complete, complete, list(imputation_method = "mice_only", imputation_available = TRUE, imputation_interval_minutes = 60))
  applied <- summarize_imputation_status(standardized, analysis, list(
    imputation_method = "mice_only",
    imputation_model = "arima",
    imputation_available = TRUE,
    imputation_seed = 42,
    imputation_interval_minutes = 60,
    imputation_missing_warning_threshold = 0.2
  ))
  failed <- standardized
  attr(failed, "imputation_error") <- "Python dependency unavailable"
  could_not_apply <- summarize_imputation_status(standardized, failed, list(imputation_method = "mice_only", imputation_available = TRUE, imputation_backend = "sklearn", imputation_interval_minutes = 60))
  warned <- analysis
  attr(warned, "imputation_warnings") <- "High missingness exceeds warning threshold"
  warning_status <- summarize_imputation_status(standardized, warned, list(imputation_method = "mice_only", imputation_model = "xgboost", imputation_available = TRUE, imputation_interval_minutes = 60))
  no_rows_filled <- summarize_imputation_status(gap_only, no_fill_analysis, list(imputation_method = "mice_only", imputation_available = TRUE, imputation_interval_minutes = 5))

  expect_equal(off$Status, "Not applied")
  expect_equal(unavailable$Status, "Unavailable")
  expect_equal(not_needed$Status, "Not needed")
  expect_equal(applied$Status, "Applied")
  expect_equal(could_not_apply$Status, "Could not apply")
  expect_equal(no_rows_filled$Status, "No rows filled")
  expect_equal(applied$Model, "ARIMA")
  expect_false("Backend" %in% names(could_not_apply))
  expect_equal(applied[["Missing warning threshold (%)"]], 20)
  expect_equal(warning_status$Warnings, "High missingness exceeds warning threshold")
  expect_true(grepl("Warnings:", warning_status$Message, fixed = TRUE))
  expect_true(grepl("Python dependency unavailable", could_not_apply$Message, fixed = TRUE))
  expect_true(grepl("returned no finite imputed glucose", no_rows_filled$Message, fixed = TRUE))
  expect_equal(applied[["Filled glucose rows"]], 4)
  expect_equal(applied[["Original missing glucose"]], 4)
  expect_equal(applied[["Original missing glucose (%)"]], round(100 * 4 / 48, 2))
  expect_equal(applied[["Analysis missing glucose"]], 0)
  expect_equal(applied[["Analysis missing glucose (%)"]], 0)
  expect_equal(applied[["Estimated missing readings from gaps"]], 2)
  expect_false("CGMissingDataR available" %in% names(applied))
  expect_false(any(grepl("CGMissingDataR|adapter|engine", unlist(applied), ignore.case = TRUE)))
})

test_that("preprocessing comparison summary appears only when imputation is selected", {
  off <- data.frame(
    Method = "None",
    Status = "Not applied",
    Seed = NA_integer_,
    `Original missing glucose` = 2,
    `Original missing glucose (%)` = 4.35,
    `Analysis missing glucose` = 2,
    `Analysis missing glucose (%)` = 4.35,
    `Filled glucose rows` = 0,
    `Estimated missing readings from gaps` = 2,
    check.names = FALSE
  )
  applied <- off
  applied$Method <- "MICE imputation"
  applied[["Analysis missing glucose"]] <- 0
  applied[["Analysis missing glucose (%)"]] <- 0
  applied[["Filled glucose rows"]] <- 2

  expect_equal(nrow(preprocessing_comparison_summary(off)), 0)
  summary <- preprocessing_comparison_summary(applied)
  expect_equal(
    summary$Label,
    c(
      "Original missing glucose rows",
      "Original missing glucose (%)",
      "Missing glucose rows after preprocessing",
      "Missing glucose (%) after preprocessing",
      "Filled glucose rows",
      "Estimated missing readings from gaps"
    )
  )
  expect_equal(summary$Value, c("2", "4.35%", "0", "0%", "2", "2"))
})
