test_that("missingness demo loads with intentional missing values and gaps", {
  demo <- load_missingness_demo_cgm_data()
  expect_equal(nrow(demo), 46)
  expect_equal(sum(is.na(demo$glucose)), 2)

  standardized <- standardize_cgm_data(
    demo,
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )

  summary <- compute_missingness_summary(standardized, valid_day_hours = 14)
  expect_equal(summary$missing_glucose, c(1, 1))
  expect_equal(summary$missing_glucose_rate, c(round(100 / 22, 2), round(100 / 24, 2)))
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
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  gaps <- detect_gap_periods(data)
  expect_equal(nrow(gaps), 1)
  expect_equal(gaps$gap_minutes, 180)
  expect_equal(gaps$expected_interval_minutes, 60)
  expect_equal(gaps$estimated_missing_readings, 2)
})

test_that("missingness plot helpers return expected plot types", {
  standardized <- standardize_cgm_data(
    load_missingness_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )

  expect_s3_class(create_missingness_timeline_plot(standardized), "ggplot")
  expect_s3_class(create_missingness_heatmap_plot(standardized), "plotly")
  expect_false(inherits(create_missingness_heatmap_plot(standardized), "ggplot"))
})

test_that("missingness heatmap data is participant-day aggregated", {
  standardized <- standardize_cgm_data(
    load_missingness_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )

  heatmap <- compute_missingness_heatmap_data(standardized)

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
  expect_equal(sum(heatmap$missing_glucose), 2)
  expect_equal(sum(heatmap$timestamp_gaps), 1)
  expect_equal(sum(heatmap$estimated_missing_readings), 2)
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
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  calendar <- compute_missingness_calendar_data(data)
  no_data <- calendar[calendar$date == as.Date("2026-05-05"), , drop = FALSE]
  may_4 <- calendar[calendar$date == as.Date("2026-05-04"), , drop = FALSE]

  expect_equal(nrow(calendar), 3)
  expect_equal(no_data$readings, 0)
  expect_equal(as.character(no_data$coverage_status), "No data")
  expect_equal(may_4$expected_readings, 24)
  expect_equal(may_4$coverage_percent, 12.5)
  expect_equal(may_4$missing_glucose, 1)
  expect_equal(may_4$missing_glucose_rate, round(100 / 3, 2))
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
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )

  calendar <- compute_missingness_calendar_data(data)
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
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )

  calendar <- compute_missingness_calendar_data(data)
  ticks <- missingness_calendar_month_ticks(calendar)

  expect_equal(as.character(ticks$month), c("Jan 2026", "May 2026"))
  expect_lt(diff(ticks$calendar_x), 3)
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
    visit = NA_character_,
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
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  calendar <- compute_missingness_calendar_data(data)

  expect_equal(calendar$expected_readings, 24)
  expect_equal(calendar$coverage_percent, 100)
})

test_that("daily coverage calendar uses fixed-color plotly traces without colorscales", {
  standardized <- standardize_cgm_data(
    load_missingness_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )

  plot <- create_missingness_heatmap_plot(standardized)
  built <- plotly::plotly_build(plot)
  traces <- built$x$data

  expect_s3_class(plot, "plotly")
  expect_true(length(traces) >= 1)
  expect_false(any(vapply(traces, function(trace) !is.null(trace$marker$colorscale), logical(1))))
  expect_true(all(vapply(traces, function(trace) length(trace$marker$color) == 1, logical(1))))
})

test_that("missingness comparison reports original and analysis data counts", {
  standardized <- standardize_cgm_data(
    load_missingness_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  fake_result <- list(
    imputed_data = list(
      mice_only = data.frame(
        .RowID = seq_len(nrow(standardized)),
        .Missing = is.na(standardized$glucose),
        ImputedValue = ifelse(is.na(standardized$glucose), 123, standardized$glucose)
      )
    )
  )
  analysis <- apply_imputed_glucose(standardized, fake_result)

  comparison <- compare_missingness_summaries(standardized, analysis, valid_day_hours = 14)

  expect_equal(comparison[["Subject ID"]], c("GAP001", "GAP002"))
  expect_equal(
    names(comparison),
    c(
      "Subject ID",
      "Missing glucose",
      "Missing glucose rate (%)",
      "Missing glucose after preprocessing",
      "Missing glucose rate after preprocessing (%)",
      "Filled glucose rows",
      "Timestamp gaps",
      "Estimated missing readings",
      "Valid days"
    )
  )
  expect_equal(comparison[["Missing glucose"]], c(1, 1))
  expect_equal(comparison[["Missing glucose after preprocessing"]], c(0, 0))
  expect_equal(comparison[["Filled glucose rows"]], c(1, 1))
  expect_equal(comparison[["Timestamp gaps"]], c(1, 0))
  expect_equal(comparison[["Estimated missing readings"]], c(2, 0))
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
    visit = NA_character_,
    source_file = "FallbackA.csv",
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  comparison <- compare_missingness_summaries(original, original, valid_day_hours = 14)

  expect_false("Subject ID" %in% names(comparison))
})

test_that("analysis missingness table visibility follows imputation setting", {
  expect_false(should_show_analysis_missingness(list(imputation_method = "none")))
  expect_true(should_show_analysis_missingness(list(imputation_method = "mice_only")))
})

test_that("imputation status reports off, unavailable, no-missing, and applied states", {
  standardized <- standardize_cgm_data(
    load_missingness_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  complete <- standardized
  complete$glucose[is.na(complete$glucose)] <- 120
  fake_result <- list(
    imputed_data = list(
      mice_only = data.frame(
        .RowID = seq_len(nrow(standardized)),
        .Missing = is.na(standardized$glucose),
        ImputedValue = ifelse(is.na(standardized$glucose), 123, standardized$glucose)
      )
    )
  )
  analysis <- apply_imputed_glucose(standardized, fake_result)

  off <- summarize_imputation_status(standardized, standardized, list(imputation_method = "none", imputation_available = TRUE))
  unavailable <- summarize_imputation_status(standardized, standardized, list(imputation_method = "mice_only", imputation_available = FALSE))
  not_needed <- summarize_imputation_status(complete, complete, list(imputation_method = "mice_only", imputation_available = TRUE))
  applied <- summarize_imputation_status(standardized, analysis, list(imputation_method = "mice_only", imputation_available = TRUE, imputation_seed = 42))

  expect_equal(off$Status, "Not applied")
  expect_equal(unavailable$Status, "Unavailable")
  expect_equal(not_needed$Status, "Not needed")
  expect_equal(applied$Status, "Applied")
  expect_equal(applied[["Filled glucose rows"]], 2)
  expect_false("CGMissingDataR available" %in% names(applied))
  expect_false(any(grepl("CGMissingDataR", unlist(applied), fixed = TRUE)))
})
