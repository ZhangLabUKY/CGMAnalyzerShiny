test_that("compute_qc_summary reports interval, duplicates, gaps, and implausible values", {
  data <- data.frame(
    id = c("A", "A", "A", "A"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:30:00"
    )),
    glucose = c(100, 110, 500, NA),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  qc <- compute_qc_summary(data, valid_day_hours = 1)

  expect_equal(qc$readings, 4)
  expect_equal(qc$duplicate_timestamps, 1)
  expect_equal(qc$missing_glucose, 1)
  expect_equal(qc$implausible_values, 1)
  expect_true(qc$gap_count >= 1)
})

test_that("prepare_qc_display adds review status and notes", {
  ok_data <- data.frame(
    id = "A",
    timestamp = seq(
      parse_cgm_timestamp("2026-05-05 00:00:00"),
      parse_cgm_timestamp("2026-05-05 23:00:00"),
      by = "hour"
    ),
    glucose = 100,
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )
  review_data <- data.frame(
    id = c("B", "B", "B", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:30:00"
    )),
    glucose = c(100, 110, 500, NA),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )

  ok_display <- prepare_qc_display(compute_qc_summary(ok_data, valid_day_hours = 1), ok_data)
  review_display <- prepare_qc_display(compute_qc_summary(review_data, valid_day_hours = 1), review_data)

  expect_equal(ok_display[["QC status"]], "OK")
  expect_equal(ok_display[["Review notes"]], "No review flags")
  expect_equal(review_display[["QC status"]], "Review")
  expect_match(review_display[["Review notes"]], "missing glucose row")
  expect_match(review_display[["Review notes"]], "timestamp gap")
  expect_match(review_display[["Review notes"]], "duplicate timestamp")
  expect_match(review_display[["Review notes"]], "outside review range")
})

test_that("prepare_qc_display uses readable column labels without changing raw QC names", {
  data <- data.frame(
    id = "A",
    timestamp = seq(
      parse_cgm_timestamp("2026-05-05 00:00:00"),
      parse_cgm_timestamp("2026-05-05 23:00:00"),
      by = "hour"
    ),
    glucose = 100,
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )

  qc <- compute_qc_summary(data, valid_day_hours = 1)
  display <- prepare_qc_display(qc, data)

  expect_true(all(c(
    "id",
    "first_timestamp",
    "wear_time_percent",
    "duplicate_timestamps"
  ) %in% names(qc)))
  expect_true(all(c(
    "Subject ID",
    "QC status",
    "Review notes",
    "Readings",
    "First timestamp",
    "Last timestamp",
    "Observed days",
    "Valid days",
    "Median interval (minutes)",
    "Coverage (%)",
    "Missing glucose",
    "Duplicate timestamps",
    "Implausible glucose values",
    "Timestamp gaps",
    "Max gap (minutes)"
  ) %in% names(display)))
  expect_false(any(grepl("_", names(display), fixed = TRUE)))
  expect_equal(names(display)[seq_len(3)], c("Subject ID", "QC status", "Review notes"))
})

test_that("prepare_qc_display hides Subject ID for one filename-derived subject", {
  data <- data.frame(
    id = "OneFile",
    timestamp = seq(
      parse_cgm_timestamp("2026-05-05 00:00:00"),
      parse_cgm_timestamp("2026-05-05 23:00:00"),
      by = "hour"
    ),
    glucose = 100,
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = "OneFile.csv",
    imputed_flag = FALSE,
    id_source = subject_id_source_filename(),
    stringsAsFactors = FALSE
  )

  display <- prepare_qc_display(compute_qc_summary(data, valid_day_hours = 1), data)

  expect_false("Subject ID" %in% names(display))
  expect_equal(names(display)[seq_len(2)], c("QC status", "Review notes"))
})

test_that("duplicate timestamp note appears only when duplicates exist", {
  data <- data.frame(
    id = c("A", "A", "A"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00"
    )),
    glucose = c(100, 110, 120),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )
  no_duplicate_data <- data
  no_duplicate_data$timestamp <- parse_cgm_timestamp(c(
    "2026-05-05 08:00:00",
    "2026-05-05 08:05:00",
    "2026-05-05 08:10:00"
  ))

  note <- duplicate_timestamp_note(compute_qc_summary(data), data)
  no_note <- duplicate_timestamp_note(compute_qc_summary(no_duplicate_data), no_duplicate_data)

  expect_null(no_note)
  expect_equal(note$total_duplicate_timestamps, 1)
  expect_equal(note$affected_subjects, 1)
  expect_match(note$message, "Subject ID")
  expect_match(note$message, "Review duplicate timestamps")
})

test_that("quality calendar renders through dynamic output container", {
  html <- paste(as.character(qc_module_ui("qc")), collapse = "\n")
  data <- data.frame(
    id = c("A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(100, NA, 120),
    stringsAsFactors = FALSE
  )
  calendar <- compute_missingness_calendar_data(data)
  dimensions <- missingness_calendar_dimensions(calendar, show_subject_id = TRUE)

  expect_true(grepl("qc-missingness_heatmap_ui", html, fixed = TRUE))
  expect_false(grepl("height=\"420px\"", html, fixed = TRUE))
  expect_true(nrow(calendar) >= 2L)
  expect_true(is.numeric(dimensions$height))
  expect_true(dimensions$height > 0)
})

test_that("quality imputation UI is rendered only when imputation is selected", {
  qc_html <- paste(as.character(qc_module_ui("qc")), collapse = "\n")
  off_settings <- create_reproducibility_settings(imputation_method = "none")
  on_settings <- create_reproducibility_settings(imputation_method = "mice_only")
  status <- summarize_imputation_status(
    data.frame(id = "A", timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"), glucose = NA_real_),
    data.frame(id = "A", timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"), glucose = 100, imputed_flag = TRUE),
    on_settings
  )

  expect_true(grepl("qc-imputation_status", qc_html, fixed = TRUE))
  expect_false(grepl(">Imputation Status<", qc_html, fixed = TRUE))
  expect_false(should_show_analysis_missingness(off_settings))
  expect_true(should_show_analysis_missingness(on_settings))
  expect_true("Backend" %in% names(status))
})
