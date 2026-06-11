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

  summary <- compute_qc_summary(data, valid_day_hours = 1)
  display <- prepare_qc_display(summary, data)
  forced <- prepare_qc_display(summary, data, show_subject_id = TRUE)

  expect_false("Subject ID" %in% names(display))
  expect_equal(names(display)[seq_len(2)], c("QC status", "Review notes"))
  expect_true("Subject ID" %in% names(forced))
  expect_equal(forced[["Subject ID"]], "OneFile")
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

test_that("quality calendar data drives dynamic plot dimensions", {
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

  expect_true(nrow(calendar) >= 2L)
  expect_true(is.numeric(dimensions$height))
  expect_true(dimensions$height > 0)
})

test_that("quality imputation status is summarized only when imputation is selected", {
  off_settings <- create_reproducibility_settings(imputation_method = "none")
  on_settings <- create_reproducibility_settings(imputation_method = "mice_only")
  status <- summarize_imputation_status(
    data.frame(id = "A", timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"), glucose = NA_real_),
    data.frame(id = "A", timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"), glucose = 100, imputed_flag = TRUE),
    on_settings
  )

  expect_false(should_show_analysis_missingness(off_settings))
  expect_true(should_show_analysis_missingness(on_settings))
  expect_true("Model" %in% names(status))
  expect_false("Backend" %in% names(status))
  expect_true("Missing warning threshold (%)" %in% names(status))
})

test_that("quality imputation warning text is split into list items", {
  items <- quality_imputation_warning_list(paste(
    "Warnings:",
    "Long contiguous missing glucose blocks were detected after timestamp-gap regularization:",
    "Subject 01VV_CGM has a contiguous missing block of at least one full day.",
    "Subject K25_CGM has a contiguous missing block of at least one half day. |",
    "Number of logged events: 2"
  ))

  expect_equal(length(items), 2)
  expect_match(items[[1L]], "Subject 01VV_CGM has a contiguous missing block", fixed = TRUE)
  expect_match(items[[2L]], "Subject K25_CGM has a contiguous missing block", fixed = TRUE)
  expect_false(any(grepl("logged events", items, ignore.case = TRUE)))
})

test_that("quality imputation warnings are grouped and expandable", {
  warnings <- quality_imputation_warning_items(paste(
    "Subject 01VV_CGM: High missingness after timestamp-gap regularization: 54.8% of glucose values are missing.",
    "Subject 01VV_CGM has a contiguous missing block of at least one full day. |",
    "Subject K25_CGM: High missingness after timestamp-gap regularization: 26.3% of glucose values are missing. |",
    "Number of logged events: 2"
  ))
  collapsed <- paste(as.character(quality_imputation_warning_groups_ui(
    warnings,
    show_all = FALSE,
    toggle_id = "toggle_warnings",
    max_groups = 1L,
    max_items_per_group = 1L
  )), collapse = "\n")
  expanded <- paste(as.character(quality_imputation_warning_groups_ui(
    warnings,
    show_all = TRUE,
    toggle_id = "toggle_warnings"
  )), collapse = "\n")

  expect_equal(unique(warnings$Group), c("Subject 01VV_CGM", "Subject K25_CGM"))
  expect_true(grepl("Show all warnings", collapsed, fixed = TRUE))
  expect_true(grepl("Show fewer warnings", expanded, fixed = TRUE))
  expect_false(grepl("Number of logged events", expanded, fixed = TRUE))
  expect_true(grepl("Subject 01VV_CGM", expanded, fixed = TRUE))
  expect_true(grepl("High missingness", expanded, fixed = TRUE))
  expect_true(grepl("Contiguous full-day block", expanded, fixed = TRUE))
})

test_that("quality imputation warnings merge repeated subject groups", {
  warnings <- quality_imputation_warning_items(paste(
    "Subject 01VV_CGM: High missingness after timestamp-gap regularization: 54.8% of glucose values are missing. |",
    "Subject 01VV_CGM has a contiguous missing block of at least one full day (173.6 hours), from 2021-01-21 12:53:33 to 2021-01-28 18:23:33. |",
    "Subject 01VV_CGM has a contiguous missing block of at least one full day (27.0 hours), from 2021-03-03 17:01:33 to 2021-03-04 19:56:33. |",
    "Timestamp-gap review found dataset-level issues. |",
    "Number of logged events: 4"
  ))
  html <- paste(as.character(quality_imputation_warning_groups_ui(
    warnings,
    show_all = TRUE,
    toggle_id = "toggle_warnings"
  )), collapse = "\n")
  subject_occurrences <- gregexpr("Subject 01VV_CGM", html, fixed = TRUE)[[1L]]

  expect_equal(sum(warnings$Group == "Subject 01VV_CGM"), 3)
  expect_equal(length(unique(warnings$GroupKey[warnings$Group == "Subject 01VV_CGM"])), 1)
  expect_equal(length(subject_occurrences[subject_occurrences > 0L]), 1)
  expect_true(grepl("Dataset warnings", html, fixed = TRUE))
  expect_false(grepl("Number of logged events", html, fixed = TRUE))
})

test_that("quality imputation status renders grouped warnings and method details", {
  status <- data.frame(
    Method = "MICE + ARIMA / XGBoost",
    Model = "Auto",
    Status = "Applied",
    `Method details` = "ARIMA: 1 Subject ID(s); XGBoost: 1 Subject ID(s)",
    `Filled glucose rows` = 2,
    `Original missing glucose` = 2,
    `Original missing glucose (%)` = 10,
    `Analysis missing glucose` = 0,
    `Analysis missing glucose (%)` = 0,
    `Estimated missing readings from gaps` = 0,
    Warnings = paste(
      "Long contiguous missing glucose blocks were detected after timestamp-gap regularization:",
      "Subject 01VV_CGM has a contiguous missing block.",
      "Subject K25_CGM has a contiguous missing block. |",
      "Number of logged events: 2"
    ),
    Message = "Imputation filled 2 missing glucose row(s).",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  html <- paste(as.character(quality_imputation_status_ui(status)), collapse = "\n")

  expect_true(grepl("Warnings", html, fixed = TRUE))
  expect_false(grepl("Warnings and logged events", html, fixed = TRUE))
  expect_false(grepl("Number of logged events", html, fixed = TRUE))
  expect_true(grepl("Subject-level methods", html, fixed = TRUE))
  expect_true(grepl("Subject 01VV_CGM", html, fixed = TRUE))
  expect_true(grepl("Subject K25_CGM", html, fixed = TRUE))
})

test_that("quality imputation status hides method details without subject metadata", {
  status <- data.frame(
    Method = "MICE + ARIMA / XGBoost",
    Model = "Auto",
    Status = "Applied",
    `Method details` = "",
    `Filled glucose rows` = 2,
    `Original missing glucose` = 2,
    `Original missing glucose (%)` = 10,
    `Analysis missing glucose` = 0,
    `Analysis missing glucose (%)` = 0,
    `Estimated missing readings from gaps` = 0,
    Warnings = "",
    Message = "Imputation filled 2 missing glucose row(s).",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  html <- paste(as.character(quality_imputation_status_ui(status)), collapse = "\n")

  expect_false(grepl("Subject-level methods", html, fixed = TRUE))
})

test_that("quality QC review summary counts review flags", {
  ok <- data.frame(
    id = "A",
    timestamp = seq(
      parse_cgm_timestamp("2026-05-05 00:00:00"),
      parse_cgm_timestamp("2026-05-05 23:00:00"),
      by = "hour"
    ),
    glucose = 100,
    stringsAsFactors = FALSE
  )
  review <- data.frame(
    id = c("B", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:00:00"
    )),
    glucose = c(120, 500),
    stringsAsFactors = FALSE
  )
  data <- rbind(ok, review)
  data$units <- "mg/dL"
  data$device <- NA_character_
  data$group <- NA_character_
  data$source_file <- NA_character_
  data$imputed_flag <- FALSE
  data$id_source <- subject_id_source_mapped()
  qc <- compute_qc_summary(data, valid_day_hours = 1)
  summary <- quality_qc_review_summary(qc, data)

  expect_equal(summary$cards$Value[summary$cards$Label == "Subjects OK"], "1")
  expect_equal(summary$cards$Value[summary$cards$Label == "Needs review"], "1")
  expect_equal(summary$cards$Value[summary$cards$Label == "Duplicate timestamps"], "1")
  expect_equal(summary$cards$Value[summary$cards$Label == "Implausible values"], "1")
  expect_equal(nrow(summary$review_rows), 1)
  expect_equal(summary$review_rows[["Subject ID"]], "B")
})

test_that("quality module UI renders simplified dashboard sections without tabsets", {
  html <- paste(as.character(qc_module_ui("qc")), collapse = "\n")

  expect_true(grepl("cgm-quality-dashboard", html, fixed = TRUE))
  expect_true(grepl("cgm-quality-overview", html, fixed = TRUE))
  expect_true(grepl("cgm-quality-section", html, fixed = TRUE))
  expect_true(grepl("cgm-quality-section-header", html, fixed = TRUE))
  expect_true(grepl("cgm-quality-qc-section", html, fixed = TRUE))
  expect_true(grepl("cgm-quality-calendar-section", html, fixed = TRUE))

  expect_true(grepl("qc-qc_summary_cards", html, fixed = TRUE))
  expect_true(grepl("qc-duplicate_timestamp_note", html, fixed = TRUE))
  expect_true(grepl("qc-qc_review_ui", html, fixed = TRUE))
  expect_true(grepl("qc-imputation_status", html, fixed = TRUE))
  expect_true(grepl("qc-missingness_subject_filter", html, fixed = TRUE))
  expect_true(grepl("qc-missingness_heatmap_ui", html, fixed = TRUE))

  expect_false(grepl("Study window coverage", html, fixed = TRUE))
  expect_false(grepl("Missingness Summary", html, fixed = TRUE))
  expect_false(grepl("qc-study_window_table", html, fixed = TRUE))
  expect_false(grepl("qc-missingness_comparison_table", html, fixed = TRUE))
  expect_false(grepl("tabset", html, ignore.case = TRUE))
  expect_false(grepl("nav-tabs", html, fixed = TRUE))
})

test_that("quality module returns QC summaries for the active Quality tab", {
  data <- data.frame(
    id = c("A", "A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:30:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(100, NA, 120, 130),
    units = "mg/dL",
    device = NA_character_,
    group = c("Control", "Control", "Control", "Treatment"),
    source_file = c("A.csv", "A.csv", "A.csv", "B.csv"),
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )
  settings <- create_reproducibility_settings(
    valid_day_hours = 1,
    analysis_date_range = c(start = as.Date("2026-05-05"), end = as.Date("2026-05-06")),
    imputation_method = "none"
  )

  shiny::testServer(
    qc_module_server,
    args = list(
      standardized = shiny::reactive(data),
      analysis_data = shiny::reactive(data),
      settings = shiny::reactive(settings),
      active_tab = function() "quality"
    ),
    {
      summary <- qc_summary()
      expect_equal(nrow(summary), 2)
      expect_equal(sum(summary$readings), 4)
      expect_equal(sum(summary$missing_glucose), 1)
      expect_true(any(summary$gap_count > 0))

      session$setInputs(missingness_participant = "A")
      filtered_calendar <- missingness_calendar_data()
      expect_true(nrow(filtered_calendar) > 0)
      expect_equal(unique(filtered_calendar$id), "A")
    }
  )
})

test_that("quality module reuses supplied missingness precompute reactives", {
  data <- data.frame(
    id = c("A", "A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:30:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(100, NA, 120, 130),
    units = "mg/dL",
    device = NA_character_,
    group = c("Control", "Control", "Control", "Treatment"),
    source_file = c("A.csv", "A.csv", "A.csv", "B.csv"),
    imputed_flag = FALSE,
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )
  settings <- create_reproducibility_settings(
    valid_day_hours = 1,
    analysis_date_range = c(start = as.Date("2026-05-05"), end = as.Date("2026-05-06")),
    imputation_method = "none"
  )
  analysis_calls <- 0L
  standardized_calls <- 0L
  analysis_precompute <- shiny::reactive({
    analysis_calls <<- analysis_calls + 1L
    missingness_precompute(data)
  })
  standardized_precompute <- shiny::reactive({
    standardized_calls <<- standardized_calls + 1L
    analysis_precompute()
  })

  shiny::testServer(
    qc_module_server,
    args = list(
      standardized = shiny::reactive(data),
      analysis_data = shiny::reactive(data),
      settings = shiny::reactive(settings),
      active_tab = function() "quality",
      standardized_missingness_precompute = standardized_precompute,
      analysis_missingness_precompute = analysis_precompute
    ),
    {
      comparison <- missingness_comparison()
      gaps <- gap_periods()
      calendar <- missingness_calendar_all()

      expect_true(nrow(comparison) > 0)
      expect_true(nrow(gaps) > 0)
      expect_true(nrow(calendar) > 0)
      expect_equal(analysis_calls, 1L)
      expect_equal(standardized_calls, 1L)
    }
  )
})
