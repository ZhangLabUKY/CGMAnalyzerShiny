collect_polish_tag_ids <- function(ui) {
  ids <- character()
  walk <- function(node) {
    if (inherits(node, "shiny.tag")) {
      id <- node$attribs$id
      if (!is.null(id)) {
        ids <<- c(ids, id)
      }
      lapply(node$children, walk)
    } else if (inherits(node, "shiny.tag.list") || is.list(node)) {
      lapply(node, walk)
    }
    invisible(NULL)
  }
  walk(ui)
  ids
}

test_that("data setup status reports readiness steps", {
  upload <- list(
    data = data.frame(time = "2026-05-05 08:00:00", glucose = 100),
    files = "sample.csv"
  )
  mapping <- list(timestamp = "time", glucose = "glucose")
  standardized <- data.frame(
    id = "sample",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    stringsAsFactors = FALSE
  )
  settings <- list(analysis_date_range = c(start = "2026-05-05", end = "2026-05-05"))

  status <- data_setup_status(upload, mapping, standardized, settings = settings)

  expect_equal(status$Step, c("Files loaded", "Required mappings", "Timestamps parsed", "Analysis date range"))
  expect_true(all(status$Status == "Ready"))
  expect_true(any(grepl("2026-05-05", status$Detail, fixed = TRUE)))

  validation <- data_validation_rows(upload, mapping, settings = settings)
  expect_true(all(c("Timestamp parsing", "Glucose parsing") %in% validation$Step))
  expect_true(all(validation$Status == "Ready"))
})

test_that("uploaded data helper detects usable uploads", {
  expect_false(has_uploaded_data(NULL))
  expect_false(has_uploaded_data(list(data = data.frame(), files = character())))
  expect_false(has_uploaded_data(list(data = NULL, files = "sample.csv")))
  expect_true(has_uploaded_data(list(data = data.frame(glucose = 100), files = "sample.csv")))
})

test_that("data setup status identifies missing required mappings", {
  upload <- list(data = data.frame(time = "x", glucose = 100), files = "sample.csv")
  mapping <- list(timestamp = "", glucose = "glucose")

  status <- data_setup_status(upload, mapping)
  mappings <- status[status$Step == "Required mappings", , drop = FALSE]

  expect_equal(mappings$Status, "Needs attention")
  expect_true(grepl("Select timestamp", mappings$Detail, fixed = TRUE))

  validation <- data_validation_rows(upload, mapping)
  timestamp_row <- validation[validation$Step == "Timestamp column", , drop = FALSE]
  expect_equal(timestamp_row$Status, "Needs attention")
  expect_true(grepl("Select a timestamp column", timestamp_row$Detail, fixed = TRUE))
})

test_that("timestamp validation summary reports parsed, invalid, and ambiguous values", {
  upload <- list(data = data.frame(time = c("2026-05-05 08:00:00", "bad time", "01-02-2019 02:49")), files = "sample.csv")
  mapping <- list(timestamp = "time", glucose = "glucose")

  summary <- timestamp_validation_summary(upload, mapping)
  display <- timestamp_summary_display(summary)
  warnings <- data_validation_warnings(summary, NULL)

  expect_equal(summary$rows, 3)
  expect_equal(summary$parsed_timestamps, 2)
  expect_equal(summary$failed_timestamps, 1)
  expect_equal(summary$ambiguous_timestamps, 1)
  expect_true(grepl("bad time", summary$example_failed_values, fixed = TRUE))
  expect_equal(display$Value[display$Label == "Invalid timestamps"], "1")
  expect_true(any(grepl("Timestamp parsing needs review", warnings, fixed = TRUE)))
  expect_true(any(grepl("day-first", warnings, fixed = TRUE)))
})

test_that("glucose validation summary reports parsing and review warnings", {
  upload <- list(data = data.frame(glucose = c("100", "", "abc", "401")), files = "sample.csv")
  mapping <- list(timestamp = "time", glucose = "glucose", source_units = "mg/dL")

  summary <- glucose_validation_summary(upload, mapping)
  display <- glucose_summary_display(summary)
  warnings <- data_validation_warnings(NULL, summary)

  expect_equal(summary$rows, 4)
  expect_equal(summary$numeric_glucose, 2)
  expect_equal(summary$missing_glucose, 1)
  expect_equal(summary$non_numeric_glucose, 1)
  expect_equal(summary$implausible_glucose, 1)
  expect_equal(display$Value[display$Label == "Non-numeric glucose"], "1")
  expect_true(any(grepl("non-numeric glucose", warnings, fixed = TRUE)))
  expect_true(any(grepl("40-400 mg/dL", warnings, fixed = TRUE)))
})

test_that("glucose validation flags suspicious source units conservatively", {
  mmol_upload <- list(data = data.frame(glucose = c("100", "120", "140")), files = "sample.csv")
  mmol_mapping <- list(glucose = "glucose", source_units = "mmol/L")
  mgdl_upload <- list(data = data.frame(glucose = c("5.5", "6.2", "7.1")), files = "sample.csv")
  mgdl_mapping <- list(glucose = "glucose", source_units = "mg/dL")

  expect_true(glucose_validation_summary(mmol_upload, mmol_mapping)$suspicious_units)
  expect_true(glucose_validation_summary(mgdl_upload, mgdl_mapping)$suspicious_units)
})

test_that("data upload summary reports compact counts", {
  upload <- list(
    data = data.frame(time = c("2026-05-05 08:00:00", "2026-05-06 08:00:00"), glucose = c(100, NA)),
    files = c("a.csv", "b.csv")
  )
  standardized <- data.frame(
    id = c("A", "B"),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-06 08:00:00")),
    glucose = c(100, NA),
    stringsAsFactors = FALSE
  )

  summary <- data_upload_summary(upload, standardized)

  expect_equal(summary$Value[summary$Label == "Rows"], "2")
  expect_equal(summary$Value[summary$Label == "Files"], "2")
  expect_equal(summary$Value[summary$Label == "Subject IDs"], "2")
  expect_equal(summary$Value[summary$Label == "Missing glucose"], "1")
})

test_that("imputation missingness summary reports severity and affected subjects", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 50),
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00") + seq(0, by = 300, length.out = 100),
    glucose = rep(100, 100),
    stringsAsFactors = FALSE
  )
  none <- imputation_missingness_summary(data)
  data$glucose[seq_len(5)] <- NA
  acceptable <- imputation_missingness_summary(data)
  data$glucose[seq_len(10)] <- NA
  moderate_10 <- imputation_missingness_summary(data)
  data$glucose[seq_len(25)] <- NA
  moderate_25 <- imputation_missingness_summary(data)
  data$glucose[seq_len(26)] <- NA
  severe <- imputation_missingness_summary(data)

  expect_equal(none$severity, "No missing glucose")
  expect_equal(none$severity_class, "success")
  expect_true(grepl("No missing glucose", none$message, fixed = TRUE))
  expect_equal(acceptable$missing_glucose, 5)
  expect_equal(acceptable$missing_percent, 5)
  expect_equal(acceptable$estimated_missing_readings, 0)
  expect_equal(acceptable$subjects_affected, 1)
  expect_equal(acceptable$severity, "Acceptable missingness")
  expect_equal(acceptable$severity_class, "success")
  expect_equal(moderate_10$severity, "Moderate missingness")
  expect_equal(moderate_10$severity_class, "warning")
  expect_equal(moderate_25$severity, "Moderate missingness")
  expect_equal(moderate_25$severity_class, "warning")
  expect_false(moderate_25$imputation_warning)
  expect_equal(severe$severity, "Severe missingness")
  expect_equal(severe$severity_class, "danger")
  expect_true(severe$imputation_warning)
  expect_true(grepl("Imputation caution", severe$message, fixed = TRUE))
  expect_true(grepl("25%", severe$imputation_warning_message, fixed = TRUE))
})

test_that("imputation summary reports combined missing glucose values", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 00:00:00",
      "2026-05-05 00:05:00",
      "2026-05-05 00:10:00",
      "2026-05-05 00:30:00"
    )),
    glucose = c(100, NA, 120, 130),
    stringsAsFactors = FALSE
  )

  summary <- imputation_missingness_summary(data)
  cards <- imputation_summary_cards(summary)

  expect_equal(summary$missing_glucose, 4)
  expect_equal(summary$explicit_missing_glucose, 1)
  expect_equal(summary$estimated_missing_readings, 3)
  expect_equal(cards$Value[cards$Label == "Missing glucose values"], "4")
  expect_false("Explicit missing rows" %in% cards$Label)
  expect_false("Inferred gap readings" %in% cards$Label)
  expect_false("Rows after gap expansion" %in% cards$Label)
  expect_true(grepl("missing glucose values", summary$message, fixed = TRUE))
})

test_that("imputation summary includes per-subject rows for multiple Subject IDs", {
  data <- data.frame(
    id = c(rep("A", 4), rep("B", 4)),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 00:00:00",
      "2026-05-05 00:05:00",
      "2026-05-05 00:10:00",
      "2026-05-05 00:30:00",
      "2026-05-05 00:00:00",
      "2026-05-05 00:05:00",
      "2026-05-05 00:10:00",
      "2026-05-05 00:15:00"
    )),
    glucose = c(100, NA, 120, 130, 100, 110, 120, 130),
    source_file = "one_file.csv",
    stringsAsFactors = FALSE
  )

  summary <- imputation_missingness_summary(data)
  by_subject <- attr(summary, "subject_missingness", exact = TRUE)
  html <- paste(as.character(imputation_summary_box_ui(summary)), collapse = "\n")

  expect_s3_class(by_subject, "data.frame")
  expect_equal(by_subject[["Subject ID"]], c("A", "B"))
  expect_equal(by_subject[["Missing glucose values"]], c(4L, 0L))
  expect_true(grepl("Missingness by Subject ID", html, fixed = TRUE))
  expect_true(grepl("Missing glucose values include uploaded blank glucose values", html, fixed = TRUE))
})

test_that("imputation subject summary is hidden for one Subject ID", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 00:00:00",
      "2026-05-05 00:05:00",
      "2026-05-05 00:10:00",
      "2026-05-05 00:15:00"
    )),
    glucose = c(100, NA, 120, 130),
    stringsAsFactors = FALSE
  )

  summary <- imputation_missingness_summary(data)
  by_subject <- attr(summary, "subject_missingness", exact = TRUE)
  html <- paste(as.character(imputation_summary_box_ui(summary)), collapse = "\n")

  expect_equal(nrow(by_subject), 0)
  expect_false(grepl("Missingness by Subject ID", html, fixed = TRUE))
})

test_that("imputation option UI is conditional on selected method", {
  off_html <- paste(as.character(imputation_options_ui("none", ns = identity)), collapse = "\n")
  on_html <- paste(as.character(imputation_options_ui("mice_only", ns = identity)), collapse = "\n")

  expect_false(grepl("imputation_backend", off_html, fixed = TRUE))
  expect_false(grepl("imputation_xgb_rounds", off_html, fixed = TRUE))
  expect_true(grepl("imputation_seed", on_html, fixed = TRUE))
  expect_true(grepl("imputation_backend", on_html, fixed = TRUE))
  expect_true(grepl("Python/sklearn", on_html, fixed = TRUE))
  expect_true(grepl("imputation_interval_minutes", on_html, fixed = TRUE))
  expect_true(grepl("imputation_arima_threshold", on_html, fixed = TRUE))
  expect_true(grepl("imputation_arima_min_history", on_html, fixed = TRUE))
  expect_true(grepl("imputation_xgb_rounds", on_html, fixed = TRUE))
})

test_that("quality summary cards use QC and missingness values", {
  data <- data.frame(
    id = c("A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(100, NA, 120),
    imputed_flag = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  qc <- data.frame(id = c("A", "B"), valid_days = c(1, 2))
  missingness <- data.frame(
    `Timestamp gaps` = c(2, 3),
    `Missing glucose rows after preprocessing` = c(1, 0),
    `Filled glucose rows` = c(1, 0),
    `Full missing days` = c(1, 0),
    `Half-day or worse coverage days` = c(0, 2),
    check.names = FALSE
  )
  study_window <- data.frame(
    `Subject ID` = c("A", "B"),
    `Expected days` = c(4L, 4L),
    `Shortfall days` = c(1L, 0L),
    check.names = FALSE
  )
  day_coverage <- missingness[, c("Full missing days", "Half-day or worse coverage days"), drop = FALSE]

  cards <- quality_summary_cards(data, qc, missingness, study_window = study_window, day_coverage = day_coverage)

  expect_equal(cards$Value[cards$Label == "Subject IDs"], "2")
  expect_equal(cards$Value[cards$Label == "Valid days"], "3")
  expect_equal(cards$Value[cards$Label == "Timestamp gaps"], "5")
  expect_equal(cards$Value[cards$Label == "Filled rows"], "1")
  expect_equal(cards$Value[cards$Label == "Expected duration"], "4 days")
  expect_equal(cards$Value[cards$Label == "Short study windows"], "1")
  expect_equal(cards$Value[cards$Label == "Full missing days"], "1")
  expect_equal(cards$Value[cards$Label == "Low coverage days"], "2")
})

test_that("plot selection summary respects daily overlay day filtering", {
  data <- data.frame(
    id = c("A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-06 08:00:00",
      "2026-05-06 09:00:00"
    )),
    glucose = c(100, 110, 120),
    group = NA_character_,
    stringsAsFactors = FALSE
  )

  summary <- plot_selection_summary(data, plot_type = "daily_overlay", day = "2026-05-06")

  expect_equal(summary$Value[summary$Label == "Rows plotted"], "2")
  expect_equal(summary$Value[summary$Label == "Subject IDs"], "2")
  expect_equal(summary$Value[summary$Label == "Days"], "1")
  expect_equal(summary$Value[summary$Label == "Day selection"], "Selected days")
  expect_equal(summary$Value[summary$Label == "Date legend"], "Shown")
})

test_that("summary card UI uses shared classes across tabs", {
  summary <- data.frame(
    Label = c("Rows", "Subject IDs"),
    Value = c("10", "2"),
    stringsAsFactors = FALSE
  )
  html <- paste(as.character(summary_card_ui(summary)), collapse = "\n")
  compact_html <- paste(as.character(summary_card_ui(summary, compact = TRUE)), collapse = "\n")

  expect_true(grepl("cgm-summary-cards", html, fixed = TRUE))
  expect_equal(lengths(regmatches(html, gregexpr("card cgm-summary-card", html, fixed = TRUE))), 2L)
  expect_true(grepl("min-width:150px", html, fixed = TRUE))
  expect_true(grepl("min-width:132px", compact_html, fixed = TRUE))
})

test_that("main tab UI exposes polish summary outputs", {
  app_ids <- collect_polish_tag_ids(app_ui())
  qc_html <- paste(as.character(qc_module_ui("qc")), collapse = "\n")
  plots_html <- paste(as.character(plots_module_ui("plots")), collapse = "\n")
  metrics_html <- paste(as.character(metrics_module_ui("metrics")), collapse = "\n")
  combined_static_html <- paste(
    qc_html,
    plots_html,
    metrics_html,
    collapse = "\n"
  )

  expect_true("data_workflow_ui" %in% app_ids)
  expect_true(is.function(data_validation_panel_ui))
  expect_true(is.function(data_upload_summary))
  expect_true(is.function(column_mapping_module_ui))
  expect_true(is.function(preprocessing_module_ui))
  expect_true(is.function(upload_preview_ui))
  expect_false("column_mapping-mapping_note" %in% app_ids)
  expect_false("preprocessing-tir_lower" %in% app_ids)
  expect_false("upload-preview_rows" %in% app_ids)
  preprocessing_html <- paste(as.character(preprocessing_module_ui("preprocessing")), collapse = "\n")
  expect_true(grepl("preprocessing-imputation_panel", preprocessing_html, fixed = TRUE))
  expect_true(grepl("preprocessing-expected_study_duration_days", preprocessing_html, fixed = TRUE))
  expect_true(grepl("preprocessing-imputation_summary", preprocessing_html, fixed = TRUE))
  expect_true(grepl("preprocessing-imputation_options_ui", preprocessing_html, fixed = TRUE))
  expect_false(grepl("preprocessing-imputation_backend", preprocessing_html, fixed = TRUE))
  expect_true(grepl("qc-qc_summary_cards", qc_html, fixed = TRUE))
  expect_true(grepl("qc-study_window_table", qc_html, fixed = TRUE))
  expect_true(grepl("qc-day_coverage_warning_note", qc_html, fixed = TRUE))
  expect_true(grepl("plots-plot_summary", plots_html, fixed = TRUE))
  expect_equal(
    plot_download_filename(
      data.frame(
        id = "A",
        timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
        glucose = 100,
        stringsAsFactors = FALSE
      )
    ),
    "cgm_trace_all-subjects_dates-2026-05-05-to-2026-05-05.png"
  )
  expect_true(grepl("Metric overview", metrics_html, fixed = TRUE))
  expect_true(grepl("Detailed metrics", metrics_html, fixed = TRUE))
  expect_true(is.function(metric_summary_cards))
  expect_false(grepl(">Participant<", combined_static_html, fixed = TRUE))
  expect_true(grepl(">Daily data coverage<", qc_html, fixed = TRUE))
})

test_that("metrics filter row keeps category next to subject", {
  metrics_html <- paste(as.character(metrics_module_ui("metrics")), collapse = "\n")
  subject_pos <- regexpr("metrics-participant_filter", metrics_html, fixed = TRUE)[[1L]]
  category_pos <- regexpr("metrics-category_filter", metrics_html, fixed = TRUE)[[1L]]
  group_pos <- regexpr("metrics-group_filter", metrics_html, fixed = TRUE)[[1L]]

  expect_true(subject_pos > 0)
  expect_true(category_pos > subject_pos)
  expect_true(group_pos > category_pos)
  expect_false(grepl("metrics-visit_filter", metrics_html, fixed = TRUE))
})
