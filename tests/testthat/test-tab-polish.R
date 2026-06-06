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

test_that("timestamp validation summary reports date-only parsed values", {
  upload <- list(
    data = data.frame(
      time = c("2020-12-25", "2019-12-29", "2019-11-18"),
      glucose = c(100, 110, 120),
      stringsAsFactors = FALSE
    ),
    files = "sample.csv"
  )
  mapping <- list(timestamp = "time", glucose = "glucose")

  summary <- timestamp_validation_summary(upload, mapping)
  display <- timestamp_summary_display(summary)
  warnings <- data_validation_warnings(summary, NULL)

  expect_equal(summary$parsed_timestamps, 3)
  expect_equal(summary$failed_timestamps, 0)
  expect_equal(summary$date_only_timestamps, 3)
  expect_equal(display$Value[display$Label == "Date-only values"], "3")
  expect_true(any(grepl("parsed at midnight", warnings, fixed = TRUE)))
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

test_that("data status strip adds validation state to upload summary", {
  upload <- list(
    data = data.frame(time = "2026-05-05 08:00:00", glucose = 100),
    files = "sample.csv"
  )
  mapping <- list(timestamp = "time", glucose = "glucose", source_units = "mg/dL")
  standardized <- data.frame(
    id = "sample",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    stringsAsFactors = FALSE
  )
  settings <- list(analysis_date_range = c(start = "2026-05-05", end = "2026-05-05"))

  summary <- data_status_summary(upload, mapping, standardized, settings = settings)
  html <- paste(as.character(data_status_strip_ui(upload, mapping, standardized, settings = settings)), collapse = "\n")

  expect_true("Validation" %in% summary$Label)
  expect_equal(summary$Value[summary$Label == "Validation"], "Ready")
  expect_true(grepl("Current dataset", html, fixed = TRUE))
  expect_true(grepl("cgm-data-status-strip", html, fixed = TRUE))
})

test_that("app UI includes dynamic version badge and theme CSS", {
  description_path <- system.file("DESCRIPTION", package = "CGMAnalyzerShiny")
  expected_version <- paste0(
    "v",
    as.character(read.dcf(description_path, fields = "Version")[[1L]])
  )
  ui <- app_ui()
  brand_html <- paste(as.character(app_brand_ui()), collapse = "\n")
  css_html <- paste(as.character(app_theme_css()), collapse = "\n")

  expect_true(file.exists(description_path))
  expect_s3_class(ui, "shiny.tag.list")
  expect_equal(app_version_label(), expected_version)
  expect_true(grepl("cgm-brand-stack", brand_html, fixed = TRUE))
  expect_true(grepl("cgm-version-label", brand_html, fixed = TRUE))
  expect_true(grepl(expected_version, brand_html, fixed = TRUE))
  expect_true(grepl("CGMAnalyzerShiny theme polish", css_html, fixed = TRUE))
  expect_true(file.exists(app_theme_css_path()))
})

test_that("data workflow tabs render the expected stages", {
  html <- paste(as.character(data_workflow_tabs_ui(
    setup_ui = shiny::div(id = "setup-marker"),
    validate_ui = shiny::div(id = "validate-marker"),
    impute_ui = shiny::div(id = "impute-marker"),
    preview_ui = shiny::div(id = "preview-marker")
  )), collapse = "\n")

  expect_true(grepl("cgm-data-workflow", html, fixed = TRUE))
  expect_true(grepl("data_stage_tabs", html, fixed = TRUE))
  expect_true(grepl("Setup", html, fixed = TRUE))
  expect_true(grepl("Validate", html, fixed = TRUE))
  expect_true(grepl("Impute", html, fixed = TRUE))
  expect_true(grepl("Preview", html, fixed = TRUE))
  expect_true(grepl("setup-marker", html, fixed = TRUE))
  expect_true(grepl("preview-marker", html, fixed = TRUE))
})

test_that("preprocessing settings and imputation UI can render separately", {
  settings_html <- paste(as.character(preprocessing_settings_ui("preprocessing")), collapse = "\n")
  imputation_html <- paste(as.character(preprocessing_imputation_ui("preprocessing")), collapse = "\n")

  expect_true(grepl("preprocessing-tir_lower", settings_html, fixed = TRUE))
  expect_true(grepl("preprocessing-analysis_date_range_ui", settings_html, fixed = TRUE))
  expect_true(grepl("preprocessing-expected_study_duration_days", settings_html, fixed = TRUE))
  expect_false(grepl("preprocessing-imputation", settings_html, fixed = TRUE))
  expect_true(grepl("preprocessing-imputation", imputation_html, fixed = TRUE))
  expect_true(grepl("preprocessing-imputation_summary", imputation_html, fixed = TRUE))
  expect_false(grepl("preprocessing-tir_lower", imputation_html, fixed = TRUE))
})

test_that("metrics module UI renders dashboard sections without tabsets", {
  html <- paste(as.character(metrics_module_ui("metrics")), collapse = "\n")

  expect_true(grepl("cgm-metrics-dashboard", html, fixed = TRUE))
  expect_true(grepl("cgm-metrics-overview", html, fixed = TRUE))
  expect_true(grepl("cgm-dashboard-section", html, fixed = TRUE))
  expect_true(grepl("cgm-metrics-filter-bar", html, fixed = TRUE))
  expect_true(grepl("metrics-summary_cards", html, fixed = TRUE))
  expect_true(grepl("metrics-optional_metric_note", html, fixed = TRUE))
  expect_true(grepl("metrics-participant_filter", html, fixed = TRUE))
  expect_true(grepl("metrics-category_filter", html, fixed = TRUE))
  expect_true(grepl("metrics-group_filter", html, fixed = TRUE))
  expect_true(grepl("metrics-metrics_empty_state", html, fixed = TRUE))
  expect_true(grepl("metrics-metrics_table", html, fixed = TRUE))
  expect_false(grepl("tabset", html, ignore.case = TRUE))
  expect_false(grepl("nav-tabs", html, fixed = TRUE))
})

test_that("complexity module UI renders dashboard sections without inline CSS or tabsets", {
  html <- paste(as.character(complexity_module_ui("complexity")), collapse = "\n")

  expect_true(grepl("cgm-complexity-dashboard", html, fixed = TRUE))
  expect_true(grepl("cgm-complexity-controls-section", html, fixed = TRUE))
  expect_true(grepl("cgm-complexity-visual-filter-bar", html, fixed = TRUE))
  expect_true(grepl("cgm-complexity-table-section", html, fixed = TRUE))
  expect_true(grepl("complexity-summary_cards", html, fixed = TRUE))
  expect_true(grepl("complexity-mse_status_note", html, fixed = TRUE))
  expect_true(grepl("complexity-subject_filter", html, fixed = TRUE))
  expect_true(grepl("complexity-group_filter", html, fixed = TRUE))
  expect_true(grepl("complexity-metric_filter", html, fixed = TRUE))
  expect_true(grepl("complexity-curve_filter", html, fixed = TRUE))
  expect_true(grepl("complexity-complexity_plot_ui", html, fixed = TRUE))
  expect_true(grepl("complexity-metrics_table_ui", html, fixed = TRUE))
  expect_true(grepl("complexity-download_complexity", html, fixed = TRUE))
  expect_false(grepl("<style", html, fixed = TRUE))
  expect_false(grepl("tabset", html, ignore.case = TRUE))
  expect_false(grepl("nav-tabs", html, fixed = TRUE))
})

test_that("plots module UI renders dashboard visual section without tabsets", {
  html <- paste(as.character(plots_module_ui("plots")), collapse = "\n")

  expect_true(grepl("cgm-plots-dashboard", html, fixed = TRUE))
  expect_true(grepl("cgm-plots-overview", html, fixed = TRUE))
  expect_true(grepl("cgm-plots-visual-section", html, fixed = TRUE))
  expect_true(grepl("cgm-plots-header-actions", html, fixed = TRUE))
  expect_true(grepl("plots-filter_layout", html, fixed = TRUE))
  expect_true(grepl("plots-plot_summary", html, fixed = TRUE))
  expect_true(grepl("plots-active_plot", html, fixed = TRUE))
  expect_true(grepl("plots-download_plot", html, fixed = TRUE))
  expect_true(grepl("cgm-plots-summary", html, fixed = TRUE))
  expect_true(grepl("cgm-plot-panel", html, fixed = TRUE))
  expect_false(grepl("<style", html, fixed = TRUE))
  expect_false(grepl("tabset", html, ignore.case = TRUE))
  expect_false(grepl("nav-tabs", html, fixed = TRUE))
})

test_that("statistics module UI renders dashboard sections without tabsets", {
  html <- paste(as.character(stats_module_ui("stats")), collapse = "\n")

  expect_true(grepl("cgm-statistics-dashboard", html, fixed = TRUE))
  expect_true(grepl("cgm-statistics-overview", html, fixed = TRUE))
  expect_true(grepl("cgm-statistics-filter-bar", html, fixed = TRUE))
  expect_true(grepl("cgm-statistics-results-grid", html, fixed = TRUE))
  expect_true(grepl("stats-metric", html, fixed = TRUE))
  expect_true(grepl("stats-grouping", html, fixed = TRUE))
  expect_true(grepl("stats-period_filter", html, fixed = TRUE))
  expect_true(grepl("stats-test_type", html, fixed = TRUE))
  expect_true(grepl("stats-stats_status_note", html, fixed = TRUE))
  expect_true(grepl("stats-group_summary", html, fixed = TRUE))
  expect_true(grepl("stats-test_result_summary", html, fixed = TRUE))
  expect_true(grepl("stats-test_result", html, fixed = TRUE))
  expect_false(grepl("tabset", html, ignore.case = TRUE))
  expect_false(grepl("nav-tabs", html, fixed = TRUE))
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
  expect_true(grepl("cgm-scroll-table", html, fixed = TRUE))
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
  expect_true(grepl("imputation_model", on_html, fixed = TRUE))
  expect_true(grepl("LightGBM", on_html, fixed = TRUE))
  expect_true(grepl("run_imputation", on_html, fixed = TRUE))
  expect_false(grepl("imputation_seed", on_html, fixed = TRUE))
  expect_false(grepl("imputation_backend", on_html, fixed = TRUE))
  expect_false(grepl("Python/sklearn", on_html, fixed = TRUE))
  expect_false(grepl("imputation_interval_minutes", on_html, fixed = TRUE))
  expect_false(grepl("imputation_missing_warning_threshold", on_html, fixed = TRUE))
  expect_false(grepl("imputation_arima_threshold", on_html, fixed = TRUE))
  expect_false(grepl("imputation_arima_min_history", on_html, fixed = TRUE))
  expect_false(grepl("imputation_arima_order", on_html, fixed = TRUE))
  expect_false(grepl("imputation_lag_values", on_html, fixed = TRUE))
  expect_false(grepl("imputation_add_rollmean", on_html, fixed = TRUE))
  expect_false(grepl("imputation_roll_window", on_html, fixed = TRUE))
  expect_false(grepl("imputation_xgb_rounds", on_html, fixed = TRUE))
  expect_false(grepl("imputation_rf_trees", on_html, fixed = TRUE))
  expect_false(grepl("imputation_knn_k", on_html, fixed = TRUE))
  expect_false(grepl("imputation_lgb_rounds", on_html, fixed = TRUE))
  expect_false(grepl("imputation_study_start", on_html, fixed = TRUE))
  expect_false(grepl("imputation_study_end", on_html, fixed = TRUE))
})

test_that("imputation run status UI reflects explicit run states", {
  off_html <- paste(as.character(imputation_run_status_ui(list(state = "not_run"), "none")), collapse = "\n")
  running_html <- paste(as.character(imputation_run_status_ui(list(state = "running"), "mice_only")), collapse = "\n")
  stale_html <- paste(as.character(imputation_run_status_ui(list(state = "stale"), "mice_only")), collapse = "\n")
  failed_html <- paste(as.character(imputation_run_status_ui(list(state = "failed", message = "Imputation failed."), "mice_only")), collapse = "\n")

  expect_equal(off_html, "")
  expect_true(grepl("running", running_html, fixed = TRUE))
  expect_true(grepl("changed", stale_html, fixed = TRUE))
  expect_true(grepl("Imputation failed", failed_html, fixed = TRUE))
})

test_that("preprocessing module returns explicit imputation settings through testServer", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-06 08:00:00")),
    glucose = c(100, 110),
    stringsAsFactors = FALSE
  )
  mapping <- list(
    timestamp = "timestamp",
    glucose = "glucose",
    source_units = "mg/dL",
    upload_mode = "single_file"
  )

  shiny::testServer(
    preprocessing_module_server,
    args = list(
      mapping = shiny::reactive(mapping),
      standardized = shiny::reactive(data)
    ),
    {
      session$setInputs(
        tir_lower = 75,
        tir_upper = 175,
        tbr_level2 = 55,
        tar_level2 = 240,
        valid_day_hours = 12,
        analysis_date_range = as.Date(c("2026-05-05", "2026-05-06")),
        expected_study_duration_days = 2,
        imputation = "mice_only",
        imputation_model = "xgboost",
        run_imputation = 1
      )

      current <- settings()
      expect_equal(current$thresholds_mg_dl$tir_lower, 75)
      expect_equal(current$valid_day_hours, 12)
      expect_equal(as.Date(current$analysis_date_range[["start"]]), as.Date("2026-05-05"))
      expect_equal(current$expected_study_duration_days, 2)
      expect_equal(current$imputation_method, "mice_only")
      expect_equal(current$imputation_model, "xgboost")
      expect_equal(current$imputation_backend, "mice")
      expect_equal(current$imputation_interval_minutes, 5L)
      expect_equal(as.Date(current$imputation_study_start), as.Date("2026-05-05"))
      expect_equal(attr(settings, "imputation_run", exact = TRUE)(), 1)

      set_status <- attr(settings, "set_imputation_status", exact = TRUE)
      set_status(list(state = "stale", message = "Changed inputs require another imputation run."))
      expect_equal(attr(settings, "imputation_status", exact = TRUE)()$state, "stale")
    }
  )
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
  expect_false(any(c(
    "Expected duration",
    "Short study windows",
    "Full missing days",
    "Low coverage days"
  ) %in% cards$Label))
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
