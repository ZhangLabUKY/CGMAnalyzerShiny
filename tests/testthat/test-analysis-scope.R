test_that("analysis date range defaults to full timestamp span", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-07 08:00:00")),
    glucose = c(100, 120),
    stringsAsFactors = FALSE
  )

  expect_equal(
    available_analysis_date_range(data),
    c(start = "2026-05-05", end = "2026-05-07")
  )
  expect_equal(
    normalize_analysis_date_range(NULL, data),
    c(start = "2026-05-05", end = "2026-05-07")
  )
})

test_that("analysis date range filters inclusively by calendar date", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 23:59:00",
      "2026-05-06 00:00:00",
      "2026-05-06 23:59:00",
      "2026-05-07 00:00:00"
    )),
    glucose = c(90, 100, 110, 120),
    stringsAsFactors = FALSE
  )

  filtered <- filter_analysis_date_range(data, c("2026-05-06", "2026-05-06"))

  expect_equal(nrow(filtered), 2)
  expect_true(all(as.Date(filtered$timestamp) == as.Date("2026-05-06")))
})

test_that("analysis date range is stored in reproducibility settings and signature", {
  settings <- create_reproducibility_settings(
    analysis_date_range = c(start = "2026-05-05", end = "2026-05-07")
  )
  changed <- settings
  changed$analysis_date_range <- c(start = "2026-05-06", end = "2026-05-07")

  expect_equal(settings$analysis_date_range, c(start = "2026-05-05", end = "2026-05-07"))
  expect_false(identical(analysis_date_range_signature(settings), analysis_date_range_signature(changed)))
})

test_that("expected study duration is stored and summarized per subject", {
  settings <- create_reproducibility_settings(expected_study_duration_days = 4)
  changed <- settings
  changed$expected_study_duration_days <- 5L
  data <- data.frame(
    id = c("A", "A", "B", "B"),
    id_source = subject_id_source_mapped(),
    timestamp = parse_cgm_timestamp(c(
      "2026-01-01 08:00:00",
      "2026-01-02 08:00:00",
      "2026-03-01 08:00:00",
      "2026-03-04 08:00:00"
    )),
    glucose = c(100, 110, 120, 130),
    stringsAsFactors = FALSE
  )

  summary <- study_window_summary(data, expected_duration_days = settings$expected_study_duration_days)
  no_expected <- study_window_summary(data, expected_duration_days = NA)

  expect_equal(settings$expected_study_duration_days, 4L)
  expect_false(identical(expected_study_duration_signature(settings), expected_study_duration_signature(changed)))
  expect_equal(summary[["Observed days"]], c(2L, 4L))
  expect_equal(summary[["Shortfall days"]], c(2L, 0L))
  expect_equal(summary[["Study window status"]], c("Short observed span", "Complete observed span"))
  expect_true(all(is.na(no_expected[["Expected days"]])))
  expect_equal(no_expected[["Study window status"]], rep("No expected duration set", 2))
})

test_that("study window summary can force selected filename-derived Subject ID", {
  data <- data.frame(
    id = "FallbackA",
    id_source = subject_id_source_filename(),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-06 08:00:00")),
    glucose = c(100, 110),
    stringsAsFactors = FALSE
  )

  hidden <- study_window_summary(data)
  forced <- study_window_summary(data, show_subject_id = TRUE)

  expect_false("Subject ID" %in% names(hidden))
  expect_true("Subject ID" %in% names(forced))
  expect_equal(forced[["Subject ID"]], "FallbackA")
})

test_that("imputation settings are stored and included in signature", {
  settings <- create_reproducibility_settings(
    imputation_method = "mice_only",
    imputation_model = "auto",
    imputation_backend = "mice",
    imputation_interval_minutes = 5L,
    imputation_missing_warning_threshold = 0.20,
    imputation_arima_threshold = 0.05,
    imputation_arima_order = c(4L, 1L, 0L),
    imputation_arima_min_history = 20L,
    imputation_xgb_rounds = 300L,
    imputation_rf_trees = 200L,
    imputation_knn_k = 7L,
    imputation_lgb_rounds = 400L,
    imputation_lag_values = c(1L, 2L, 3L),
    imputation_add_rollmean = TRUE,
    imputation_roll_window = 3L,
    imputation_study_start = "2026-05-01",
    imputation_study_end = "2026-05-14"
  )
  changed <- settings
  changed$imputation_model <- "xgboost"
  changed_interval <- settings
  changed_interval$imputation_interval_minutes <- 10L
  changed_backend <- settings
  changed_backend$imputation_backend <- "sklearn"

  expect_equal(settings$imputation_model, "auto")
  expect_equal(settings$imputation_backend, "mice")
  expect_equal(settings$imputation_interval_minutes, 5L)
  expect_equal(settings$imputation_missing_warning_threshold, 0.20)
  expect_equal(settings$imputation_arima_threshold, 0.05)
  expect_equal(settings$imputation_arima_order, c(4L, 1L, 0L))
  expect_equal(settings$imputation_arima_min_history, 20L)
  expect_equal(settings$imputation_xgb_rounds, 300L)
  expect_equal(settings$imputation_rf_trees, 200L)
  expect_equal(settings$imputation_knn_k, 7L)
  expect_equal(settings$imputation_lgb_rounds, 400L)
  expect_equal(settings$imputation_lag_values, c(1L, 2L, 3L))
  expect_true(settings$imputation_add_rollmean)
  expect_equal(settings$imputation_roll_window, 3L)
  expect_equal(settings$imputation_study_start, "2026-05-01")
  expect_equal(settings$imputation_study_end, "2026-05-14")
  expect_false(identical(imputation_settings_signature(settings), imputation_settings_signature(changed)))
  expect_false(identical(imputation_settings_signature(settings), imputation_settings_signature(changed_interval)))
  expect_false(identical(imputation_settings_signature(settings), imputation_settings_signature(changed_backend)))
})
