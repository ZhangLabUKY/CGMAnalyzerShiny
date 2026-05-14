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

test_that("imputation settings are stored and included in signature", {
  settings <- create_reproducibility_settings(
    imputation_method = "mice_only",
    imputation_backend = "mice",
    imputation_interval_minutes = 5L,
    imputation_arima_threshold = 0.05,
    imputation_arima_min_history = 20L,
    imputation_xgb_rounds = 300L
  )
  changed <- settings
  changed$imputation_backend <- "sklearn"
  changed_interval <- settings
  changed_interval$imputation_interval_minutes <- 10L

  expect_equal(settings$imputation_backend, "mice")
  expect_equal(settings$imputation_interval_minutes, 5L)
  expect_equal(settings$imputation_arima_threshold, 0.05)
  expect_equal(settings$imputation_arima_min_history, 20L)
  expect_equal(settings$imputation_xgb_rounds, 300L)
  expect_false(identical(imputation_settings_signature(settings), imputation_settings_signature(changed)))
  expect_false(identical(imputation_settings_signature(settings), imputation_settings_signature(changed_interval)))
})
