test_that("to_iglu_data converts standardized data shape", {
  demo <- example_missing_5pct_standardized(fill_missing = TRUE)

  iglu_data <- to_iglu_data(demo)

  expect_named(iglu_data, c("id", "time", "gl"))
  expect_equal(nrow(iglu_data), nrow(demo))
  expect_s3_class(iglu_data$time, "POSIXct")
})

test_that("regularize_cgm_series keeps regular example at five-minute interval", {
  demo <- example_missing_5pct_standardized(fill_missing = TRUE)

  one_id <- demo[demo$id == "11", , drop = FALSE]
  regular <- regularize_cgm_series(one_id)

  expect_equal(nrow(regular), 288)
  expect_equal(attr(regular, "interval_minutes"), 5)
  expect_false(any(is.na(regular$glucose)))
})

test_that("CGManalyzer adapter computes internal vector-backed metrics", {
  demo <- example_missing_5pct_standardized(fill_missing = TRUE)

  metrics <- compute_cgmanalyzer_metrics(demo)

  expect_equal(nrow(metrics), 5)
  expect_true(all(c("conga_12h", "conga_24h", "modd", "cgmanalyzer_status") %in% names(metrics)))
  expect_false("conga_2h" %in% names(metrics))
  expect_true(all(metrics$cgmanalyzer_status %in% c("internal_observed_pairs", "insufficient_data")))
  expect_true(all(is.finite(metrics$conga_12h) | is.na(metrics$conga_12h)))
  expect_true(all(is.finite(metrics$conga_24h) | is.na(metrics$conga_24h)))
  expect_true(all(is.finite(metrics$modd) | is.na(metrics$modd)))
})

test_that("timestamp-aware lag metrics match regular interval lag metrics", {
  timestamps <- seq(
    parse_cgm_timestamp("2026-05-05 00:00:00"),
    by = 5 * 60,
    length.out = 576
  )
  glucose <- 120 + sin(seq_along(timestamps) / 9) * 20 + rep(c(0, 12), each = 288)

  by_position <- optional_metrics_cpp(glucose, 5)
  by_time <- optional_lag_metrics_by_time_cpp(as.numeric(timestamps), glucose, 0)

  expect_equal(by_time$conga_12h, by_position$conga_12h, tolerance = 1e-8)
  expect_equal(by_time$conga_24h, by_position$conga_24h, tolerance = 1e-8)
  expect_equal(by_time$modd, by_position$modd, tolerance = 1e-8)
})

test_that("timestamp-aware lag metrics handle irregular observed data without grid expansion", {
  timestamps <- seq(
    parse_cgm_timestamp("2026-05-05 00:00:00"),
    by = 5 * 60,
    length.out = 576
  )
  keep <- seq_along(timestamps) %% 7 != 0
  timestamps <- timestamps[keep] + rep(c(0, 20, -20), length.out = sum(keep))
  glucose <- 110 + cos(seq_along(timestamps) / 11) * 15 + rep(c(0, 10), length.out = length(timestamps))

  metrics <- optional_lag_metrics_by_time_cpp(as.numeric(timestamps), glucose)

  expect_true(is.finite(metrics$conga_12h))
  expect_true(is.finite(metrics$conga_24h))
  expect_true(is.finite(metrics$modd))
  expect_lt(metrics$tolerance_seconds, 5 * 60)
})

test_that("timestamp-aware lag metrics return NA for sparse data", {
  timestamps <- parse_cgm_timestamp(c(
    "2026-05-05 00:00:00",
    "2026-05-05 00:05:00",
    "2026-05-05 00:10:00"
  ))

  metrics <- optional_lag_metrics_by_time_cpp(as.numeric(timestamps), c(100, 110, 120))

  expect_true(is.na(metrics$conga_12h))
  expect_true(is.na(metrics$conga_24h))
  expect_true(is.na(metrics$modd))
})

test_that("lag adapter reuses current analysis rows without calling regularization", {
  timestamps <- seq(
    parse_cgm_timestamp("2026-05-05 00:00:00"),
    by = 5 * 60,
    length.out = 576
  )
  data <- data.frame(
    id = "A",
    timestamp = timestamps,
    glucose = 120 + sin(seq_along(timestamps) / 12) * 25,
    group = "Control",
    imputed_flag = TRUE,
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    regularize_cgm_series = function(...) {
      stop("regularize_cgm_series should not run for optional Metrics lag enrichment", call. = FALSE)
    }
  )

  metrics <- compute_cgmanalyzer_metrics(data, by = c("id", "group"))

  expect_equal(nrow(metrics), 1)
  expect_equal(metrics$cgmanalyzer_status, "internal_observed_pairs")
  expect_true(is.finite(metrics$conga_12h))
  expect_true(is.finite(metrics$conga_24h))
  expect_true(is.finite(metrics$modd))
})

test_that("iglu adapter computes selected internal fallback metrics", {
  demo <- example_missing_5pct_standardized(fill_missing = TRUE)

  metrics <- compute_iglu_metrics(demo)

  expect_equal(nrow(metrics), 5)
  expect_true(all(c("lbgi", "hbgi", "j_index", "mage", "iglu_status") %in% names(metrics)))
  expect_true(all(metrics$iglu_status %in% c("internal", "insufficient_data")))
  expect_true(all(is.finite(metrics$lbgi)))
  expect_true(all(is.finite(metrics$hbgi)))
  expect_true(all(is.finite(metrics$j_index)))
})

test_that("batched internal iglu adapter stays close to participant-wise iglu outputs when available", {
  skip_if_not_installed("iglu")

  demo <- example_missing_5pct_standardized(fill_missing = TRUE)

  batched <- compute_iglu_metrics(demo, by = "id")
  iglu_data <- to_iglu_data(demo)
  expected_lbgi <- iglu::lbgi(iglu_data)
  expected_hbgi <- iglu::hbgi(iglu_data)
  expected_j <- iglu::j_index(iglu_data)

  expect_equal(batched$lbgi[match(expected_lbgi$id, batched$id)], expected_lbgi$LBGI, tolerance = 2e-3)
  expect_equal(batched$hbgi[match(expected_hbgi$id, batched$id)], expected_hbgi$HBGI, tolerance = 2e-3)
  expect_equal(batched$j_index[match(expected_j$id, batched$id)], expected_j$J_index, tolerance = 1e-6)
})

test_that("internal optional adapters do not emit external package chatter", {
  demo <- example_missing_5pct_standardized(fill_missing = TRUE)

  messages <- character()
  withCallingHandlers(
    compute_metric_adapters(demo),
    message = function(message) {
      messages <<- c(messages, conditionMessage(message))
      invokeRestart("muffleMessage")
    }
  )

  external_messages <- messages[!startsWith(messages, "[CGMA perf]")]
  expect_false(any(grepl(
    "mapped_column|Gap found|CGManalyzer|MSEbyC|iglu",
    external_messages,
    ignore.case = TRUE
  )))
})
