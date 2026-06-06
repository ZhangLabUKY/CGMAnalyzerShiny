synthetic_metric_data <- function() {
  data.frame(
    id = paste0("S", 1:8),
    group = rep(c("Control", "Treatment"), each = 4),
    mean_glucose = c(105, 110, 112, 108, 140, 145, 150, 142),
    tir_percent = c(92, 90, 88, 91, 75, 72, 70, 74),
    stringsAsFactors = FALSE
  )
}

test_that("run_metric_stat_test runs Welch t-test on participant-level metrics", {
  result <- run_metric_stat_test(
    synthetic_metric_data(),
    metric = "mean_glucose",
    grouping = "group",
    test_type = "welch_t"
  )

  expect_equal(result$Metric, "Mean glucose")
  expect_equal(result$Test, "Welch t-test")
  expect_equal(result$Groups, "Control vs Treatment")
  expect_equal(result$N, "4 / 4")
  expect_true(is.finite(result$Statistic))
  expect_true(is.finite(result$`P-value`))
  expect_equal(result$Note, "")
})

test_that("run_metric_stat_test runs Wilcoxon rank-sum on participant-level metrics", {
  result <- run_metric_stat_test(
    synthetic_metric_data(),
    metric = "tir_percent",
    grouping = "group",
    test_type = "wilcoxon"
  )

  expect_equal(result$Metric, "Time in range (70-180 mg/dL)")
  expect_equal(result$Test, "Wilcoxon rank-sum")
  expect_true(is.finite(result$Statistic))
  expect_true(is.finite(result$`P-value`))
})

test_that("run_metric_stat_test reports unavailable grouping for current bundled example", {
  demo <- example_missing_5pct_standardized()
  metrics <- compute_core_metrics(demo)
  result <- run_metric_stat_test(metrics, metric = "mean_glucose", grouping = "group", test_type = "welch_t")

  expect_true(is.na(result$`P-value`))
  expect_match(result$Note, "not available")
})

test_that("run_metric_stat_test filters period rows before testing", {
  full <- transform(synthetic_metric_data(), metric_period = "full_day")
  day <- transform(synthetic_metric_data(), metric_period = "daytime", mean_glucose = mean_glucose + 20)
  metrics <- rbind(full, day)

  default_result <- run_metric_stat_test(metrics, metric = "mean_glucose", grouping = "group", test_type = "welch_t")
  daytime_result <- run_metric_stat_test(metrics, metric = "mean_glucose", grouping = "group", test_type = "welch_t", period = "daytime")

  expect_equal(default_result$N, "4 / 4")
  expect_equal(daytime_result$N, "4 / 4")
  expect_true(is.finite(default_result$`P-value`))
  expect_true(is.finite(daytime_result$`P-value`))
})
