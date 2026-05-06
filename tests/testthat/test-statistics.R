synthetic_metric_data <- function() {
  data.frame(
    id = paste0("S", 1:8),
    group = rep(c("Control", "Treatment"), each = 4),
    visit = "Baseline",
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

test_that("run_metric_stat_test reports insufficient data for current demo", {
  demo <- standardize_cgm_data(
    load_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  metrics <- compute_core_metrics(demo)
  result <- run_metric_stat_test(metrics, metric = "mean_glucose", grouping = "group", test_type = "welch_t")

  expect_true(is.na(result$`P-value`))
  expect_match(result$Note, "At least two observations per group")
})
