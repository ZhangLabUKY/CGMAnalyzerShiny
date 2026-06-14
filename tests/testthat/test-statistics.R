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

test_that("statistics module does not evaluate metrics while inactive", {
  calls <- 0L

  shiny::testServer(
    stats_module_server,
    args = list(
      metrics = shiny::reactive({
        calls <<- calls + 1L
        stop("inactive statistics tab should not request metrics", call. = FALSE)
      }),
      active_tab = function() "data"
    ),
    {
      session$flushReact()
      expect_equal(calls, 0L)
    }
  )
})

test_that("subject metadata attaches to metric rows without splitting calculations", {
  data <- data.frame(
    id = rep(c("S1", "S2"), each = 2),
    id_source = "file",
    timestamp = as.POSIXct(c(
      "2026-05-05 08:00:00",
      "2026-05-05 09:00:00",
      "2026-05-05 08:00:00",
      "2026-05-05 09:00:00"
    ), tz = "UTC"),
    glucose = c(100, 120, 140, 160),
    group = rep(c("Control", "Treatment"), each = 2),
    sex = rep(c("F", "M"), each = 2),
    age = rep(c("42", "55"), each = 2),
    stringsAsFactors = FALSE
  )

  metrics <- compute_base_core_metrics(data, periods = "full_day")

  expect_equal(nrow(metrics), 2)
  expect_equal(metrics$readings, c(2, 2))
  expect_equal(metrics$sex, c("F", "M"))
  expect_equal(metrics$age, c("42", "55"))
})

test_that("subject metadata that varies within a subject is omitted from metric rows", {
  data <- data.frame(
    id = c("S1", "S1", "S2", "S2"),
    timestamp = as.POSIXct(c(
      "2026-05-05 08:00:00",
      "2026-05-05 09:00:00",
      "2026-05-05 08:00:00",
      "2026-05-05 09:00:00"
    ), tz = "UTC"),
    glucose = c(100, 120, 140, 160),
    sex = c("F", "M", "M", "M"),
    stringsAsFactors = FALSE
  )

  metrics <- compute_base_core_metrics(data, periods = "full_day")

  expect_false("sex" %in% names(metrics))
})

test_that("grouping choices include two-level categorical metadata only", {
  metrics <- synthetic_metric_data()
  metrics$sex <- rep(c("F", "M"), each = 4)
  metrics$custom_arm <- rep(c("A", "B"), each = 4)
  metrics$age <- as.character(seq_len(nrow(metrics)) + 40)
  metrics$site <- rep(c("A", "B", "C", "D"), length.out = nrow(metrics))
  metrics$one_level <- "Only"
  metrics$metric_period <- "full_day"

  choices <- grouping_choices(metrics)

  expect_true("group" %in% choices)
  expect_true("sex" %in% choices)
  expect_true("custom_arm" %in% choices)
  expect_false("age" %in% choices)
  expect_false("site" %in% choices)
  expect_false("one_level" %in% choices)
  expect_false("id" %in% choices)
  expect_false("metric_period" %in% choices)
  expect_false("mean_glucose" %in% choices)
})

test_that("summarize_metric_by_group reports readable descriptive statistics", {
  summary <- summarize_metric_by_group(
    synthetic_metric_data(),
    metric = "mean_glucose",
    grouping = "group"
  )

  expect_equal(summary$Group, c("Control", "Treatment"))
  expect_equal(summary$N, c(4, 4))
  expect_equal(summary$Mean, c(108.75, 144.25))
  expect_true(all(c("Median", "IQR", "Minimum", "Maximum") %in% names(summary)))
})
