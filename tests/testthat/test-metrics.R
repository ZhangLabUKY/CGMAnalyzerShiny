test_that("compute_core_metrics calculates core glucose summaries", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:10:00",
      "2026-05-05 08:15:00"
    )),
    glucose = c(60, 100, 150, 220),
    units = "mg/dL",
    device = NA_character_,
    group = "Control",
    visit = "Baseline",
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  metrics <- compute_core_metrics(data)

  expect_equal(metrics$readings, 4)
  expect_equal(metrics$mean_glucose, mean(c(60, 100, 150, 220)))
  expect_equal(metrics$tir_percent, 50)
  expect_equal(metrics$tbr_percent, 25)
  expect_equal(metrics$tar_percent, 25)
  expect_true("metric_engine" %in% names(metrics))
  expect_true(all(c("conga_2h", "modd", "lbgi", "hbgi", "j_index", "mage") %in% names(metrics)))
})

test_that("base core metrics are available without adapter columns", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:10:00",
      "2026-05-05 08:15:00"
    )),
    glucose = c(60, 100, 150, 220),
    units = "mg/dL",
    device = NA_character_,
    group = "Control",
    visit = "Baseline",
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  base <- compute_base_core_metrics(data)
  display <- prepare_metrics_display(base)

  expect_true("mean_glucose" %in% names(base))
  expect_false(any(c("conga_2h", "modd", "lbgi", "hbgi", "j_index", "mage") %in% names(base)))
  expect_true("Mean glucose" %in% display$Metric)
  expect_false("Low blood glucose index" %in% display$Metric)
})

test_that("metric filters detect usable group and visit values", {
  no_groups <- prepare_metrics_display(compute_base_core_metrics(data.frame(
    id = rep("A", 2),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-05 08:05:00")),
    glucose = c(100, 110),
    group = NA_character_,
    visit = NA_character_,
    stringsAsFactors = FALSE
  )))
  with_groups <- prepare_metrics_display(compute_base_core_metrics(data.frame(
    id = c("A", "B"),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-05 08:05:00")),
    glucose = c(100, 110),
    group = c("Control", "Treatment"),
    visit = c("Baseline", "Baseline"),
    stringsAsFactors = FALSE
  )))

  expect_lt(length(clean_filter_values(no_groups$Group)), 2)
  expect_equal(clean_filter_values(with_groups$Group), c("Control", "Treatment"))
  expect_lt(length(clean_filter_values(with_groups$Visit)), 2)
})
