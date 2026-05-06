test_that("data signatures change when important data features change", {
  data <- data.frame(
    id = c("A", "A"),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-05 08:05:00")),
    glucose = c(100, 110),
    source_file = "A.csv",
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  changed <- data
  changed$glucose[[2L]] <- 120

  expect_false(identical(cgm_data_signature(data), cgm_data_signature(changed)))
  expect_false(identical(threshold_signature(default_cgm_thresholds()), threshold_signature(modifyList(default_cgm_thresholds(), list(tir_upper = 190)))))
})

test_that("data.table base metrics match legacy one-group summary", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:10:00",
      "2026-05-05 08:15:00"
    )),
    glucose = c(60, 100, 150, 220),
    group = "Control",
    visit = "Baseline",
    stringsAsFactors = FALSE
  )
  thresholds <- default_cgm_thresholds()

  old <- summarize_one_metric_group(data, thresholds = thresholds, group_columns = c("id", "group", "visit"))
  fast <- compute_base_metrics_dt(data, thresholds = thresholds, by = c("id", "group", "visit"))

  expect_equal(fast[names(old)], old, tolerance = 1e-8)
})

test_that("data.table QC summary matches legacy one-id summary", {
  data <- data.frame(
    id = c("A", "A", "A", "A"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:30:00"
    )),
    glucose = c(100, 110, 500, NA),
    stringsAsFactors = FALSE
  )

  old <- summarize_one_id_qc(data, valid_day_hours = 1, glucose_min = 40, glucose_max = 400)
  fast <- summarize_qc_dt(data, valid_day_hours = 1, glucose_min = 40, glucose_max = 400)

  expect_equal(fast, old, tolerance = 1e-8)
})
