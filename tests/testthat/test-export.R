test_that("export payload helpers produce user-facing data products", {
  data <- data.frame(
    id = c("A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(100, 110, NA),
    units = "mg/dL",
    device = NA_character_,
    group = c("Control", "Control", "Treatment"),
    source_file = c("A.csv", "A.csv", "B.csv"),
    imputed_flag = c(FALSE, FALSE, TRUE),
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )
  thresholds <- default_cgm_thresholds()

  data_export <- prepare_cgm_data_export(data)
  metric_export <- prepare_metrics_display(
    compute_base_core_metrics(data, thresholds = thresholds),
    thresholds = thresholds
  )
  qc_export <- prepare_qc_display(
    compute_qc_summary(data, valid_day_hours = 1),
    data
  )

  expect_type(data_export$timestamp, "character")
  expect_true(all(grepl("^2026-05-0[56]T", data_export$timestamp)))
  expect_true("imputed_flag" %in% names(data_export))

  expect_true(all(
    c("Subject ID", "Period", "Metric", "Value", "Units", "Definition") %in%
      names(metric_export)
  ))
  expect_false("metric_engine" %in% names(metric_export))
  expect_true("Mean glucose" %in% metric_export$Metric)
  expect_true("All" %in% metric_export$Period)

  expect_true(all(
    c("Subject ID", "QC status", "Review notes", "Missing glucose") %in%
      names(qc_export)
  ))
  expect_false(any(grepl("_", names(qc_export), fixed = TRUE)))
})

test_that("fwrite export path preserves user-facing CSV columns", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    units = "mg/dL",
    device = NA_character_,
    source_file = "A.csv",
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  file <- tempfile(fileext = ".csv")

  data.table::fwrite(prepare_cgm_data_export(data), file)
  out <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)

  expect_equal(names(out), names(prepare_cgm_data_export(data)))
  expect_equal(out$timestamp, "2026-05-05T08:00:00")
  expect_false("row.names" %in% names(out))
})

test_that("metrics export includes selected-on-demand cache coverage", {
  data <- data.frame(
    id = c("A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(100, 110, 120),
    group = c("Control", "Control", "Treatment"),
    stringsAsFactors = FALSE
  )
  metrics <- compute_base_core_metrics(data[data$id == "A", , drop = FALSE])

  export <- prepare_metrics_cached_export(metrics, data, thresholds = default_cgm_thresholds())
  status <- export[export$Metric == "Metrics cache status", , drop = FALSE]

  expect_true(nrow(export) > nrow(prepare_metrics_display(metrics, show_subject_id = TRUE)))
  expect_equal(status$Value[status$`Subject ID` == "A"], "Generated")
  expect_equal(status$Value[status$`Subject ID` == "B"], "Not generated")
})

test_that("complexity export includes selected-on-demand cache coverage", {
  data <- data.frame(
    id = c("A", "A", "B", "B"),
    timestamp = parse_cgm_timestamp(rep(c("2026-05-05 08:00:00", "2026-05-05 08:05:00"), 2)),
    glucose = c(100, 110, 120, 130),
    group = rep(c("Control", "Treatment"), each = 2),
    stringsAsFactors = FALSE
  )
  params <- complexity_default_parameters(min_points = 2)
  results <- compute_complexity_quick_metrics(data[data$id == "A", , drop = FALSE], params)

  export <- prepare_complexity_cached_export(
    results,
    empty_complexity_curve_rows(),
    data,
    generated_ids = "A",
    show_subject_id = TRUE
  )
  status <- export[export$Metric == "Complexity cache status", , drop = FALSE]

  expect_equal(status$Value[status$`Subject ID` == "A"], "Generated")
  expect_equal(status$Value[status$`Subject ID` == "B"], "Not generated")
  expect_true(all(c("Output", "Derived scalar", "Derived scalar value") %in% names(export)))
})
