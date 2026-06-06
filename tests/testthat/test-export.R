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
