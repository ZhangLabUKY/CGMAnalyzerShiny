test_that("compute_qc_summary reports interval, duplicates, gaps, and implausible values", {
  data <- data.frame(
    id = c("A", "A", "A", "A"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:30:00"
    )),
    glucose = c(100, 110, 500, NA),
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  qc <- compute_qc_summary(data, valid_day_hours = 1)

  expect_equal(qc$readings, 4)
  expect_equal(qc$duplicate_timestamps, 1)
  expect_equal(qc$missing_glucose, 1)
  expect_equal(qc$implausible_values, 1)
  expect_true(qc$gap_count >= 1)
})
