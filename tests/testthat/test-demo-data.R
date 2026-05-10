test_that("bundled demo data loads and standardizes", {
  demo <- load_demo_cgm_data()

  expect_named(demo, c("id", "time", "glucose", "group", "visit", "device", ".source_file", ".source_id"))
  expect_equal(nrow(demo), 144)
  expect_equal(sort(unique(demo$id)), c("CGM001", "CGM002"))
  expect_equal(unique(demo$.source_id), "demo_cgm")

  standardized <- standardize_cgm_data(
    demo,
    mapping = list(
      id = "id",
      timestamp = "time",
      glucose = "glucose",
      group = "group",
      visit = "visit",
      device = "device"
    )
  )

  qc <- compute_qc_summary(standardized)
  metrics <- compute_core_metrics(standardized)

  expect_equal(nrow(qc), 2)
  expect_equal(qc$readings, c(72, 72))
  expect_equal(qc$median_interval_minutes, c(60, 60))
  expect_equal(nrow(metrics), 2)
  expect_true(all(c("conga_2h", "modd", "lbgi", "hbgi", "j_index", "mage") %in% names(metrics)))
})
