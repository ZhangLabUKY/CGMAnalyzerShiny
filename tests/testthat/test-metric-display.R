test_that("prepare_metrics_display hides internal adapter columns", {
  demo <- standardize_cgm_data(
    load_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  metrics <- compute_core_metrics(demo)
  display <- prepare_metrics_display(metrics)

  expect_false(any(c("metric_engine", "cgmanalyzer_status", "iglu_status") %in% names(display)))
  expect_false(any(grepl("CGManalyzer|iglu|engine|status", names(display), ignore.case = TRUE)))
  expect_false(any(grepl("CGManalyzer|iglu|engine|status", unlist(display), ignore.case = TRUE)))
})

test_that("prepare_metrics_display returns readable long-format rows", {
  demo <- standardize_cgm_data(
    load_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  display <- prepare_metrics_display(compute_core_metrics(demo))

  expect_true(all(c("Subject ID", "Group", "Visit", "Category", "Metric", "Value", "Units") %in% names(display)))
  expect_true(nrow(display) > 20)
  expect_true(all(c("Data Coverage", "Central Tendency", "Variability", "Time in Range", "Detailed Range Bands", "Risk", "Excursions") %in% display$Category))
  expect_true("Mean glucose" %in% display$Metric)
  expect_true("Coefficient of variation" %in% display$Metric)
})

test_that("metric display avoids wide debug columns on demo metrics", {
  demo <- standardize_cgm_data(
    load_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  display <- prepare_metrics_display(compute_core_metrics(demo))

  expect_lte(ncol(display), 7)
  expect_equal(sort(unique(display[["Subject ID"]])), c("CGM001", "CGM002"))
})

test_that("core range metrics and detailed bands use separate categories", {
  demo <- standardize_cgm_data(
    load_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  display <- prepare_metrics_display(compute_core_metrics(demo))

  core_range <- unique(display$Metric[display$Category == "Time in Range"])
  detailed_range <- unique(display$Metric[display$Category == "Detailed Range Bands"])

  expect_equal(
    core_range,
    c("Time in range (70-180 mg/dL)", "Time below range (<70 mg/dL)", "Time above range (>180 mg/dL)")
  )
  expect_equal(
    detailed_range,
    c(
      "Level 2 below range (<54 mg/dL)",
      "Level 1 below range (54-69 mg/dL)",
      "Level 1 above range (181-250 mg/dL)",
      "Level 2 above range (>250 mg/dL)"
    )
  )
})

test_that("range metric labels include threshold context", {
  catalog <- metric_display_catalog()
  labels <- catalog$metric[catalog$raw_name %in% c(
    "tir_percent",
    "tbr_percent",
    "tar_percent",
    "tbr_level2_percent",
    "tbr_level1_percent",
    "tar_level1_percent",
    "tar_level2_percent"
  )]

  expect_true(all(grepl("mg/dL", labels)))
  expect_true(any(grepl("70-180", labels, fixed = TRUE)))
  expect_true(any(grepl("<54", labels, fixed = TRUE)))
  expect_true(any(grepl(">250", labels, fixed = TRUE)))
})

test_that("category filter can be excluded for stable summary cards", {
  demo <- standardize_cgm_data(
    load_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  display <- prepare_metrics_display(compute_core_metrics(demo))

  table_filtered <- filter_metrics_display(display, category = "Detailed Range Bands", include_category = TRUE)
  card_filtered <- filter_metrics_display(display, category = "Detailed Range Bands", include_category = FALSE)

  expect_true(all(table_filtered$Category == "Detailed Range Bands"))
  expect_true("Mean glucose" %in% card_filtered$Metric)
  expect_true("Glucose management indicator" %in% card_filtered$Metric)
  expect_false("Mean glucose" %in% table_filtered$Metric)
})

test_that("All filter sentinel returns all metric display rows", {
  demo <- standardize_cgm_data(
    load_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  display <- prepare_metrics_display(compute_core_metrics(demo))

  filtered <- filter_metrics_display(
    display,
    participant = all_filter_value(),
    group = all_filter_value(),
    visit = all_filter_value(),
    category = all_filter_value()
  )

  expect_equal(filtered, display)
})

test_that("metric display hides Subject ID for one filename-derived subject", {
  raw <- data.frame(
    time = c("2026-05-05 08:00:00", "2026-05-05 08:05:00"),
    glucose = c(100, 110),
    .source_id = "one_subject",
    stringsAsFactors = FALSE
  )
  standardized <- standardize_cgm_data(
    raw,
    mapping = list(id = "", timestamp = "time", glucose = "glucose")
  )
  display <- prepare_metrics_display(compute_base_core_metrics(standardized))

  expect_false("Subject ID" %in% names(display))
  expect_equal(filter_metrics_display(display, participant = "one_subject"), display)
})
