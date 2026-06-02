test_that("prepare_metrics_display hides internal adapter columns", {
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
  metrics <- compute_core_metrics(demo)
  display <- prepare_metrics_display(metrics)

  expect_false(any(c("metric_engine", "cgmanalyzer_status", "iglu_status") %in% names(display)))
  expect_false(any(grepl("CGManalyzer|iglu|engine|status", names(display), ignore.case = TRUE)))
  expect_false(any(grepl("CGManalyzer|iglu|engine|status", unlist(display), ignore.case = TRUE)))
})

test_that("prepare_metrics_display returns readable long-format rows", {
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
  display <- prepare_metrics_display(compute_core_metrics(demo))

  expect_true(all(c("Subject ID", "Category", "Metric", "Definition", "Value", "Units") %in% names(display)))
  expect_false("Group" %in% names(display))
  expect_false("Visit" %in% names(display))
  expect_true(nrow(display) > 20)
  expect_true(all(c("Data Coverage", "Central Tendency", "Variability", "Time in Range", "Detailed Range Bands", "Risk", "Excursions") %in% display$Category))
  expect_true("Mean glucose" %in% display$Metric)
  expect_true("Coefficient of variation" %in% display$Metric)
  expect_false(any(is.na(display$Definition) | !nzchar(display$Definition)))
})

test_that("metric display avoids wide debug columns on demo metrics", {
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
  display <- prepare_metrics_display(compute_core_metrics(demo))

  expect_lte(ncol(display), 8)
  expect_equal(sort(unique(display[["Subject ID"]])), sort(c("11", "18", "80", "115", "217")))
})

test_that("core range metrics and detailed bands use separate categories", {
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
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

test_that("CONGA display catalog uses 12 and 24 hour metrics", {
  catalog <- metric_display_catalog()
  conga <- catalog[grepl("^conga_", catalog$raw_name), , drop = FALSE]

  expect_equal(conga$raw_name, c("conga_12h", "conga_24h"))
  expect_equal(conga$metric, c("CONGA, 12 hour", "CONGA, 24 hour"))
  expect_true(all(grepl("12 hours|24 hours", conga$definition)))
  expect_false("conga_2h" %in% catalog$raw_name)
  expect_false("CONGA, 2 hour" %in% catalog$metric)
})

test_that("range metric labels and definitions use current thresholds", {
  thresholds <- list(
    tir_lower = 80,
    tir_upper = 160,
    tbr_level2 = 55,
    tar_level2 = 240
  )
  catalog <- metric_display_catalog(thresholds = thresholds)
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
  display <- prepare_metrics_display(compute_base_core_metrics(demo, thresholds = thresholds), thresholds = thresholds)

  expect_true("Time in range (80-160 mg/dL)" %in% catalog$metric)
  expect_true("Time below range (<80 mg/dL)" %in% display$Metric)
  expect_true("Level 1 above range (161-240 mg/dL)" %in% display$Metric)
  expect_true(any(grepl("between 80 and 160 mg/dL", display$Definition, fixed = TRUE)))
  expect_equal(
    key_metric_names(thresholds)[3:5],
    c(
      "Time in range (80-160 mg/dL)",
      "Time below range (<80 mg/dL)",
      "Time above range (>160 mg/dL)"
    )
  )
})

test_that("metric display is ordered by category, subject, and catalog order", {
  data <- data.frame(
    id = rep(c("B", "A"), each = 4),
    timestamp = rep(parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:10:00",
      "2026-05-05 08:15:00"
    )), times = 2),
    glucose = c(60, 100, 150, 220, 70, 110, 140, 180),
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )
  display <- prepare_metrics_display(compute_base_core_metrics(data))

  expect_equal(unique(display$Category)[1:4], metric_category_order()[1:4])
  coverage <- display[display$Category == "Data Coverage", , drop = FALSE]
  expect_equal(coverage[["Subject ID"]], c("A", "B"))
})

test_that("metrics table options group by Category", {
  display <- empty_metrics_display()
  options <- metrics_table_options(display)

  expect_true("rowGroup" %in% names(options))
  expect_equal(options$rowGroup$dataSrc, match("Category", names(display)) - 1L)
  expect_equal(options$columnDefs[[1]]$targets, match("Category", names(display)) - 1L)
  expect_false(options$columnDefs[[1]]$visible)
  expect_equal(options$pageLength, 15)
})

test_that("optional metric note is user-facing and hides internal wording", {
  note <- optional_metric_note_text("failed")

  expect_true(nzchar(note))
  expect_equal(optional_metric_note_text("complete"), "")
  expect_false(grepl("CGManalyzer|iglu|engine|adapter|package", note, ignore.case = TRUE))
})

test_that("category filter can be excluded for stable summary cards", {
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
  display <- prepare_metrics_display(compute_core_metrics(demo))

  table_filtered <- filter_metrics_display(display, category = "Detailed Range Bands", include_category = TRUE)
  card_filtered <- filter_metrics_display(display, category = "Detailed Range Bands", include_category = FALSE)

  expect_true(all(table_filtered$Category == "Detailed Range Bands"))
  expect_true("Mean glucose" %in% card_filtered$Metric)
  expect_true("Glucose management indicator" %in% card_filtered$Metric)
  expect_false("Mean glucose" %in% table_filtered$Metric)
})

test_that("metric summary cards use shared Label and Value structure", {
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
  display <- prepare_metrics_display(compute_base_core_metrics(demo))
  cards <- metric_summary_cards(display)

  expect_named(cards, c("Label", "Value"))
  expect_equal(cards$Label, key_metric_names())
  expect_true(any(grepl("mg/dL", cards$Value, fixed = TRUE)))
  expect_true(any(grepl("%", cards$Value, fixed = TRUE)))
})

test_that("All filter sentinel returns all metric display rows", {
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
  display <- prepare_metrics_display(compute_core_metrics(demo))

  filtered <- filter_metrics_display(
    display,
    participant = all_filter_value(),
    group = all_filter_value(),
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
  metrics <- compute_base_core_metrics(standardized)
  display <- prepare_metrics_display(metrics)
  forced <- prepare_metrics_display(metrics, show_subject_id = TRUE)

  expect_false("Subject ID" %in% names(display))
  expect_equal(filter_metrics_display(display, participant = "one_subject"), display)
  expect_true("Subject ID" %in% names(forced))
  expect_equal(unique(forced[["Subject ID"]]), "one_subject")
})
