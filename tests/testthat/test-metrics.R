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

test_that("metric filter choices include selectable All and preserve selections", {
  display <- data.frame(
    `Subject ID` = c("A", "B", "", NA),
    Category = c("Central Tendency", "Risk", "Risk", "Unknown"),
    Metric = "Mean glucose",
    Value = 100,
    Units = "mg/dL",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  participant_choices <- metric_participant_filter_choices(display)
  category_choices <- metric_category_filter_choices(display)

  expect_equal(unname(participant_choices), c(all_filter_value(), "A", "B"))
  expect_equal(names(participant_choices)[[1L]], "All")
  expect_equal(unname(category_choices), c(all_filter_value(), "Central Tendency", "Risk"))
  expect_equal(preserve_filter_selection("A", participant_choices), "A")
  expect_equal(preserve_filter_selection("Missing", participant_choices), all_filter_value())
})

test_that("metric display supports base-only and failed-additional metric states", {
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
    group = NA_character_,
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  base <- compute_base_core_metrics(data)
  base_display <- prepare_metrics_display(base)
  failed_additional <- merge_core_metric_outputs(base, NULL, by = default_metric_groups(base))
  failed_display <- prepare_metrics_display(failed_additional)

  expect_true(nrow(base_display) > 0)
  expect_true("Mean glucose" %in% base_display$Metric)
  expect_false(any(c("Low blood glucose index", "CONGA, 2 hour") %in% base_display$Metric))
  expect_true(nrow(failed_display) > 0)
  expect_true("Mean glucose" %in% failed_display$Metric)
})

test_that("base metric state reports ready, empty range, and mapping states", {
  valid <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    units = "mg/dL",
    device = NA_character_,
    group = NA_character_,
    visit = NA_character_,
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  empty <- valid[0, , drop = FALSE]
  missing_columns <- data.frame(time = valid$timestamp, value = valid$glucose)

  ready <- compute_base_metric_state(valid)
  no_rows <- compute_base_metric_state(empty)
  needs_mapping <- compute_base_metric_state(missing_columns)

  expect_equal(ready$status, "base_ready")
  expect_true(nrow(ready$display) > 0)
  expect_true(should_start_additional_metrics(ready))
  expect_equal(no_rows$status, "no_analysis_rows")
  expect_equal(no_rows$message, "No CGM rows are available for the selected analysis date range.")
  expect_false(should_start_additional_metrics(no_rows))
  expect_equal(needs_mapping$status, "needs_mapping")
  expect_equal(needs_mapping$message, "Select timestamp and glucose columns to calculate metrics.")
  expect_false(should_start_additional_metrics(needs_mapping))
})

test_that("base metric state preserves calculation errors without exposing internals", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    stringsAsFactors = FALSE
  )

  state <- compute_base_metric_state(
    data,
    thresholds = list(tir_lower = list(70), tir_upper = 180, tbr_level2 = 54, tar_level2 = 250)
  )
  status_text <- metric_state("base_error", error = "CGManalyzer iglu adapter failed")

  expect_equal(state$status, "base_error")
  expect_true(nzchar(state$error))
  expect_equal(
    state$message,
    "Metrics could not be calculated from the current analysis data. Check timestamp and glucose mappings."
  )
  expect_false(grepl("CGManalyzer|iglu|engine|adapter", status_text$message, ignore.case = TRUE))
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
