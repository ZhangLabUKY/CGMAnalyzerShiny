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
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  all_metrics <- compute_core_metrics(data)
  metrics <- filter_metrics_by_period(all_metrics, "full_day")

  expect_equal(metrics$readings, 4)
  expect_equal(metrics$mean_glucose, mean(c(60, 100, 150, 220)))
  expect_equal(metrics$tir_percent, 50)
  expect_equal(metrics$tbr_percent, 25)
  expect_equal(metrics$tar_percent, 25)
  expect_true("metric_engine" %in% names(metrics))
  expect_true(all(c("conga_12h", "conga_24h", "modd", "lbgi", "hbgi", "j_index", "mage") %in% names(metrics)))
  expect_false("conga_2h" %in% names(metrics))
  expect_equal(sort(unique(all_metrics$metric_period)), c("daytime", "full_day"))
})

test_that("metric adapters fill-bind uneven optional columns across periods", {
  data <- data.frame(
    id = c("A", "A"),
    timestamp = parse_cgm_timestamp(c("2026-05-05 01:00:00", "2026-05-05 08:00:00")),
    glucose = c(100, 120),
    group = "Control",
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    compute_cgmanalyzer_metrics = function(data, by = "id", ...) {
      out <- unique(data[, by, drop = FALSE])
      out$conga_12h <- 1
      if (nrow(data) > 1L) {
        out$period_only_cgmanalyzer <- 2
      }
      out
    },
    compute_iglu_metrics = function(data, by = "id", ...) {
      out <- unique(data[, by, drop = FALSE])
      out$lbgi <- 3
      if (nrow(data) == 1L) {
        out$period_only_iglu <- 4
      }
      out
    }
  )

  adapters <- compute_metric_adapters(data, by = c("id", "group"), periods = c("full_day", "daytime", "nighttime"))

  expect_equal(sort(unique(adapters$metric_period)), c("daytime", "full_day", "nighttime"))
  expect_true(all(c("period_only_cgmanalyzer", "period_only_iglu") %in% names(adapters)))
  expect_true(all(is.na(adapters$period_only_iglu[adapters$metric_period == "full_day"])))
  expect_true(all(is.na(adapters$period_only_cgmanalyzer[adapters$metric_period != "full_day"])))
})

test_that("period metrics produce full-day daytime and nighttime rows when data exist", {
  data <- data.frame(
    id = rep("A", 4),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 01:00:00",
      "2026-05-05 05:55:00",
      "2026-05-05 06:00:00",
      "2026-05-05 23:55:00"
    )),
    glucose = c(90, 110, 130, 150),
    group = "Control",
    stringsAsFactors = FALSE
  )

  metrics <- compute_base_core_metrics(data)

  expect_equal(unique(metrics$metric_period), time_window_values())
  expect_equal(metrics$readings[metrics$metric_period == "full_day"], 4)
  expect_equal(metrics$readings[metrics$metric_period == "daytime"], 2)
  expect_equal(metrics$readings[metrics$metric_period == "nighttime"], 2)
})

test_that("base metric batches match per-subject metric states", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 4),
    timestamp = parse_cgm_timestamp(rep(c(
      "2026-05-05 01:00:00",
      "2026-05-05 06:00:00",
      "2026-05-05 12:00:00",
      "2026-05-05 23:55:00"
    ), 2)),
    glucose = c(90, 110, 130, 150, 80, 100, 120, 140),
    group = rep(c("Control", "Treatment"), each = 4),
    stringsAsFactors = FALSE
  )
  thresholds <- default_cgm_thresholds()

  batch <- metrics_compute_base_batch(data, c("A", "B"), thresholds)
  per_subject <- lapply(c("A", "B"), function(id) {
    compute_base_metric_state(data[as.character(data$id) == id, , drop = FALSE], thresholds = thresholds)
  })
  names(per_subject) <- c("A", "B")

  for (id in names(per_subject)) {
    expect_equal(batch[[id]]$base_state$status, per_subject[[id]]$status)
    expect_equal(batch[[id]]$base_state$base, per_subject[[id]]$base, tolerance = 1e-8)
    expect_equal(batch[[id]]$base_state$display, per_subject[[id]]$display, tolerance = 1e-8)
  }
})

test_that("metric store tracks selected-on-demand cached IDs", {
  store <- metrics_make_store("key", c("A", "B"), selected = "A")
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    group = "Control",
    stringsAsFactors = FALSE
  )
  base <- compute_base_core_metrics(data)

  expect_false(metrics_store_base_complete(store))
  expect_equal(metrics_store_cached_ids(store), character())
  expect_true(grepl("0 of 2", metrics_progress_text(store), fixed = TRUE))

  metrics_store_set(store, "A", list(
    id = "A",
    status = "base_ready",
    base_state = metric_state(
      "base_ready",
      data = data,
      base = base,
      display = prepare_metrics_display(base)
    ),
    adapters = NULL,
    adapter_status = "idle"
  ))

  expect_equal(metrics_store_cached_ids(store), "A")
  expect_false(metrics_store_base_complete(store))
  expect_equal(metrics_aggregate_entries(store), base)
})

test_that("base metric rows remain available while optional adapters are pending", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    group = "Control",
    stringsAsFactors = FALSE
  )
  base <- compute_base_core_metrics(data)
  store <- metrics_make_store("key", "A", selected = "A")
  metrics_store_set(store, "A", list(
    id = "A",
    status = "base_ready",
    base_state = metric_state(
      "base_ready",
      data = data,
      base = base,
      display = prepare_metrics_display(base)
    ),
    adapters = NULL,
    adapter_status = "pending"
  ))

  raw <- metrics_entry_raw(metrics_store_get(store, "A"))

  expect_equal(raw, base)
  expect_false(any(c("conga_12h", "lbgi") %in% names(raw)))
})

test_that("metric entry updates replace nested data frames without recursive row errors", {
  data <- data.frame(
    id = rep("A", 3),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 01:00:00",
      "2026-05-05 08:00:00",
      "2026-05-05 20:00:00"
    )),
    glucose = c(100, 110, 120),
    group = "Control",
    stringsAsFactors = FALSE
  )
  completed <- metrics_compute_subject(data, "A", default_cgm_thresholds())
  placeholder <- list(
    id = "A",
    status = "running",
    base_state = metrics_calculating_state(
      data,
      "Metrics are calculating for the selected Subject ID."
    ),
    adapter_status = "pending"
  )

  entry <- metrics_entry_replace_fields(placeholder, completed, "A")
  raw <- metrics_entry_raw(entry)

  expect_equal(entry$status, "base_ready")
  expect_true(nrow(entry$base_state$display) > 0)
  expect_setequal(unique(raw$metric_period), time_window_values())
  expect_true(is.data.frame(entry$base_state$base))
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
    source_file = NA_character_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  base <- compute_base_core_metrics(data)
  display <- prepare_metrics_display(base)

  expect_true("mean_glucose" %in% names(base))
  expect_false(any(c("conga_12h", "conga_24h", "conga_2h", "modd", "lbgi", "hbgi", "j_index", "mage") %in% names(base)))
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
  expect_false(any(c("Low blood glucose index", "CONGA, 2 hour", "CONGA, 12 hour", "CONGA, 24 hour") %in% base_display$Metric))
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

test_that("metrics module reports empty analysis data through testServer", {
  empty <- data.frame(
    id = character(),
    timestamp = as.POSIXct(character(), tz = "UTC"),
    glucose = numeric(),
    units = character(),
    device = character(),
    group = character(),
    source_file = character(),
    imputed_flag = logical(),
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    metrics_module_server,
    args = list(
      standardized = shiny::reactive(empty),
      settings = shiny::reactive(create_reproducibility_settings()),
      active_tab = function() "metrics"
    ),
    {
      state <- display_metric_state()
      expect_equal(state$status, "no_analysis_rows")
      expect_equal(state$message, "No CGM rows are available for the selected analysis date range.")
      expect_equal(nrow(metrics()), 0)
      expect_false(should_start_additional_metrics(state))
    }
  )
})

test_that("selected metric computation uses only requested Subject ID", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 2),
    timestamp = parse_cgm_timestamp(rep(c("2026-05-05 08:00:00", "2026-05-05 08:05:00"), 2)),
    glucose = c(100, 110, 200, 210),
    group = rep(c("Control", "Treatment"), each = 2),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    compute_metric_adapters = function(data, by = default_metric_groups(data), ...) {
      expect_equal(unique(as.character(data$id)), "B")
      data.frame(
        id = "B",
        group = "Treatment",
        metric_period = "full_day",
        lbgi = 1,
        stringsAsFactors = FALSE
      )
    }
  )

  entry <- metrics_compute_subject(data, "B", default_cgm_thresholds())

  expect_equal(entry$id, "B")
  expect_equal(entry$status, "base_ready")
  expect_equal(unique(as.character(entry$base_state$base$id)), "B")
  expect_equal(unique(as.character(entry$adapters$id)), "B")
})

test_that("metrics module inactive-tab reactive returns cached-only rows", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    group = "Control",
    stringsAsFactors = FALSE
  )
  base_calls <- 0L
  local_mocked_bindings(
    compute_base_core_metrics = function(...) {
      base_calls <<- base_calls + 1L
      stop("inactive metrics should not compute base rows", call. = FALSE)
    }
  )

  shiny::testServer(
    metrics_module_server,
    args = list(
      standardized = shiny::reactive(data),
      settings = shiny::reactive(create_reproducibility_settings()),
      active_tab = function() "statistics"
    ),
    {
      out <- metrics()
      expect_equal(base_calls, 0L)
      expect_equal(nrow(out), 0L)
    }
  )
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

test_that("metric filters detect usable group values", {
  no_groups <- prepare_metrics_display(compute_base_core_metrics(data.frame(
    id = rep("A", 2),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-05 08:05:00")),
    glucose = c(100, 110),
    group = NA_character_,
    stringsAsFactors = FALSE
  )))
  with_groups <- prepare_metrics_display(compute_base_core_metrics(data.frame(
    id = c("A", "B"),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-05 08:05:00")),
    glucose = c(100, 110),
    group = c("Control", "Treatment"),
    stringsAsFactors = FALSE
  )))

  expect_lt(length(clean_filter_values(no_groups$Group)), 2)
  expect_equal(clean_filter_values(with_groups$Group), c("Control", "Treatment"))
  expect_false("Visit" %in% names(with_groups))
})
