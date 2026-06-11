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

test_that("performance timing is disabled by default and can write logs when enabled", {
  old_enabled <- getOption("CGMA.performance_log")
  old_file <- getOption("CGMA.performance_log_file")
  on.exit({
    options(CGMA.performance_log = old_enabled)
    options(CGMA.performance_log_file = old_file)
  }, add = TRUE)

  options(CGMA.performance_log = FALSE, CGMA.performance_log_file = FALSE)
  expect_false(cgm_performance_log_enabled())
  expect_equal(cgm_timed("disabled_test", 1 + 1), 2)

  file <- tempfile(fileext = ".csv")
  options(CGMA.performance_log = TRUE, CGMA.performance_log_file = file)
  expect_true(cgm_performance_log_enabled())
  expect_message(
    value <- cgm_timed("enabled_test", data.frame(x = 1:3)),
    "\\[CGMA perf\\] enabled_test"
  )
  expect_equal(nrow(value), 3)
  log <- utils::read.csv(file, stringsAsFactors = FALSE)
  expect_equal(log$label, "enabled_test")
  expect_equal(log$rows, 3L)
  expect_equal(log$status, "ok")

  expect_message(
    expect_error(cgm_timed("error_test", stop("boom", call. = FALSE)), "boom"),
    "\\[CGMA perf\\] error_test"
  )
  log <- utils::read.csv(file, stringsAsFactors = FALSE)
  error_row <- log[log$label == "error_test", , drop = FALSE]
  expect_equal(error_row$status, "error")
  expect_true(grepl("error_message=boom", error_row$context, fixed = TRUE))
})

test_that("non-CGMA diagnostic messages can be suppressed without hiding performance logs", {
  expect_silent(cgm_suppress_non_cgma_messages(message("external gap diagnostic")))
  expect_message(
    cgm_suppress_non_cgma_messages(message("[CGMA perf] kept")),
    "\\[CGMA perf\\] kept"
  )
})

test_that("background workers reuse an existing matching plan", {
  old_generation <- background_worker_state$generation
  old_configured <- background_worker_state$configured
  old_workers <- background_worker_state$workers
  on.exit({
    background_worker_state$generation <- old_generation
    background_worker_state$configured <- old_configured
    background_worker_state$workers <- old_workers
  }, add = TRUE)

  background_worker_state$generation <- 0L
  background_worker_state$configured <- FALSE
  background_worker_state$workers <- NULL
  calls <- 0L
  fake_plan <- function(...) {
    calls <<- calls + 1L
  }

  first <- configure_background_workers(workers = 2L, plan_fn = fake_plan)
  second <- configure_background_workers(workers = 2L, plan_fn = fake_plan)

  expect_gt(second, first)
  expect_equal(calls, 1L)
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
    stringsAsFactors = FALSE
  )
  thresholds <- default_cgm_thresholds()

  old <- summarize_one_metric_group(data, thresholds = thresholds, group_columns = c("id", "group"))
  fast <- compute_base_metrics_dt(data, thresholds = thresholds, by = c("id", "group"))

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
