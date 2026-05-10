test_that("background worker count stays in the local 2-4 worker range", {
  expect_equal(background_worker_count(1), 2)
  expect_equal(background_worker_count(3), 2)
  expect_equal(background_worker_count(5), 4)
  expect_equal(background_worker_count(99), 4)
  expect_equal(background_worker_count(NA), 2)
})

test_that("background worker setup and cleanup call the expected future plans", {
  calls <- list()
  fake_plan <- function(strategy, ...) {
    calls[[length(calls) + 1L]] <<- list(
      strategy = deparse(substitute(strategy)),
      args = list(...)
    )
    invisible(NULL)
  }

  token <- configure_background_workers(workers = 3, plan_fn = fake_plan)
  cleaned <- cleanup_background_workers(plan_fn = fake_plan)
  cleaned_again <- cleanup_background_workers(plan_fn = fake_plan)

  expect_true(is.numeric(token))
  expect_true(cleaned)
  expect_true(cleaned_again)
  expect_equal(calls[[1L]]$strategy, "future::multisession")
  expect_equal(calls[[1L]]$args$workers, 3)
  expect_equal(calls[[2L]]$strategy, "future::sequential")
  expect_equal(calls[[3L]]$strategy, "future::sequential")
})

test_that("scheduled cleanup uses plain generation tokens", {
  delayed_callback <- NULL
  delayed_delay <- NULL
  cleanup_calls <- 0L
  fake_scheduler <- function(callback, delay) {
    delayed_callback <<- callback
    delayed_delay <<- delay
    invisible(NULL)
  }
  fake_plan <- function(strategy, ...) invisible(NULL)
  fake_cleanup <- function() {
    cleanup_calls <<- cleanup_calls + 1L
    TRUE
  }

  old_token <- configure_background_workers(workers = 2, plan_fn = fake_plan)
  expect_true(schedule_background_worker_cleanup(
    token = old_token,
    delay = 1,
    scheduler = fake_scheduler,
    cleanup_fn = fake_cleanup
  ))
  expect_equal(delayed_delay, 1)

  invisible(configure_background_workers(workers = 2, plan_fn = fake_plan))
  delayed_callback()
  expect_equal(cleanup_calls, 0)

  current_token <- background_worker_state$generation
  expect_true(schedule_background_worker_cleanup(
    token = current_token,
    delay = 1,
    scheduler = fake_scheduler,
    cleanup_fn = fake_cleanup
  ))
  delayed_callback()
  expect_equal(cleanup_calls, 1)
})

test_that("app server registers session-ended worker cleanup", {
  app_server_code <- paste(readLines(testthat::test_path("../../R/app_server.R"), warn = FALSE), collapse = "\n")

  expect_true(grepl("onSessionEnded", app_server_code, fixed = TRUE))
  expect_true(grepl("cleanup_background_workers", app_server_code, fixed = TRUE))
  expect_false(grepl("future::plan(future::multisession", app_server_code, fixed = TRUE))
})

test_that("metrics module configures and schedules background workers around adapters", {
  metrics_code <- paste(readLines(testthat::test_path("../../R/module_metrics.R"), warn = FALSE), collapse = "\n")

  expect_true(grepl("configure_background_workers", metrics_code, fixed = TRUE))
  expect_true(grepl("schedule_background_worker_cleanup", metrics_code, fixed = TRUE))
  expect_true(grepl("promises::future_promise", metrics_code, fixed = TRUE))
})
