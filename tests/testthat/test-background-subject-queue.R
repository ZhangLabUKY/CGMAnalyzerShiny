test_that("subject background queue prioritizes selected subjects and batches", {
  queue <- subject_queue_new(paste0("S", 1:6), selected = "S4", batch_size = 2)

  expect_equal(queue$pending, c("S4", "S1", "S2", "S3", "S5", "S6"))
  expect_equal(subject_queue_next_batch(queue), c("S4", "S1"))

  queue <- subject_queue_mark_running(queue)
  expect_equal(subject_queue_next_batch(queue), character())

  queue <- subject_queue_mark_finished(queue, c("S4", "S1"))
  expect_equal(queue$complete, c("S1", "S4"))
  expect_equal(subject_queue_next_batch(queue), c("S2", "S3"))
})

test_that("subject background queue re-prioritizes a newly selected pending subject", {
  queue <- subject_queue_new(paste0("S", 1:5), selected = "S1", batch_size = 2)
  queue <- subject_queue_mark_finished(queue, c("S1", "S2"))
  queue <- subject_queue_reprioritize(queue, "S5")

  expect_equal(subject_queue_next_batch(queue), c("S5", "S3"))
  expect_equal(queue$pending, c("S5", "S3", "S4"))
})

test_that("subject background queue progress tracks completed and failed subjects", {
  queue <- subject_queue_new(paste0("S", 1:4), batch_size = 2)
  queue <- subject_queue_mark_finished(queue, c("S1", "S2"), failed = "S2")

  progress <- subject_queue_progress(queue, paste0("S", 1:4))

  expect_equal(progress$done, 2L)
  expect_equal(progress$total, 4L)
  expect_false(progress$complete)
  expect_match(subject_queue_progress_text("Metrics", queue, paste0("S", 1:4)), "Metrics cached for 2 of 4 Subject IDs")
})

test_that("metrics and complexity use different default background batch sizes", {
  expect_equal(subject_background_batch_size("metrics"), 20L)
  expect_equal(subject_background_batch_size("complexity"), 5L)
})
