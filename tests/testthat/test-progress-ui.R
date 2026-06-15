test_that("progress wrapper returns expression value without a Shiny session", {
  value <- 1L
  result <- cgm_with_progress(
    "Testing progress",
    {
      value <- value + 1L
      value
    },
    session = NULL
  )

  expect_equal(result, 2L)
})

test_that("progress wrapper propagates expression errors", {
  expect_error(
    cgm_with_progress(
      "Testing progress",
      stop("progress failure", call. = FALSE),
      session = NULL
    ),
    "progress failure"
  )
})
