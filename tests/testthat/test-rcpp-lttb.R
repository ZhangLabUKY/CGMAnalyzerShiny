test_that("Rcpp LTTB returns bounded ordered indices and endpoints", {
  x <- seq_len(100)
  y <- sin(x / 10)

  selected <- lttb_indices_cpp(x, y, 10L)

  expect_type(selected, "integer")
  expect_lte(length(selected), 10L)
  expect_equal(selected[[1L]], 1L)
  expect_equal(selected[[length(selected)]], length(x))
  expect_true(all(diff(selected) > 0))
})

test_that("Rcpp LTTB handles small and empty inputs", {
  expect_equal(lttb_indices_cpp(numeric(), numeric(), 10L), integer())
  expect_equal(lttb_indices_cpp(1, 10, 10L), 1L)
  expect_equal(lttb_indices_cpp(1:2, c(10, 20), 10L), 1:2)
  expect_equal(lttb_indices_cpp(1:5, 1:5, 5L), 1:5)
})

test_that("Rcpp LTTB returns endpoints for tiny budgets", {
  selected <- lttb_indices_cpp(seq_len(10), seq_len(10), 2L)

  expect_equal(selected, c(1L, 10L))
})

test_that("Rcpp LTTB errors on mismatched x and y lengths", {
  expect_error(lttb_indices_cpp(1:3, 1:2, 2L), "same length")
})

test_that("DESCRIPTION does not import unavailable lttb package", {
  description <- utils::packageDescription("CGMAnalyzerShiny")
  imports <- if (is.null(description$Imports)) "" else description$Imports

  expect_false(grepl("\\blttb\\b", imports))
  expect_true(grepl("\\bRcpp\\b", imports))
  expect_equal(description$LinkingTo, "Rcpp")
})
