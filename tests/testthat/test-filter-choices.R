test_that("filter choices always include All first", {
  choices <- filter_select_choices(c("B", "A"))

  expect_equal(names(choices)[[1L]], "All")
  expect_equal(unname(choices)[[1L]], all_filter_value())
  expect_true(nzchar(unname(choices)[[1L]]))
  expect_equal(unname(choices[-1L]), c("B", "A"))
})

test_that("filter choices clean blank, NA, and duplicate concrete values", {
  choices <- filter_select_choices(c("A", NA, "", " A ", "B", "B"))

  expect_equal(unname(choices), c(all_filter_value(), "A", "B"))
  expect_equal(names(choices), c("All", "A", "B"))
})

test_that("filter selection is preserved only when still valid", {
  choices <- filter_select_choices(c("A", "B"))

  expect_equal(preserve_filter_selection("B", choices), "B")
  expect_equal(preserve_filter_selection("C", choices), all_filter_value())
  expect_equal(preserve_filter_selection(NULL, choices), all_filter_value())
})

test_that("All filter sentinel normalizes to no filter", {
  expect_equal(normalize_filter_value(all_filter_value()), "")
  expect_equal(normalize_filter_value("A"), "A")
  expect_equal(normalize_filter_value(NULL), "")
})
