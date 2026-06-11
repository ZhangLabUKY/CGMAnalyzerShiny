test_that("subject filters default to the first sorted Subject ID with All explicit", {
  ids <- c("S03", "S01", "S02")

  choices <- subject_filter_choices(ids)

  expect_equal(unname(choices), c("S01", "S02", "S03", all_filter_value()))
  expect_equal(default_subject_selection(ids), "S01")
  expect_equal(preserve_subject_filter_selection(NULL, choices, ids), "S01")
  expect_equal(preserve_subject_filter_selection(all_filter_value(), choices, ids), all_filter_value())
})

test_that("subject filtering supports individual IDs and explicit All only", {
  data <- data.frame(
    id = rep(paste0("S", 1:3), each = 2),
    value = seq_len(6),
    stringsAsFactors = FALSE
  )

  filtered <- filter_data_by_subject_selection(data, "S2")
  all_rows <- filter_data_by_subject_selection(data, all_filter_value())

  expect_equal(unique(filtered$id), "S2")
  expect_equal(nrow(filtered), 2)
  expect_equal(all_rows, data)
})
