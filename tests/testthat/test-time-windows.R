test_that("time window helpers normalize labels and aliases", {
  expect_equal(time_window_values(), c("full_day", "daytime", "nighttime"))
  expect_equal(normalize_time_window(NULL), "full_day")
  expect_equal(normalize_time_window(""), "full_day")
  expect_equal(normalize_time_window("All"), "full_day")
  expect_equal(normalize_time_window("day"), "daytime")
  expect_equal(normalize_time_window("Nighttime (00:00-05:59)"), "nighttime")
  expect_equal(time_window_label("daytime"), "Daytime (06:00-23:59)")
  expect_equal(unname(time_window_filter_choices()), time_window_values())
})

test_that("time window filtering applies ADA-style boundaries", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 05:59:59",
      "2026-05-05 06:00:00",
      "2026-05-05 23:59:59",
      "2026-05-06 00:00:00",
      "2026-05-06 05:59:59",
      "2026-05-06 06:00:00"
    )),
    glucose = seq_len(6),
    stringsAsFactors = FALSE
  )

  daytime <- filter_time_window_data(data, "daytime")
  nighttime <- filter_time_window_data(data, "nighttime")

  expect_equal(nrow(filter_time_window_data(data, "full_day")), 6)
  expect_equal(daytime$glucose, c(2, 3, 6))
  expect_equal(nighttime$glucose, c(1, 4, 5))
})
