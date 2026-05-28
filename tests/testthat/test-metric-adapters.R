test_that("to_iglu_data converts standardized data shape", {
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )

  iglu_data <- to_iglu_data(demo)

  expect_named(iglu_data, c("id", "time", "gl"))
  expect_equal(nrow(iglu_data), nrow(demo))
  expect_s3_class(iglu_data$time, "POSIXct")
})

test_that("regularize_cgm_series keeps regular complete example at five-minute interval", {
  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )

  one_id <- demo[demo$id == "11", , drop = FALSE]
  regular <- regularize_cgm_series(one_id)

  expect_equal(nrow(regular), 100)
  expect_equal(attr(regular, "interval_minutes"), 5)
  expect_false(any(is.na(regular$glucose)))
})

test_that("CGManalyzer adapter computes vector-backed metrics", {
  skip_if_not_installed("CGManalyzer")

  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(
      id = "USUBJID",
      timestamp = "Time",
      glucose = "LBORRES"
    )
  )

  metrics <- compute_cgmanalyzer_metrics(demo)

  expect_equal(nrow(metrics), 5)
  expect_true(all(c("conga_12h", "conga_24h", "modd", "cgmanalyzer_status") %in% names(metrics)))
  expect_false("conga_2h" %in% names(metrics))
  expect_true(all(is.finite(metrics$conga_12h) | is.na(metrics$conga_12h)))
  expect_true(all(is.finite(metrics$conga_24h) | is.na(metrics$conga_24h)))
  expect_true(all(is.finite(metrics$modd) | is.na(metrics$modd)))
})

test_that("iglu adapter computes selected fallback metrics", {
  skip_if_not_installed("iglu")

  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(
      id = "USUBJID",
      timestamp = "Time",
      glucose = "LBORRES"
    )
  )

  metrics <- compute_iglu_metrics(demo)

  expect_equal(nrow(metrics), 5)
  expect_true(all(c("lbgi", "hbgi", "j_index", "mage", "iglu_status") %in% names(metrics)))
  expect_true(all(is.finite(metrics$lbgi)))
  expect_true(all(is.finite(metrics$hbgi)))
  expect_true(all(is.finite(metrics$j_index)))
})

test_that("batched iglu adapter matches participant-wise iglu outputs", {
  skip_if_not_installed("iglu")

  demo <- standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(
      id = "USUBJID",
      timestamp = "Time",
      glucose = "LBORRES"
    )
  )

  batched <- compute_iglu_metrics(demo, by = "id")
  iglu_data <- to_iglu_data(demo)
  expected_lbgi <- iglu::lbgi(iglu_data)
  expected_hbgi <- iglu::hbgi(iglu_data)
  expected_j <- iglu::j_index(iglu_data)

  expect_equal(batched$lbgi[match(expected_lbgi$id, batched$id)], expected_lbgi$LBGI, tolerance = 1e-8)
  expect_equal(batched$hbgi[match(expected_hbgi$id, batched$id)], expected_hbgi$HBGI, tolerance = 1e-8)
  expect_equal(batched$j_index[match(expected_j$id, batched$id)], expected_j$J_index, tolerance = 1e-8)
})
