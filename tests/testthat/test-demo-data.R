test_that("bundled example datasets load with expected missingness", {
  examples <- list(
    missing_5pct = list(
      data = load_example_missing_5pct_cgm_data(),
      file = "CGMExmplDat5Pct",
      rows = 1440L,
      missing = 72L,
      readings_per_subject = rep(288L, 5),
      has_sex = TRUE
    ),
    missing_10pct = list(
      data = load_example_missing_10pct_cgm_data(),
      file = "CGMExmplDat10Pct",
      rows = 1440L,
      missing = 144L,
      readings_per_subject = rep(288L, 5),
      has_sex = TRUE
    )
  )

  for (example in examples) {
    expect_true(all(
      c(
        "USUBJID",
        "LBORRES",
        "Time",
        "AGE",
        "hba1c",
        ".source_file",
        ".source_id"
      ) %in%
        names(example$data)
    ))
    expect_equal(nrow(example$data), example$rows)
    expect_equal(length(unique(example$data$USUBJID)), 5)
    expect_equal(unique(example$data$.source_id), example$file)
    expect_equal(sum(is.na(example$data$LBORRES)), example$missing)
    if (example$has_sex) {
      expect_true("SEX" %in% names(example$data))
      expect_setequal(unique(example$data$SEX), c("F", "M"))
    } else {
      expect_false("SEX" %in% names(example$data))
    }
  }
})

test_that("bundled examples standardize with colon timestamps and expected missing glucose", {
  examples <- list(
    list(data = load_example_missing_5pct_cgm_data(), rows = 1440L, missing = 72L, readings_per_subject = rep(288L, 5)),
    list(data = load_example_missing_10pct_cgm_data(), rows = 1440L, missing = 144L, readings_per_subject = rep(288L, 5))
  )

  for (example in examples) {
    standardized <- standardize_cgm_data(
      example$data,
      mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
    )
    qc <- compute_qc_summary(standardized)
    metrics <- compute_core_metrics(standardized)

    expect_equal(nrow(standardized), example$rows)
    expect_equal(length(subject_id_values(standardized)), 5)
    expect_equal(sum(is.na(standardized$timestamp)), 0)
    expect_equal(sum(is.na(standardized$glucose)), example$missing)
    expect_equal(nrow(qc), 5)
    expect_equal(qc$readings, example$readings_per_subject)
    expect_equal(qc$median_interval_minutes, rep(5, 5))
    expect_equal(nrow(metrics), 5)
    expect_true(all(
      c("conga_12h", "conga_24h", "modd", "lbgi", "hbgi", "j_index", "mage") %in%
        names(metrics)
    ))
    expect_false("conga_2h" %in% names(metrics))
  }
})

test_that("missingness examples can prefill SEX as subject metadata", {
  examples <- list(
    load_example_missing_5pct_cgm_data(),
    load_example_missing_10pct_cgm_data()
  )

  for (example in examples) {
    standardized <- standardize_cgm_data(
      example,
      mapping = list(
        id = "USUBJID",
        timestamp = "Time",
        glucose = "LBORRES",
        subject_metadata = prefill_subject_metadata(
          list(upload_mode = "single_file", data = example),
          id_mapping = "USUBJID"
        )
      )
    )

    expect_true("sex" %in% names(standardized))
    expect_setequal(unique(standardized$sex), c("F", "M"))
    expect_false("group" %in% names(standardized))
  }
})
