test_that("apply_imputed_glucose preserves rows and flags only originally missing values", {
  data <- standardize_cgm_data(
    load_missingness_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  original_glucose <- data$glucose
  missing_rows <- which(is.na(data$glucose))
  fake_values <- ifelse(is.na(data$glucose), 123, data$glucose + 1000)
  fake_result <- list(
    imputed_data = list(
      mice_only = data.frame(
        .RowID = seq_len(nrow(data)),
        .Missing = is.na(data$glucose),
        ImputedValue = fake_values
      )
    )
  )

  imputed <- apply_imputed_glucose(data, fake_result)

  expect_equal(nrow(imputed), nrow(data))
  expect_equal(imputed$glucose[-missing_rows], original_glucose[-missing_rows])
  expect_equal(imputed$glucose[missing_rows], rep(123, length(missing_rows)))
  expect_equal(which(imputed$imputed_flag), missing_rows)
})

test_that("CGMissingDataR adapter is guarded until GitHub/current function is installed", {
  if (!cgmissingdata_available()) {
    expect_error(
      run_cgmissingdata_imputation(data.frame(id = "A", timestamp = Sys.time(), glucose = NA_real_)),
      "run_missing_glucose_imputation"
    )
  } else {
    skip("CGMissingDataR imputation function is available; guarded-unavailable path not applicable.")
  }
})

test_that("CGMissingDataR input uses canonical ISO timestamps", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-06 11:30:00"),
    glucose = NA_real_,
    stringsAsFactors = FALSE
  )

  imputation_input <- prepare_cgmissingdata_input(data)

  expect_equal(imputation_input$time, "2026-05-06T11:30:00")
})

test_that("CGMissingDataR adapter can impute missingness demo when available", {
  testthat::skip_if_not(cgmissingdata_available())

  data <- standardize_cgm_data(
    load_missingness_demo_cgm_data(),
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group", visit = "visit")
  )
  result <- run_cgmissingdata_imputation(data, seed = 42)
  imputed <- apply_imputed_glucose(data, result)

  expect_equal(nrow(imputed), nrow(data))
  expect_equal(sum(imputed$imputed_flag), sum(is.na(data$glucose)))
  expect_false(any(is.na(imputed$glucose[imputed$imputed_flag])))
  expect_s3_class(create_trace_plot(imputed), "ggplot")
  expect_equal(nrow(compute_qc_summary(imputed)), 2)
  expect_equal(nrow(compute_core_metrics(imputed)), 2)
})
