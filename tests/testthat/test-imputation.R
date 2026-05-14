test_that("apply_imputed_glucose preserves rows and flags only originally missing values", {
  data <- standardize_cgm_data(
    load_example_missing_5pct_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
  original_glucose <- data$glucose
  missing_rows <- which(is.na(data$glucose))
  fake_values <- ifelse(is.na(data$glucose), 123, data$glucose + 1000)
  fake_result <- data.frame(
    .row_id = seq_len(nrow(data)),
    glucose = data$glucose,
    imputed_glucose_value = fake_values,
    imputation_method = "MICE+ARIMA",
    missing_rate = mean(is.na(data$glucose)),
    stringsAsFactors = FALSE
  )

  imputed <- apply_imputed_glucose(data, fake_result)

  expect_equal(nrow(imputed), nrow(data))
  expect_equal(imputed$glucose[-missing_rows], original_glucose[-missing_rows])
  expect_equal(imputed$glucose[missing_rows], rep(123, length(missing_rows)))
  expect_equal(which(imputed$imputed_flag), missing_rows)
  expect_equal(attr(imputed, "imputation_method"), "MICE+ARIMA")
  expect_equal(attr(imputed, "imputation_missing_rate"), mean(is.na(data$glucose)))
})

test_that("apply_imputed_glucose rejects old benchmark-style imputed lists", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-06 11:30:00"),
    glucose = NA_real_,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  old_result <- list(imputed_data = list(mice_only = data.frame(.RowID = 1, ImputedValue = 123)))

  expect_error(apply_imputed_glucose(data, old_result), "must be a data frame")
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

  expect_equal(imputation_input$.row_id, 1L)
  expect_equal(imputation_input$subject_index, 1L)
  expect_equal(imputation_input$time, "2026-05-06T11:30:00")
  expect_equal(imputation_input$glucose, NA_real_)
})

test_that("CGMissingDataR input converts character subject ids to numeric indices", {
  data <- data.frame(
    id = c("SubjectA", "SubjectB", "SubjectA"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-06 11:30:00",
      "2026-05-06 11:35:00",
      "2026-05-06 11:40:00"
    )),
    glucose = c(100, NA, 110),
    stringsAsFactors = FALSE
  )

  imputation_input <- prepare_cgmissingdata_input(data)

  expect_equal(imputation_input$subject_index, c(1L, 2L, 1L))
  expect_type(imputation_input$subject_index, "integer")
})

test_that("CGMissingDataR adapter can impute 5 percent missing example when available", {
  testthat::skip_if_not(cgmissingdata_available())

  data <- standardize_cgm_data(
    load_example_missing_5pct_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
  result <- suppressWarnings(run_cgmissingdata_imputation(data, seed = 42, backend = "mice"))
  imputed <- apply_imputed_glucose(data, result)

  expect_s3_class(result, "data.frame")
  expect_true(all(c(".row_id", "imputed_glucose_value", "imputation_method", "missing_rate") %in% names(result)))
  expect_equal(nrow(imputed), nrow(data))
  expect_equal(sum(imputed$imputed_flag), sum(is.na(data$glucose)))
  expect_false(any(is.na(imputed$glucose[imputed$imputed_flag])))
  expect_s3_class(create_trace_plot(imputed), "ggplot")
  expect_equal(nrow(compute_qc_summary(imputed)), 5)
  expect_equal(nrow(compute_core_metrics(imputed)), 5)
})
