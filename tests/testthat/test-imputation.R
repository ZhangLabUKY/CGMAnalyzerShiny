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

test_that("apply_imputed_glucose appends generated timestamp-gap rows", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-06 00:00:00", "2026-05-06 00:10:00")),
    glucose = c(100, 120),
    units = "mg/dL",
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  fake_result <- data.frame(
    .row_id = c(1L, NA_integer_, 2L),
    subject_index = c(1L, 1L, 1L),
    time = c("2026-05-06T00:00:00", "2026-05-06T00:05:00", "2026-05-06T00:10:00"),
    glucose = c(100, NA, 120),
    imputed_glucose_value = c(100, 111, 120),
    imputation_method = "MICE+ARIMA",
    missing_rate = 1 / 3,
    stringsAsFactors = FALSE
  )

  imputed <- apply_imputed_glucose(data, fake_result)
  gap_row <- imputed[imputed$inserted_timestamp_gap %in% TRUE, , drop = FALSE]

  expect_equal(nrow(imputed), nrow(data) + 1L)
  expect_equal(nrow(gap_row), 1L)
  expect_equal(gap_row$glucose, 111)
  expect_true(gap_row$imputed_flag)
  expect_equal(gap_row$missing_source, missing_source_gap())
  expect_equal(attr(imputed, "imputation_missing_rate"), 1 / 3)
})

test_that("run_cgmissingdata_imputation missing-rate fallback can use expanded package output", {
  local_mocked_bindings(
    cgmissingdata_function = function() {
      function(...) {
        data.frame(
          .row_id = c(1L, NA_integer_, 2L),
          subject_index = c(1L, 1L, 1L),
          time = c("2026-05-06T00:00:00", "2026-05-06T00:05:00", "2026-05-06T00:10:00"),
          glucose = c(100, NA, 120),
          imputed_glucose_value = c(100, 111, 120),
          stringsAsFactors = FALSE
        )
      }
    }
  )
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-06 00:00:00", "2026-05-06 00:10:00")),
    glucose = c(100, 120),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  result <- run_cgmissingdata_imputation(data, interval_minutes = 5)

  expect_equal(result$missing_rate, rep(1 / 3, 3))
  expect_equal(result$imputation_method, rep("mice", 3))
})

test_that("imputation settings run for timestamp-gap candidates without explicit NA rows", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-06 00:00:00", "2026-05-06 00:10:00")),
    glucose = c(100, 120),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  called <- FALSE
  local_mocked_bindings(
    run_cgmissingdata_imputation = function(...) {
      called <<- TRUE
      data.frame(
        .row_id = c(1L, NA_integer_, 2L),
        subject_index = c(1L, 1L, 1L),
        time = c("2026-05-06T00:00:00", "2026-05-06T00:05:00", "2026-05-06T00:10:00"),
        glucose = c(100, NA, 120),
        imputed_glucose_value = c(100, 111, 120),
        imputation_method = "MICE+ARIMA",
        missing_rate = 1 / 3,
        stringsAsFactors = FALSE
      )
    }
  )

  result <- apply_imputation_settings(
    data,
    list(
      imputation_method = "mice_only",
      imputation_available = TRUE,
      imputation_interval_minutes = 5L,
      imputation_seed = 42
    )
  )

  expect_true(called)
  expect_equal(nrow(result), 3L)
  expect_equal(sum(result$inserted_timestamp_gap %in% TRUE), 1L)
  expect_equal(sum(result$imputed_flag %in% TRUE), 1L)
})

test_that("imputation candidate summary counts explicit NA and timestamp gaps", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c(
      "2026-05-06 00:00:00",
      "2026-05-06 00:05:00",
      "2026-05-06 00:15:00"
    )),
    glucose = c(100, NA, 130),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  summary <- imputation_candidate_summary(data, interval_minutes = 5)

  expect_equal(summary$explicit_missing_glucose, 1L)
  expect_equal(summary$inferred_timestamp_gap_rows, 1L)
  expect_equal(summary$missing_candidates, 2L)
  expect_equal(summary$expanded_rows, 4L)
  expect_equal(summary$missing_rate, 0.5)
})

test_that("imputation settings skip when no explicit or inferred candidates exist", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-06 00:00:00", "2026-05-06 00:05:00")),
    glucose = c(100, 120),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    run_cgmissingdata_imputation = function(...) {
      stop("imputation should not run", call. = FALSE)
    }
  )

  result <- apply_imputation_settings(
    data,
    list(
      imputation_method = "mice_only",
      imputation_available = TRUE,
      imputation_interval_minutes = 5L
    )
  )

  expect_equal(result, data)
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
