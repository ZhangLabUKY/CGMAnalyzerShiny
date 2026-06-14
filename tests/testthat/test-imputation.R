test_that("apply_imputed_glucose preserves rows and flags only originally missing values", {
  data <- standardize_cgm_data(
    load_example_missing_5pct_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES"),
    timestamp_parser = "compatibility"
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
  attr(fake_result, "cgmissingdata_model") <- "arima"
  attr(fake_result, "cgmissingdata_backend") <- "mice"
  attr(fake_result, "cgmissingdata_warning_threshold") <- 0.2

  imputed <- apply_imputed_glucose(data, fake_result)

  expect_equal(nrow(imputed), nrow(data))
  expect_equal(imputed$glucose[-missing_rows], original_glucose[-missing_rows])
  expect_equal(imputed$glucose[missing_rows], rep(123, length(missing_rows)))
  expect_equal(which(imputed$imputed_flag), missing_rows)
  expect_equal(attr(imputed, "imputation_method"), "MICE+ARIMA")
  expect_equal(attr(imputed, "imputation_missing_rate"), mean(is.na(data$glucose)))
  expect_equal(attr(imputed, "imputation_model"), "arima")
  expect_equal(attr(imputed, "imputation_backend"), "mice")
  expect_equal(attr(imputed, "imputation_warning_threshold"), 0.2)
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

test_that("apply_imputed_glucose carries unique subject metadata into inserted rows", {
  data <- data.frame(
    id = c("A", "A", "B", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-06 00:00:00",
      "2026-05-06 00:10:00",
      "2026-05-06 00:00:00",
      "2026-05-06 00:10:00"
    )),
    glucose = c(100, 120, 130, 140),
    units = "mg/dL",
    group = c("Control", "Control", "Treatment", "Treatment"),
    source_file = c("A.csv", "A.csv", "B.csv", "B.csv"),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  fake_result <- data.frame(
    .row_id = c(1L, NA_integer_, 2L, 3L, NA_integer_, 4L),
    subject_index = c(1L, 1L, 1L, 2L, 2L, 2L),
    time = c(
      "2026-05-06T00:00:00",
      "2026-05-06T00:05:00",
      "2026-05-06T00:10:00",
      "2026-05-06T00:00:00",
      "2026-05-06T00:05:00",
      "2026-05-06T00:10:00"
    ),
    glucose = c(100, NA, 120, 130, NA, 140),
    imputed_glucose_value = c(100, 111, 120, 130, 135, 140),
    stringsAsFactors = FALSE
  )

  imputed <- apply_imputed_glucose(data, fake_result)
  gap_rows <- imputed[imputed$inserted_timestamp_gap %in% TRUE, , drop = FALSE]

  expect_equal(nrow(gap_rows), 2L)
  expect_equal(gap_rows$group[match(c("A", "B"), gap_rows$id)], c("Control", "Treatment"))
  expect_equal(gap_rows$source_file[match(c("A", "B"), gap_rows$id)], c("A.csv", "B.csv"))
  expect_true(all(gap_rows$imputed_flag))
})

test_that("run_cgmissingdata_imputation missing-rate fallback can use expanded package output", {
  local_mocked_bindings(
    cgmissingdata_version = function() numeric_version("0.0.2"),
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
  expect_equal(result$imputation_method, rep("MICE+XGBoost", 3))
  expect_equal(attr(result, "cgmissingdata_model"), "auto")
})

test_that("CGMissingDataR external messages are suppressed during imputation", {
  local_mocked_bindings(
    cgmissingdata_version = function() numeric_version("0.0.2"),
    cgmissingdata_function = function() {
      function(...) {
        message("Gap found in data for subject id: 1013")
        warning("external warning")
        data.frame(
          .row_id = c(1L, 2L),
          subject_index = c(1L, 1L),
          time = c("2026-05-06T00:00:00", "2026-05-06T00:05:00"),
          glucose = c(100, NA),
          imputed_glucose_value = c(100, 111),
          stringsAsFactors = FALSE
        )
      }
    }
  )
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-06 00:00:00", "2026-05-06 00:05:00")),
    glucose = c(100, NA_real_),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  messages <- character()
  withCallingHandlers(
    result <- run_cgmissingdata_imputation(data, interval_minutes = 5),
    message = function(message) {
      messages <<- c(messages, conditionMessage(message))
      invokeRestart("muffleMessage")
    }
  )

  expect_false(any(grepl("Gap found", messages, fixed = TRUE)))
  expect_equal(result$imputed_glucose_value[[2L]], 111)
  expect_equal(attr(result, "cgmissingdata_warnings"), "external warning")
})

test_that("CGMissingDataR model argument mapping supports enhanced model choices", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-06 00:00:00", "2026-05-06 00:05:00")),
    glucose = c(100, NA_real_),
    age = c("42", "42"),
    sex = c("F", "F"),
    hba1c = c("6.1", "6.1"),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  captured_models <- character()
  captured_features <- list()
  local_mocked_bindings(
    cgmissingdata_version = function() numeric_version("0.0.2"),
    cgmissingdata_function = function() {
      function(...) {
        args <- list(...)
        captured_models <<- c(captured_models, args$models)
        captured_features <<- c(captured_features, list(args$feature_cols))
        data.frame(
          .row_id = c(1L, 2L),
          subject_index = c(1L, 1L),
          time = c("2026-05-06T00:00:00", "2026-05-06T00:05:00"),
          glucose = c(100, NA),
          imputed_glucose_value = c(100, 111),
          stringsAsFactors = FALSE
        )
      }
    }
  )

  models <- cgmissingdata_imputation_models()
  for (model in models) {
    run_cgmissingdata_imputation(data, model = model, interval_minutes = 5)
  }

  expect_equal(captured_models, models)
  expect_true(all(vapply(captured_features, identical, logical(1), c("age", "sex", "hba1c"))))
})

test_that("CGMissingDataR adapter passes hidden defaults and analysis date study bounds", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-06-01 00:00:00", "2026-06-01 00:05:00")),
    glucose = c(100, NA_real_),
    sex = c("F", "F"),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  captured <- NULL
  local_mocked_bindings(
    cgmissingdata_version = function() numeric_version("0.0.2"),
    cgmissingdata_function = function() {
      function(...) {
        captured <<- list(...)
        data.frame(
          .row_id = c(1L, 2L),
          subject_index = c(1L, 1L),
          time = c("2026-06-01T00:00:00", "2026-06-01T00:05:00"),
          glucose = c(100, NA),
          imputed_glucose_value = c(100, 111),
          stringsAsFactors = FALSE
        )
      }
    }
  )

  settings <- create_reproducibility_settings(
    imputation_method = "mice_only",
    imputation_model = "rf",
    imputation_available = TRUE,
    analysis_date_range = c(start = "2026-06-01", end = "2026-06-14")
  )
  result <- apply_imputation_settings(data, settings)

  expect_true(any(result$imputed_flag))
  expect_equal(captured$models, "rf")
  expect_equal(captured$imputer_backend, "mice")
  expect_equal(captured$seed, 42)
  expect_equal(captured$interval_minutes, 5)
  expect_equal(captured$missing_warning_threshold, 0.20)
  expect_equal(captured$arima_order, c(4L, 1L, 0L))
  expect_equal(captured$xgb_nrounds, 300)
  expect_equal(captured$rf_n_estimators, 200)
  expect_equal(captured$knn_k, 7)
  expect_equal(captured$lgb_nrounds, 400)
  expect_equal(captured$lag_k, c(1L, 2L, 3L))
  expect_true(captured$add_rollmean)
  expect_equal(captured$roll_window, 3)
  expect_equal(captured$study_start, "2026-06-01")
  expect_equal(captured$study_end, "2026-06-14")
  expect_equal(captured$feature_cols, "sex")
})

test_that("imputation settings run separately per Subject ID for auto models", {
  make_subject <- function(id, n, missing) {
    data.frame(
      id = id,
      timestamp = parse_cgm_timestamp("2026-06-01 00:00:00") + seq(0, by = 300, length.out = n),
      glucose = replace(rep(100, n), missing, NA_real_),
      imputed_flag = FALSE,
      stringsAsFactors = FALSE
    )
  }
  data <- rbind(
    make_subject("A", 20, 1),
    make_subject("B", 10, c(1, 2)),
    make_subject("C", 5, integer())
  )
  captured <- data.frame(id = character(), model = character(), seed = integer())
  local_mocked_bindings(
    run_cgmissingdata_imputation = function(data, seed, model, ...) {
      id <- unique(as.character(data$id))
      captured <<- rbind(
        captured,
        data.frame(id = id, model = model, seed = seed, stringsAsFactors = FALSE)
      )
      method <- if (identical(id, "A")) "MICE+ARIMA" else "MICE+XGBoost"
      data.frame(
        .row_id = seq_len(nrow(data)),
        glucose = data$glucose,
        imputed_glucose_value = ifelse(is.na(data$glucose), 111, data$glucose),
        imputation_method = method,
        missing_rate = mean(is.na(data$glucose)),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- apply_imputation_settings(
    data,
    list(
      imputation_method = "mice_only",
      imputation_model = "auto",
      imputation_available = TRUE,
      imputation_seed = 100,
      imputation_interval_minutes = 5L
    )
  )

  expect_equal(captured$id, c("A", "B"))
  expect_equal(captured$model, c("auto", "auto"))
  expect_equal(captured$seed, c(100L, 101L))
  expect_equal(sum(result$imputed_flag %in% TRUE), 3L)
  expect_false(any(result$imputed_flag %in% TRUE & result$id == "C"))
  expect_equal(attr(result, "imputation_method"), c("MICE+ARIMA", "MICE+XGBoost"))
  expect_equal(
    attr(result, "imputation_method_by_subject")[["Subject ID"]],
    c("A", "B")
  )
  expect_equal(
    attr(result, "imputation_method_by_subject")$Method,
    c("MICE+ARIMA", "MICE+XGBoost")
  )
})

test_that("imputation settings run explicit models per Subject ID", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 2),
    timestamp = rep(parse_cgm_timestamp(c("2026-06-01 00:00:00", "2026-06-01 00:05:00")), 2),
    glucose = c(100, NA, 120, NA),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  captured_models <- character()
  local_mocked_bindings(
    run_cgmissingdata_imputation = function(data, model, ...) {
      captured_models <<- c(captured_models, model)
      data.frame(
        .row_id = seq_len(nrow(data)),
        glucose = data$glucose,
        imputed_glucose_value = ifelse(is.na(data$glucose), 111, data$glucose),
        imputation_method = "MICE+kNN",
        missing_rate = mean(is.na(data$glucose)),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- apply_imputation_settings(
    data,
    list(
      imputation_method = "mice_only",
      imputation_model = "knn",
      imputation_available = TRUE,
      imputation_interval_minutes = 5L
    )
  )

  expect_equal(captured_models, c("knn", "knn"))
  expect_equal(sum(result$imputed_flag %in% TRUE), 2L)
  expect_equal(attr(result, "imputation_method"), "MICE+kNN")
  expect_equal(nrow(attr(result, "imputation_method_by_subject")), 2L)
})

test_that("CGMissingDataR call helper passes only supported fixed-signature arguments", {
  captured <- NULL
  fake <- function(data, models, n_threads, prefer_cgmanalyzer_equal_interval) {
    captured <<- list(
      data = data,
      models = models,
      n_threads = n_threads,
      prefer_cgmanalyzer_equal_interval = prefer_cgmanalyzer_equal_interval
    )
    data
  }

  result <- call_cgmissingdata_imputation(
    fake,
    list(
      data = data.frame(glucose = NA_real_),
      models = "xgboost",
      n_threads = 1L,
      prefer_cgmanalyzer_equal_interval = FALSE,
      unsupported = "ignored"
    )
  )

  expect_s3_class(result, "data.frame")
  expect_equal(names(captured), c("data", "models", "n_threads", "prefer_cgmanalyzer_equal_interval"))
  expect_equal(captured$models, "xgboost")
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

test_that("imputation status reports not-run and stale analysis data", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-06 00:00:00", "2026-05-06 00:05:00")),
    glucose = c(100, NA),
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  not_run <- data
  attr(not_run, "imputation_pending") <- "not_run"
  stale <- data
  attr(stale, "imputation_pending") <- "stale"
  settings <- list(imputation_method = "mice_only", imputation_available = TRUE, imputation_interval_minutes = 5L)

  not_run_status <- summarize_imputation_status(data, not_run, settings)
  stale_status <- summarize_imputation_status(data, stale, settings)

  expect_equal(not_run_status$Status, "Not run")
  expect_match(not_run_status$Message, "has not been run", fixed = TRUE)
  expect_equal(stale_status$Status, "Stale")
  expect_match(stale_status$Message, "changed", fixed = TRUE)
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

test_that("CGMissingDataR input includes optional imputation feature mappings", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-06 11:30:00", "2026-05-06 11:35:00")),
    glucose = c(100, NA),
    age = c("44", "44"),
    sex = c("F", "F"),
    hba1c = c("6.4", "6.4"),
    stringsAsFactors = FALSE
  )

  imputation_input <- prepare_cgmissingdata_input(data)

  expect_equal(cgmissingdata_feature_cols(imputation_input), c("age", "sex", "hba1c"))
  expect_equal(imputation_input$age, c(44, 44))
  expect_equal(imputation_input$sex, c("F", "F"))
  expect_equal(imputation_input$hba1c, c(6.4, 6.4))
})

test_that("CGMissingDataR feature columns omit entirely missing optional mappings", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-06 11:30:00"),
    glucose = NA_real_,
    age = NA_character_,
    sex = NA_character_,
    hba1c = NA_character_,
    stringsAsFactors = FALSE
  )

  imputation_input <- prepare_cgmissingdata_input(data)

  expect_equal(cgmissingdata_feature_cols(imputation_input), character())
})

test_that("CGMissingDataR adapter can impute 5 percent missing example when available", {
  testthat::skip_if_not(cgmissingdata_available())

  data <- standardize_cgm_data(
    load_example_missing_5pct_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES"),
    timestamp_parser = "compatibility"
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
  expect_equal(nrow(filter_metrics_by_period(compute_core_metrics(imputed), "full_day")), 5)
})
