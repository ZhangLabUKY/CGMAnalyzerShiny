complexity_example_data <- function() {
  standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
}

test_that("compute_complexity_metrics returns finite core metrics for complete example", {
  data <- complexity_example_data()
  results <- compute_complexity_metrics(data, min_points = 80)

  expect_equal(nrow(results), 5)
  expect_true(all(results$eligible))
  expect_true(all(results$usable_points >= 80))
  expect_true(all(is.finite(results$shannon_entropy)))
  expect_true(all(results$shannon_entropy >= 0 & results$shannon_entropy <= 1))
  expect_true(any(is.finite(results$sample_entropy)))
  expect_true(any(is.finite(results$approximate_entropy)))
  expect_true(any(is.finite(results$hurst_exponent)))
  expect_true(any(is.finite(results$dfa_alpha)))
  expect_true(any(is.finite(results$higuchi_fractal_dimension)))
})

test_that("complexity metrics report ineligible short series", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-05 08:05:00")),
    glucose = c(100, 105),
    stringsAsFactors = FALSE
  )

  results <- compute_complexity_metrics(data, min_points = 100)

  expect_false(results$eligible)
  expect_equal(results$usable_points, 2)
  expect_true(grepl("Needs at least 100", results$notes, fixed = TRUE))
  expect_true(is.na(results$sample_entropy))
})

test_that("complexity regularization does not bridge large gaps", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 10:00:00",
      "2026-05-05 10:05:00"
    )),
    glucose = c(100, 110, 120, 130),
    stringsAsFactors = FALSE
  )

  results <- compute_complexity_metrics(data, min_points = 5, max_gap_intervals = 4)

  expect_equal(results$regularized_points, 26)
  expect_equal(results$usable_points, 4)
  expect_false(results$eligible)
  expect_gte(results$gap_count, 1)
})

test_that("CGManalyzer MSE wrapper restores working directory for short series", {
  old_wd <- getwd()
  result <- compute_cgmanalyzer_mse(seq_len(20), scale_max = 5)

  expect_identical(getwd(), old_wd)
  expect_true(is.na(result$value))
  expect_true(grepl("Needs at least 50", result$note, fixed = TRUE))
})

test_that("CGManalyzer MSE wrapper restores working directory after live call", {
  skip_if_not_installed("CGManalyzer")
  skip_if(!"MSEbyC.fn" %in% getNamespaceExports("CGManalyzer"))
  old_wd <- getwd()
  result <- compute_cgmanalyzer_mse(sin(seq_len(120) / 4) + seq_len(120) / 50, scale_max = 5)

  expect_identical(getwd(), old_wd)
  expect_true(is.finite(result$value) || is.na(result$value))
  expect_true(is.character(result$note))
})

test_that("advanced complexity helpers handle sufficient and constant series", {
  x <- sin(seq_len(160) / 5) + seq_len(160) / 100
  constant <- rep(100, 160)

  expect_true(is.finite(safe_dfa_alpha(x)))
  expect_true(is.finite(safe_higuchi_fd(x, kmax = 8)))
  expect_true(is.na(safe_dfa_alpha(constant)))
  expect_true(is.na(safe_higuchi_fd(constant, kmax = 8)))
})

test_that("complexity display helpers use user-facing labels", {
  data <- complexity_example_data()
  results <- compute_complexity_metrics(data, min_points = 80)
  display <- prepare_complexity_metrics_display(results, data)

  expect_true(all(c("Subject ID", "Metric", "Value", "Units / scale", "Definition", "Notes") %in% names(display)))
  expect_true(all(c(
    "Shannon entropy",
    "Sample entropy",
    "Approximate entropy",
    "Multiscale sample entropy",
    "Hurst exponent",
    "DFA alpha",
    "Higuchi fractal dimension"
  ) %in% display$Metric))
  expect_false(any(grepl("pracma|package|engine|adapter", unlist(display), ignore.case = TRUE)))
})

test_that("complexity display hides Subject ID for one filename-derived subject", {
  data <- data.frame(
    id = "FallbackA",
    id_source = subject_id_source_filename(),
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00") + seq(0, by = 300, length.out = 120),
    glucose = 100 + sin(seq_len(120) / 8),
    stringsAsFactors = FALSE
  )
  results <- compute_complexity_metrics(data, min_points = 100)

  expect_false("Subject ID" %in% names(prepare_complexity_metrics_display(results, data)))
})

test_that("complexity summary cards report eligibility and parameters", {
  data <- complexity_example_data()
  params <- complexity_default_parameters(min_points = 80, entropy_bin_width = 10, embedding_dimension = 2, tolerance_multiplier = 0.2)
  cards <- complexity_summary_cards(compute_complexity_metrics(data, min_points = 80), params)

  expect_equal(cards$Value[cards$Label == "Eligible Subject IDs"], "5")
  expect_equal(cards$Value[cards$Label == "Needs review"], "0")
  expect_true(grepl("m=2", cards$Value[cards$Label == "Parameters"], fixed = TRUE))
  expect_true(grepl("MSE scales 1-5", cards$Value[cards$Label == "Parameters"], fixed = TRUE))
  expect_true(grepl("Higuchi kmax=8", cards$Value[cards$Label == "Parameters"], fixed = TRUE))
})

test_that("complexity module exposes controls, tables, and export", {
  html <- paste(as.character(complexity_module_ui("complexity")), collapse = "\n")

  expect_true(grepl("complexity-subject_filter", html, fixed = TRUE))
  expect_true(grepl("complexity-min_points", html, fixed = TRUE))
  expect_true(grepl("complexity-entropy_bin_width", html, fixed = TRUE))
  expect_true(grepl("complexity-embedding_dimension", html, fixed = TRUE))
  expect_true(grepl("complexity-tolerance_multiplier", html, fixed = TRUE))
  expect_true(grepl("complexity-mse_scale_max", html, fixed = TRUE))
  expect_true(grepl("complexity-higuchi_kmax", html, fixed = TRUE))
  expect_true(grepl("complexity-summary_cards", html, fixed = TRUE))
  expect_false(grepl("complexity-data_requirements", html, fixed = TRUE))
  expect_false(grepl("Data requirements", html, fixed = TRUE))
  expect_true(grepl("complexity-metrics_table", html, fixed = TRUE))
  expect_true(grepl("complexity-download_complexity", html, fixed = TRUE))
})
