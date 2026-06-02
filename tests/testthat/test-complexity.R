complexity_example_data <- function() {
  standardize_cgm_data(
    load_example_complete_cgm_data(),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
}

complexity_grouped_example_data <- function() {
  data <- complexity_example_data()
  ids <- sort(unique(data$id))
  group_map <- stats::setNames(rep(c("F", "M"), length.out = length(ids)), ids)
  data$group <- unname(group_map[as.character(data$id)])
  data
}

test_that("compute_complexity_metrics returns finite core metrics for complete example", {
  data <- complexity_example_data()
  results <- compute_complexity_metrics(data, min_points = 80)

  expect_equal(nrow(results), 5)
  expect_true(all(results$eligible))
  expect_true(all(results$usable_points >= 80))
  expect_true(all(is.finite(results$shannon_entropy)))
  expect_true(all(results$shannon_entropy >= 0 & results$shannon_entropy <= 1))
  expect_true(all(is.na(results$sample_entropy)))
  expect_true(all(is.na(results$approximate_entropy)))
  expect_true(any(is.finite(results$hurst_exponent)))
  expect_true(any(is.finite(results$dfa_alpha)))
  expect_true(any(is.finite(results$higuchi_fractal_dimension)))
  expect_true(all(is.na(results$multiscale_sample_entropy)))
  expect_true(all(grepl("Pending background MSE", results$multiscale_sample_entropy_note, fixed = TRUE)))
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
  expect_true(is.na(results$approximate_entropy))
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

test_that("pending complexity summary is lightweight and marks metrics pending", {
  data <- data.frame(
    id = c("A", "A", "A", "A", "B", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-01 00:00:00",
      "2026-05-01 00:05:00",
      "2026-05-01 00:10:00",
      "2026-05-29 00:00:00",
      "2026-05-03 08:00:00",
      "2026-05-03 08:05:00"
    )),
    glucose = c(100, 105, 110, 115, 120, 125),
    stringsAsFactors = FALSE
  )
  params <- complexity_default_parameters(min_points = 2)

  pending <- compute_complexity_pending_summary(data, params)

  expect_equal(nrow(pending), 2)
  expect_true(all(is.na(pending$regularized_points)))
  expect_true(all(is.na(pending$sample_entropy)))
  expect_true(all(!nzchar(pending$sample_entropy_note)))
  expect_true(all(!nzchar(pending$approximate_entropy_note)))
  expect_equal(pending$gap_count[pending$id == "A"], 1)
})

test_that("failed complexity summary keeps rows and marks metric notes failed", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00") + seq(0, by = 300, length.out = 120),
    glucose = 100 + sin(seq_len(120) / 8),
    stringsAsFactors = FALSE
  )
  params <- complexity_default_parameters(min_points = 100)

  failed <- compute_complexity_pending_summary(data, params, status = "failed")

  expect_true(failed$eligible)
  expect_false(nzchar(failed$sample_entropy_note))
  expect_false(nzchar(failed$approximate_entropy_note))
  expect_true(grepl("could not compute", failed$multiscale_sample_entropy_note, fixed = TRUE))
})

test_that("quick complexity metrics populate Shannon and leave removed entropy scalars empty", {
  data <- complexity_example_data()
  params <- complexity_default_parameters(min_points = 80)

  quick <- compute_complexity_quick_metrics(data, params)

  expect_equal(nrow(quick), length(subject_id_values(data)))
  expect_true(all(quick$eligible))
  expect_true(all(is.finite(quick$shannon_entropy)))
  expect_true(all(is.na(quick$sample_entropy)))
  expect_true(all(is.na(quick$approximate_entropy)))
  expect_true(all(is.na(quick$hurst_exponent)))
  expect_true(all(!nzchar(quick$sample_entropy_note)))
  expect_true(all(!nzchar(quick$approximate_entropy_note)))
  expect_true(all(grepl("Hurst exponent is calculating", quick$hurst_exponent_note, fixed = TRUE)))
  expect_true(all(is.na(quick$dfa_alpha)))
  expect_true(all(is.na(quick$higuchi_fractal_dimension)))
  expect_true(all(grepl("Pending DFA/Higuchi", quick$dfa_alpha_note, fixed = TRUE)))
  expect_true(all(is.na(quick$multiscale_sample_entropy)))
})

test_that("Hurst merge fills Hurst without dropping quick metadata", {
  data <- complexity_example_data()
  params <- complexity_default_parameters(min_points = 80)
  quick <- compute_complexity_quick_metrics(data, params)
  hurst <- compute_complexity_hurst_metrics(data, params)

  merged <- merge_complexity_hurst_results(quick, hurst, status = "complete")

  expect_equal(merged$id, quick$id)
  expect_equal(merged$readings, quick$readings)
  expect_equal(merged$usable_points, quick$usable_points)
  expect_equal(merged$shannon_entropy, quick$shannon_entropy)
  expect_true(all(is.na(merged$sample_entropy)))
  expect_true(all(is.na(merged$approximate_entropy)))
  expect_true(any(is.finite(merged$hurst_exponent)))
  expect_true(all(!nzchar(merged$sample_entropy_note)))
  expect_true(all(!nzchar(merged$approximate_entropy_note)))
})

test_that("Hurst merge marks pending and failed values by stage status", {
  data <- complexity_example_data()
  params <- complexity_default_parameters(min_points = 80)
  quick <- compute_complexity_quick_metrics(data, params)

  running <- merge_complexity_hurst_results(quick, NULL, status = "running")
  failed <- merge_complexity_hurst_results(quick, NULL, status = "failed")

  expect_true(all(is.na(running$hurst_exponent)))
  expect_true(all(grepl("Hurst exponent is calculating", running$hurst_exponent_note, fixed = TRUE)))
  expect_true(all(is.na(failed$hurst_exponent)))
  expect_true(all(grepl("Hurst exponent could not compute", failed$hurst_exponent_note, fixed = TRUE)))
})

test_that("CGManalyzer MSE wrapper restores working directory for short series", {
  old_wd <- getwd()
  result <- compute_cgmanalyzer_mse(seq_len(20), scale_max = 5)

  expect_identical(getwd(), old_wd)
  expect_true(is.na(result$value))
  expect_true(grepl("Needs at least 50", result$note, fixed = TRUE))
  expect_s3_class(result$scales, "data.frame")
  expect_equal(nrow(result$scales), 0)
})

test_that("CGManalyzer MSE wrapper restores working directory after live call", {
  skip_if_not_installed("CGManalyzer")
  skip_if(!"MSEbyC.fn" %in% getNamespaceExports("CGManalyzer"))
  old_wd <- getwd()
  result <- compute_cgmanalyzer_mse(sin(seq_len(120) / 4) + seq_len(120) / 50, scale_max = 5)

  expect_identical(getwd(), old_wd)
  expect_true(is.finite(result$value) || is.na(result$value))
  expect_true(is.character(result$note))
  expect_s3_class(result$scales, "data.frame")
  if (nrow(result$scales)) {
    expect_true(all(c("Scale", "SampleEntropy") %in% names(result$scales)))
    expect_true(is.na(result$value))
    expect_true(any(is.finite(result$scales$SampleEntropy)))
  }
})

test_that("advanced complexity helpers handle sufficient and constant series", {
  x <- sin(seq_len(160) / 5) + seq_len(160) / 100
  constant <- rep(100, 160)
  dfa <- compute_dfa_details(x)
  higuchi <- compute_higuchi_details(x, kmax = 8)

  expect_true(is.finite(safe_dfa_alpha(x)))
  expect_true(is.finite(safe_higuchi_fd(x, kmax = 8)))
  expect_true(is.finite(dfa$value))
  expect_true(is.finite(higuchi$value))
  expect_true(nrow(dfa$curve) > 0)
  expect_true(nrow(higuchi$curve) > 0)
  expect_true(is.na(safe_dfa_alpha(constant)))
  expect_true(is.na(safe_higuchi_fd(constant, kmax = 8)))
})

test_that("complexity display helpers use user-facing labels", {
  data <- complexity_example_data()
  results <- compute_complexity_metrics(data, min_points = 80)
  display <- prepare_complexity_metrics_display(results, data)
  scalar_metrics <- c(
    "Shannon entropy",
    "Hurst exponent"
  )

  expect_true(all(c("Subject ID", "Metric", "Value", "Units / scale", "Definition", "Notes") %in% names(display)))
  expect_true(all(scalar_metrics %in% display$Metric))
  expect_false("Sample entropy" %in% display$Metric)
  expect_false("Approximate entropy" %in% display$Metric)
  expect_false("DFA alpha" %in% display$Metric)
  expect_false("Higuchi fractal dimension" %in% display$Metric)
  expect_false("Multiscale sample entropy" %in% display$Metric)
  expect_false(any(grepl("pracma|package|engine|adapter", unlist(display), ignore.case = TRUE)))
})

test_that("complexity group filtering narrows data without changing subject-level calculation", {
  data <- complexity_grouped_example_data()
  filtered <- filter_complexity_data(data, group = "F")
  results <- compute_complexity_metrics(filtered, min_points = 80)

  expect_true(plot_filter_available(data, "group", min_values = 2))
  expect_true(nrow(filtered) < nrow(data))
  expect_setequal(unique(filtered$group), "F")
  expect_equal(nrow(results), length(subject_id_values(filtered)))
  expect_true(all(results$id %in% subject_id_values(filtered)))
  expect_equal(nrow(filter_complexity_data(data, group = all_filter_value())), nrow(data))
})

test_that("complexity display includes Group context when available", {
  data <- complexity_grouped_example_data()
  results <- compute_complexity_metrics(data, min_points = 80)
  display <- prepare_complexity_metrics_display(results, data)
  plot_data <- prepare_complexity_plot_data(results, data)

  expect_true("Group" %in% names(display))
  expect_true("Group" %in% names(plot_data))
  expect_setequal(clean_filter_values(display$Group), c("F", "M"))
  expect_true(any(grepl("Group:", plot_data$Tooltip, fixed = TRUE)))
})

test_that("complexity compute key ignores display filters when full data is reused", {
  data <- complexity_grouped_example_data()
  params <- complexity_default_parameters(min_points = 80)
  key <- complexity_compute_key(data, params)
  selected_subject <- subject_id_values(data)[[1L]]
  results <- compute_complexity_pending_summary(data, params)
  changed_params <- params
  changed_params$min_points <- params$min_points + 10

  filter_complexity_results(results, data, subject = selected_subject)
  expect_identical(key, complexity_compute_key(data, params))
  expect_false(identical(key, complexity_compute_key(data, changed_params)))
  expect_false(identical(key, complexity_compute_key(filter_complexity_data(data, subject = selected_subject), params)))
})

test_that("complexity display filters reuse all-subject scalar results", {
  data <- complexity_grouped_example_data()
  results <- compute_complexity_metrics(data, min_points = 80)
  all_ids <- subject_id_values(data)
  selected_subject <- all_ids[[1L]]
  group_ids <- subject_id_values(filter_complexity_data(data, group = "F"))

  subject_results <- filter_complexity_results(results, data, subject = selected_subject)
  group_results <- filter_complexity_results(results, data, group = "F")
  subject_display <- prepare_complexity_metrics_display(
    subject_results,
    filter_complexity_data(data, subject = selected_subject)
  )
  group_display <- prepare_complexity_metrics_display(
    group_results,
    filter_complexity_data(data, group = "F")
  )

  expect_equal(nrow(results), length(all_ids))
  expect_equal(unique(as.character(subject_results$id)), selected_subject)
  expect_setequal(as.character(group_results$id), group_ids)
  expect_true(nrow(subject_display) > 0)
  expect_equal(unique(as.character(subject_display$`Subject ID`)), selected_subject)
  expect_true(nrow(group_display) > 0)
  expect_setequal(clean_filter_values(group_display$Group), "F")
})

test_that("complexity display filters reuse all-subject scale curves", {
  data <- complexity_grouped_example_data()
  ids <- subject_id_values(data)
  curves <- data.frame(
    id = rep(ids, each = 2),
    curve_metric = rep(c("dfa", "higuchi"), length(ids)),
    scale_variable = "scale",
    scale_value = rep(1:2, length(ids)),
    metric_value = seq_along(rep(ids, each = 2)),
    value_label = "Curve value",
    derived_scalar_label = rep(c("DFA alpha", "Higuchi fractal dimension"), length(ids)),
    derived_scalar_value = seq_along(rep(ids, each = 2)) / 10,
    note = "",
    stringsAsFactors = FALSE
  )
  selected_subject <- ids[[2L]]
  group_ids <- subject_id_values(filter_complexity_data(data, group = "M"))

  subject_curves <- filter_complexity_curves(curves, data, subject = selected_subject)
  group_curves <- filter_complexity_curves(curves, data, group = "M")
  subject_plot_data <- prepare_complexity_curve_plot_data(
    subject_curves,
    filter_complexity_data(data, subject = selected_subject)
  )
  group_plot_data <- prepare_complexity_curve_plot_data(
    group_curves,
    filter_complexity_data(data, group = "M")
  )

  expect_equal(nrow(curves), length(ids) * 2L)
  expect_equal(unique(as.character(subject_curves$id)), selected_subject)
  expect_setequal(as.character(group_curves$id), group_ids)
  expect_true(nrow(subject_plot_data) > 0)
  expect_equal(unique(as.character(subject_plot_data$`Subject ID`)), selected_subject)
  expect_true(nrow(group_plot_data) > 0)
  expect_setequal(clean_filter_values(group_plot_data$Group), "M")
})

test_that("complexity export respects display filters over cached results", {
  data <- complexity_grouped_example_data()
  params <- complexity_default_parameters(min_points = 80, mse_scale_max = 2)
  quick <- compute_complexity_quick_metrics(data, params)
  hurst <- compute_complexity_hurst_metrics(data, params)
  metrics <- merge_complexity_hurst_results(quick, hurst, status = "complete")
  curves <- compute_complexity_dfa_higuchi_curves(data, params)
  selected_subject <- subject_id_values(data)[[1L]]
  display_data <- filter_complexity_data(data, subject = selected_subject)
  export <- prepare_complexity_export(
    filter_complexity_results(metrics, data, subject = selected_subject),
    filter_complexity_curves(curves, data, subject = selected_subject),
    display_data
  )

  expect_true(nrow(export) > 0)
  expect_equal(unique(as.character(export$`Subject ID`)), selected_subject)
})

test_that("complexity plot helpers prepare finite metric values and choices", {
  data <- complexity_example_data()
  results <- compute_complexity_metrics(data, min_points = 80)
  plot_data <- prepare_complexity_plot_data(results, data)
  choices <- complexity_metric_filter_choices()

  expect_equal(unname(choices[[1L]]), all_filter_value())
  expect_equal(names(choices)[[1L]], "All metrics")
  expect_true(all(complexity_metric_catalog()$metric %in% names(choices)))
  expect_false(any(c("Sample entropy", "Approximate entropy", "DFA alpha", "Higuchi fractal dimension", "Multiscale sample entropy") %in% names(choices)))
  expect_true(nrow(plot_data) > 0)
  expect_true(all(is.finite(plot_data$Value)))
  expect_true(all(c("Subject ID", "Metric", "Value", "Units / scale", "Notes", "Tooltip") %in% names(plot_data)))
  expect_false(any(c("Sample entropy", "Approximate entropy", "DFA alpha", "Higuchi fractal dimension", "Multiscale sample entropy") %in% plot_data$Metric))
  expect_true(any(grepl("Subject ID:", plot_data$Tooltip, fixed = TRUE)))
})

test_that("complexity plot data filters selected metrics", {
  data <- complexity_example_data()
  results <- compute_complexity_metrics(data, min_points = 80)
  plot_data <- prepare_complexity_plot_data(results, data, metric = "Shannon entropy")

  expect_true(nrow(plot_data) > 0)
  expect_equal(unique(as.character(plot_data$Metric)), "Shannon entropy")
})

test_that("complexity summary plots support all and single metric views", {
  data <- complexity_example_data()
  results <- compute_complexity_metrics(data, min_points = 80)
  all_data <- prepare_complexity_plot_data(results, data)
  single_data <- prepare_complexity_plot_data(results, data, metric = "Shannon entropy")

  all_plot <- create_complexity_summary_plot(all_data)
  single_plot <- create_complexity_summary_plot(single_data, metric = "Shannon entropy")
  empty <- create_complexity_summary_plot(data.frame())

  expect_s3_class(all_plot, "ggplot")
  expect_s3_class(single_plot, "ggplot")
  expect_s3_class(empty, "ggplot")
  expect_true(inherits(all_plot$facet, "FacetWrap"))
  expect_false(inherits(single_plot$facet, "FacetWrap"))
  expect_true(any(vapply(all_plot$scales$scales, function(scale) {
    "y" %in% scale$aesthetics && identical(scale$expand, c(0.18, 0.02, 0.18, 0.02))
  }, logical(1))))
  expect_gt(complexity_plot_height(all_filter_value()), complexity_plot_height("Shannon entropy"))
})

test_that("complexity MSE curve helpers prepare and plot scale-level data", {
  skip_if_not_installed("CGManalyzer")
  skip_if(!"MSEbyC.fn" %in% getNamespaceExports("CGManalyzer"))
  data <- complexity_example_data()
  curves <- compute_complexity_mse_curves(data, min_points = 80, mse_scale_max = 5)
  plot_data <- prepare_mse_curve_plot_data(curves, data)
  plot <- create_mse_curve_plot(plot_data)
  empty <- create_mse_curve_plot(data.frame())

  expect_true(nrow(curves) > 0)
  expect_true(all(c("id", "curve_metric", "scale_variable", "scale_value", "metric_value") %in% names(curves)))
  expect_setequal(unique(curves$curve_metric), "mse")
  expect_true(all(c("Subject ID", "Curve metric", "Scale variable", "Scale value", "Metric value", "Tooltip") %in% names(plot_data)))
  expect_true(all(is.finite(plot_data[["Scale value"]])))
  expect_true(all(is.finite(plot_data[["Metric value"]])))
  expect_true(all(!nzchar(plot_data[["Derived scalar"]])))
  expect_true(all(is.na(plot_data[["Derived scalar value"]])))
  expect_true(any(grepl("Scale:", plot_data$Tooltip, fixed = TRUE)))
  expect_s3_class(plot, "ggplot")
  expect_s3_class(empty, "ggplot")
})

test_that("complexity bundle returns scalar metrics and scale curves together", {
  data <- complexity_example_data()
  params <- complexity_default_parameters(min_points = 80, mse_scale_max = 2)

  bundle <- compute_complexity_bundle(data, params, include_mse = FALSE)

  expect_s3_class(bundle$metrics, "data.frame")
  expect_s3_class(bundle$curves, "data.frame")
  expect_equal(nrow(bundle$metrics), length(subject_id_values(data)))
  expect_true(nrow(bundle$curves) > 0)
  expect_true(all(bundle$curves$curve_metric %in% c("dfa", "higuchi")))
  expect_false("mse" %in% bundle$curves$curve_metric)
  expect_true(all(c("sample_entropy", "approximate_entropy", "multiscale_sample_entropy") %in% names(bundle$metrics)))
  expect_true(all(is.na(bundle$metrics$sample_entropy)))
  expect_true(all(is.na(bundle$metrics$approximate_entropy)))
})

test_that("DFA/Higuchi curve helper returns only curve rows with derived scalar annotations", {
  data <- complexity_example_data()
  params <- complexity_default_parameters(min_points = 80, mse_scale_max = 2)

  curves <- compute_complexity_dfa_higuchi_curves(data, params)

  expect_true(nrow(curves) > 0)
  expect_setequal(unique(curves$curve_metric), c("dfa", "higuchi"))
  expect_false("mse" %in% curves$curve_metric)
  expect_true(all(is.finite(curves$scale_value)))
  expect_true(all(is.finite(curves$metric_value)))
  expect_true(all(is.finite(curves$derived_scalar_value)))
  expect_setequal(
    unique(curves$derived_scalar_label),
    c("DFA alpha", "Higuchi fractal dimension")
  )
})

test_that("complexity MSE results merge into fast results by status", {
  data <- complexity_example_data()
  fast <- compute_complexity_metrics(data, min_points = 80)
  mse <- data.frame(
    id = fast$id,
    multiscale_sample_entropy = seq_len(nrow(fast)) / 10,
    multiscale_sample_entropy_note = "",
    stringsAsFactors = FALSE
  )

  running <- merge_complexity_mse_results(fast, NULL, status = "running")
  complete <- merge_complexity_mse_results(fast, mse, status = "complete")
  failed <- merge_complexity_mse_results(fast, NULL, status = "failed")

  expect_true(all(is.na(running$multiscale_sample_entropy)))
  expect_true(all(grepl("Pending background MSE", running$multiscale_sample_entropy_note, fixed = TRUE)))
  expect_true(all(is.na(complete$multiscale_sample_entropy)))
  expect_true(all(is.na(failed$multiscale_sample_entropy)))
  expect_true(all(grepl("could not compute", failed$multiscale_sample_entropy_note, fixed = TRUE)))
  expect_true(grepl("MSE running", complexity_mse_status_text("running"), fixed = TRUE))
  expect_true(grepl("MSE available", complexity_mse_status_text("complete"), fixed = TRUE))
  expect_true(grepl("Complexity summary running", complexity_status_text("running"), fixed = TRUE))
  expect_true(grepl("Complexity summary available", complexity_status_text("complete"), fixed = TRUE))
  expect_true(grepl("Hurst exponent is calculating", complexity_scalar_status_text("running"), fixed = TRUE))
  expect_true(grepl("DFA/Higuchi curves running", complexity_curve_status_text("running"), fixed = TRUE))
  expect_true(grepl("could not compute", complexity_status_text("failed"), fixed = TRUE))
  expect_true(grepl("could not compute", complexity_scalar_status_text("failed"), fixed = TRUE))
  expect_true(grepl("could not compute", complexity_curve_status_text("failed"), fixed = TRUE))
  expect_true(grepl("could not compute", complexity_mse_status_text("failed"), fixed = TRUE))
  expect_equal(complexity_status_text("idle"), "")
  expect_equal(complexity_scalar_status_text("idle"), "")
  expect_equal(complexity_curve_status_text("idle"), "")
  expect_equal(complexity_mse_status_text("idle"), "")
})

test_that("complexity MSE curve plot data includes Group in tooltips when available", {
  skip_if_not_installed("CGManalyzer")
  skip_if(!"MSEbyC.fn" %in% getNamespaceExports("CGManalyzer"))
  data <- complexity_grouped_example_data()
  curves <- compute_complexity_mse_curves(data, min_points = 80, mse_scale_max = 5)
  plot_data <- prepare_mse_curve_plot_data(curves, data)

  expect_true("Group" %in% names(plot_data))
  expect_true(any(grepl("Group:", plot_data$Tooltip, fixed = TRUE)))
})

test_that("scale curve helpers support DFA and Higuchi derived scalar annotations", {
  data <- complexity_example_data()
  params <- complexity_default_parameters(min_points = 80, mse_scale_max = 2)
  curves <- compute_complexity_dfa_higuchi_curves(data, params)
  plot_data <- prepare_complexity_curve_plot_data(curves, data)
  dfa_only <- prepare_complexity_curve_plot_data(curves, data, curve_metric = "dfa")
  higuchi_only <- prepare_complexity_curve_plot_data(curves, data, curve_metric = "higuchi")
  plot <- create_complexity_scale_curve_plot(plot_data)
  dfa_plot <- create_complexity_scale_curve_plot(dfa_only)

  expect_true(all(c("dfa", "higuchi") %in% unique(curves$curve_metric)))
  expect_true(all(c("Derived scalar", "Derived scalar value") %in% names(plot_data)))
  expect_true(nrow(dfa_only) > 0)
  expect_setequal(unique(dfa_only[["Derived scalar"]]), "DFA alpha")
  expect_true(any(is.finite(dfa_only[["Derived scalar value"]])))
  expect_true(any(grepl("DFA alpha:", dfa_only$Tooltip, fixed = TRUE)))
  expect_true(nrow(higuchi_only) > 0)
  expect_setequal(unique(higuchi_only[["Derived scalar"]]), "Higuchi fractal dimension")
  expect_true(any(is.finite(higuchi_only[["Derived scalar value"]])))
  expect_true(any(grepl("Higuchi fractal dimension:", higuchi_only$Tooltip, fixed = TRUE)))
  expect_s3_class(plot, "ggplot")
  expect_true(grepl("DFA alpha", dfa_plot$labels$subtitle, fixed = TRUE))
})

test_that("complexity export separates scalar metrics and curve annotations", {
  data <- complexity_example_data()
  params <- complexity_default_parameters(min_points = 80, mse_scale_max = 2)
  quick <- compute_complexity_quick_metrics(data, params)
  hurst <- compute_complexity_hurst_metrics(data, params)
  metrics <- merge_complexity_hurst_results(quick, hurst, status = "complete")
  curves <- compute_complexity_dfa_higuchi_curves(data, params)
  export <- prepare_complexity_export(metrics, curves, data)
  scalar <- export[export$Output == "Scalar metric", , drop = FALSE]
  curve_rows <- export[export$Output == "Scale curve", , drop = FALSE]

  expect_true(all(c("Derived scalar", "Derived scalar value") %in% names(export)))
  expect_false(any(c("Sample entropy", "Approximate entropy", "DFA alpha", "Higuchi fractal dimension", "Multiscale sample entropy") %in% scalar$Metric))
  expect_true(all(c("Shannon entropy", "Hurst exponent") %in% scalar$Metric))
  expect_true(any(curve_rows$`Derived scalar` == "DFA alpha"))
  expect_true(any(curve_rows$`Derived scalar` == "Higuchi fractal dimension"))
  expect_true(any(is.finite(curve_rows$`Derived scalar value`)))
})


test_that("complexity visual modes and heights are exposed", {
  choices <- complexity_visual_mode_choices()

  expect_equal(unname(choices), c("metric_summary", "scale_curves"))
  expect_true(all(c("Metric summary", "Scale curves") %in% names(choices)))
  expect_gt(complexity_visual_plot_height("metric_summary"), complexity_visual_plot_height("scale_curves"))
  expect_equal(complexity_visual_plot_height("scale_curves"), 500L)
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

test_that("complexity display can force selected Subject ID labels", {
  data <- data.frame(
    id = "FallbackA",
    id_source = subject_id_source_filename(),
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00") + seq(0, by = 300, length.out = 120),
    glucose = 100 + sin(seq_len(120) / 8),
    stringsAsFactors = FALSE
  )
  params <- complexity_default_parameters(min_points = 100, mse_scale_max = 2)
  key <- complexity_compute_key(data, params)
  results <- compute_complexity_metrics(data, min_points = 100)
  curves <- data.frame(
    id = "FallbackA",
    curve_metric = "dfa",
    scale_variable = "scale",
    scale_value = 4,
    metric_value = 1.2,
    value_label = "Fluctuation",
    derived_scalar_label = "DFA alpha",
    derived_scalar_value = 0.8,
    note = "",
    stringsAsFactors = FALSE
  )

  metrics_display <- prepare_complexity_metrics_display(results, data, show_subject_id = TRUE)
  plot_data <- prepare_complexity_plot_data(results, data, show_subject_id = TRUE)
  curve_data <- prepare_complexity_curve_plot_data(curves, data, show_subject_id = TRUE)
  export <- prepare_complexity_export(results, curves, data, show_subject_id = TRUE)

  expect_true("Subject ID" %in% names(metrics_display))
  expect_equal(unique(as.character(metrics_display$`Subject ID`)), "FallbackA")
  expect_equal(unique(as.character(plot_data$`Subject ID`)), "FallbackA")
  expect_false(any(grepl("Analysis data", plot_data$Tooltip, fixed = TRUE)))
  expect_equal(unique(as.character(curve_data$`Subject ID`)), "FallbackA")
  expect_false(any(grepl("Analysis data", curve_data$Tooltip, fixed = TRUE)))
  expect_true(grepl("FallbackA=0.8", complexity_curve_annotation_text(curve_data), fixed = TRUE))
  expect_equal(unique(as.character(export$`Subject ID`)), "FallbackA")
  expect_identical(key, complexity_compute_key(data, params))
})

test_that("complexity summary cards report eligibility and parameters", {
  data <- complexity_example_data()
  params <- complexity_default_parameters(min_points = 80, entropy_bin_width = 10, embedding_dimension = 2)
  cards <- complexity_summary_cards(compute_complexity_metrics(data, min_points = 80), params)

  expect_equal(cards$Value[cards$Label == "Eligible Subject IDs"], "5")
  expect_equal(cards$Value[cards$Label == "Needs review"], "0")
  expect_true(grepl("m=2", cards$Value[cards$Label == "Parameters"], fixed = TRUE))
  expect_true(grepl("MSE scales 1-5", cards$Value[cards$Label == "Parameters"], fixed = TRUE))
  expect_true(grepl("Higuchi kmax=8", cards$Value[cards$Label == "Parameters"], fixed = TRUE))
})

test_that("complexity module stages fast results before MSE curves", {
  params <- complexity_default_parameters(min_points = 100)
  pending <- compute_complexity_pending_summary(complexity_example_data(), params, status = "running")
  quick <- compute_complexity_quick_metrics(complexity_example_data(), params)
  merged <- merge_complexity_hurst_results(quick, NULL, status = "running")

  expect_true(all(c("shannon_entropy", "hurst_exponent") %in% names(merged)))
  expect_true(any(grepl("Hurst exponent is calculating", merged$hurst_exponent_note, fixed = TRUE)))
  expect_true(any(grepl("Pending background MSE calculation", pending$multiscale_sample_entropy_note, fixed = TRUE)))
  expect_equal(complexity_metric_catalog()$raw_name, c("shannon_entropy", "hurst_exponent"))
  expect_true(grepl("MSE running", complexity_mse_status_text("running"), fixed = TRUE))
  expect_true(grepl("DFA/Higuchi curves running", complexity_curve_status_text("running"), fixed = TRUE))
})
