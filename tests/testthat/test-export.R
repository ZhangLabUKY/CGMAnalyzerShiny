test_that("export payload helpers produce user-facing data products", {
  data <- data.frame(
    id = c("A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(100, 110, NA),
    units = "mg/dL",
    device = NA_character_,
    group = c("Control", "Control", "Treatment"),
    source_file = c("A.csv", "A.csv", "B.csv"),
    imputed_flag = c(FALSE, FALSE, TRUE),
    id_source = subject_id_source_mapped(),
    stringsAsFactors = FALSE
  )
  thresholds <- default_cgm_thresholds()

  data_export <- prepare_cgm_data_export(data)
  metric_export <- prepare_metrics_display(
    compute_base_core_metrics(data, thresholds = thresholds),
    thresholds = thresholds
  )
  qc_export <- prepare_qc_display(
    compute_qc_summary(data, valid_day_hours = 1),
    data
  )

  expect_type(data_export$timestamp, "character")
  expect_true(all(grepl("^2026-05-0[56]T", data_export$timestamp)))
  expect_true("imputed_flag" %in% names(data_export))

  expect_true(all(
    c("Subject ID", "Period", "Metric", "Value", "Units", "Definition") %in%
      names(metric_export)
  ))
  expect_false("metric_engine" %in% names(metric_export))
  expect_true("Mean glucose" %in% metric_export$Metric)
  expect_true("All" %in% metric_export$Period)

  expect_true(all(
    c("Subject ID", "QC status", "Review notes", "Missing glucose") %in%
      names(qc_export)
  ))
  expect_false(any(grepl("_", names(qc_export), fixed = TRUE)))
})

test_that("fwrite export path preserves user-facing CSV columns", {
  data <- data.frame(
    id = "A",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    units = "mg/dL",
    device = NA_character_,
    source_file = "A.csv",
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )
  file <- tempfile(fileext = ".csv")

  data.table::fwrite(prepare_cgm_data_export(data), file)
  out <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)

  expect_equal(names(out), names(prepare_cgm_data_export(data)))
  expect_equal(out$timestamp, "2026-05-05T08:00:00")
  expect_false("row.names" %in% names(out))
})

test_that("metrics export includes selected-on-demand cache coverage", {
  data <- data.frame(
    id = c("A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(100, 110, 120),
    group = c("Control", "Control", "Treatment"),
    stringsAsFactors = FALSE
  )
  metrics <- compute_base_core_metrics(data[data$id == "A", , drop = FALSE])

  export <- prepare_metrics_cached_export(metrics, data, thresholds = default_cgm_thresholds())
  status <- export[export$Metric == "Metrics cache status", , drop = FALSE]

  expect_true(nrow(export) > nrow(prepare_metrics_display(metrics, show_subject_id = TRUE)))
  expect_equal(status$Value[status$`Subject ID` == "A"], "Generated")
  expect_equal(status$Value[status$`Subject ID` == "B"], "Not generated")
})

test_that("complete metrics export computes all Subject IDs", {
  data <- data.frame(
    id = c("A", "A", "B", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00",
      "2026-05-06 08:05:00"
    )),
    glucose = c(100, 110, 120, 130),
    group = c("Control", "Control", "Treatment", "Treatment"),
    stringsAsFactors = FALSE
  )

  export <- prepare_complete_metrics_export(data, thresholds = default_cgm_thresholds())

  expect_setequal(unique(export$`Subject ID`), c("A", "B"))
  expect_false("Metrics cache status" %in% export$Metric)
})

export_complexity_test_data <- function(points_per_subject = 36) {
  timestamps <- as.POSIXct("2026-05-05 00:00:00", tz = "UTC") +
    seq(0, by = 300, length.out = points_per_subject)
  data.frame(
    id = rep(c("Subject A", "Subject B"), each = points_per_subject),
    timestamp = rep(timestamps, 2),
    glucose = c(
      105 + sin(seq_len(points_per_subject) / 3) * 12 + seq_len(points_per_subject) %% 7,
      125 + cos(seq_len(points_per_subject) / 4) * 10 + seq_len(points_per_subject) %% 5
    ),
    group = rep(c("Control", "Treatment"), each = points_per_subject),
    stringsAsFactors = FALSE
  )
}

export_predictive_test_data <- function(ids = c("A", "B"), days = 3L, interval_minutes = 5L) {
  rows <- list()
  for (id in ids) {
    for (day in seq_len(days)) {
      timestamp <- as.POSIXct("2026-06-01 00:00:00", tz = "UTC") +
        (day - 1L) * 86400 +
        seq(0, by = interval_minutes * 60, length.out = 1440 / interval_minutes)
      phase <- switch(id, A = 0, B = pi / 5, C = pi / 3, 0)
      offset <- switch(id, A = 0, B = 18, C = -12, 0)
      glucose <- 130 + offset + 65 * sin(seq_along(timestamp) / length(timestamp) * 4 * pi + phase)
      rows[[length(rows) + 1L]] <- data.frame(
        id = id,
        timestamp = timestamp,
        glucose = glucose,
        imputed_flag = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

test_that("complete complexity export computes all Subject IDs with full-dataset basis", {
  data <- export_complexity_test_data()
  params <- complexity_default_parameters(min_points = 20, mse_scale_max = 2)

  export <- prepare_complete_complexity_export(data, parameters = params)

  expect_setequal(unique(export$`Subject ID`), c("Subject A", "Subject B"))
  expect_true("Time basis" %in% names(export))
  expect_true(all(export$`Time basis` == "Full dataset"))
  expect_false("Complexity cache status" %in% export$Metric)
})

test_that("complexity export includes selected-on-demand cache coverage", {
  data <- data.frame(
    id = c("A", "A", "B", "B"),
    timestamp = parse_cgm_timestamp(rep(c("2026-05-05 08:00:00", "2026-05-05 08:05:00"), 2)),
    glucose = c(100, 110, 120, 130),
    group = rep(c("Control", "Treatment"), each = 2),
    stringsAsFactors = FALSE
  )
  params <- complexity_default_parameters(min_points = 2)
  results <- compute_complexity_quick_metrics(data[data$id == "A", , drop = FALSE], params)

  export <- prepare_complexity_cached_export(
    results,
    empty_complexity_curve_rows(),
    data,
    generated_ids = "A",
    show_subject_id = TRUE
  )
  status <- export[export$Metric == "Complexity cache status", , drop = FALSE]

  expect_equal(status$Value[status$`Subject ID` == "A"], "Generated")
  expect_equal(status$Value[status$`Subject ID` == "B"], "Not generated")
  expect_true(all(c("Output", "Derived scalar", "Derived scalar value") %in% names(export)))
})

test_that("imputed export availability requires complete imputation", {
  expect_true(imputed_export_available(list(state = "complete")))
  expect_false(imputed_export_available(list(state = "running")))
  expect_false(imputed_export_available(list(state = "stale")))
  expect_false(imputed_export_available(NULL))
})

test_that("export plot formats normalize to supported choices", {
  expect_equal(normalize_export_plot_format(NULL), "png")
  expect_equal(normalize_export_plot_format(c("bad", "PDF", "jpg", "tif")), "pdf")
  expect_equal(normalize_export_plot_formats("tif"), "tiff")
})

test_that("per-subject plot manifest includes Subject IDs and no all-subject files", {
  data <- data.frame(
    id = rep(c("Subject A", "Subject B"), each = 2),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00",
      "2026-05-06 08:05:00"
    )),
    glucose = c(100, 110, 120, 130),
    stringsAsFactors = FALSE
  )

  manifest <- export_plot_manifest(data, formats = "png")

  expect_equal(nrow(manifest), 6L)
  expect_true(all(manifest$format == "png"))
  expect_true(all(grepl("subject-subject-", manifest$filename, fixed = TRUE)))
  expect_false(any(grepl("all-subjects", manifest$filename, fixed = TRUE)))
  expect_true(any(grepl("daily-overlay_all-days", manifest$filename, fixed = TRUE)))
})

test_that("plot bundle writes PDF-only and single image-format downloads", {
  data <- data.frame(
    id = rep(c("A", "B"), each = 3),
    timestamp = parse_cgm_timestamp(rep(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-05 08:10:00"
    ), 2)),
    glucose = c(100, 110, 115, 120, 130, 125),
    stringsAsFactors = FALSE
  )
  pdf_file <- tempfile(fileext = ".pdf")
  zip_file <- tempfile(fileext = ".zip")

  write_export_plot_bundle(data, pdf_file, formats = "pdf")
  write_export_plot_bundle(data, zip_file, formats = "png")

  expect_true(file.exists(pdf_file))
  expect_gt(file.info(pdf_file)$size, 0)
  zip_listing <- utils::unzip(zip_file, list = TRUE)
  expect_false(export_plots_pdf_filename() %in% zip_listing$Name)
  expect_true(any(grepl("subject-a_trace_full-day.png", zip_listing$Name, fixed = TRUE)))
  expect_false(any(grepl("all-subjects", zip_listing$Name, fixed = TRUE)))
})

test_that("complexity plot manifest includes Subject IDs without time-window labels", {
  data <- export_complexity_test_data(points_per_subject = 24)

  manifest <- export_complexity_plot_manifest(data, format = "png")

  expect_equal(nrow(manifest), 4L)
  expect_true(all(manifest$format == "png"))
  expect_true(all(grepl("subject-subject-", manifest$filename, fixed = TRUE)))
  expect_true(any(grepl("complexity-summary.png", manifest$filename, fixed = TRUE)))
  expect_true(any(grepl("complexity-scale-curves.png", manifest$filename, fixed = TRUE)))
  expect_false(any(grepl("full-day", manifest$filename, fixed = TRUE)))
  expect_false(any(grepl("all-subjects", manifest$filename, fixed = TRUE)))
})

test_that("complexity plot bundle writes PDF and single image-format downloads", {
  data <- export_complexity_test_data(points_per_subject = 24)
  params <- complexity_default_parameters(min_points = 20, mse_scale_max = 2)
  pdf_file <- tempfile(fileext = ".pdf")
  zip_file <- tempfile(fileext = ".zip")

  write_export_complexity_plot_bundle(data, pdf_file, format = "pdf", parameters = params)
  write_export_complexity_plot_bundle(data, zip_file, format = "png", parameters = params)

  expect_true(file.exists(pdf_file))
  expect_gt(file.info(pdf_file)$size, 0)
  zip_listing <- utils::unzip(zip_file, list = TRUE)
  expect_true(any(grepl("subject-subject-a_complexity-summary.png", zip_listing$Name, fixed = TRUE)))
  expect_true(any(grepl("subject-subject-b_complexity-scale-curves.png", zip_listing$Name, fixed = TRUE)))
  expect_false(any(grepl("full-day", zip_listing$Name, fixed = TRUE)))
  expect_false(any(grepl("all-subjects", zip_listing$Name, fixed = TRUE)))
})

test_that("predictive plot manifest includes Subject IDs, settings, and plot types", {
  data <- export_predictive_test_data(ids = c("Subject A", "Subject B"), days = 2L)
  params <- predictive_default_parameters(target = "high", model = "glm", horizon_minutes = 30)

  manifest <- export_predictive_plot_manifest(data, format = "png", parameters = params)

  expect_equal(nrow(manifest), 4L)
  expect_true(all(manifest$format == "png"))
  expect_true(any(grepl("predictive-risk_above-range_30-min_glm.png", manifest$filename, fixed = TRUE)))
  expect_true(any(grepl("predictive-feature-importance_above-range_30-min_glm.png", manifest$filename, fixed = TRUE)))
  expect_true(all(grepl("subject-subject-", manifest$filename, fixed = TRUE)))
  expect_false(any(grepl("all-subjects", manifest$filename, fixed = TRUE)))
})

test_that("predictive plot bundle writes PDF and single image-format downloads", {
  data <- export_predictive_test_data(ids = c("A", "B"), days = 3L)
  params <- predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3)
  pdf_file <- tempfile(fileext = ".pdf")
  zip_file <- tempfile(fileext = ".zip")

  write_export_predictive_plot_bundle(data, pdf_file, format = "pdf", parameters = params, thresholds = default_cgm_thresholds())
  write_export_predictive_plot_bundle(data, zip_file, format = "png", parameters = params, thresholds = default_cgm_thresholds())

  expect_true(file.exists(pdf_file))
  expect_gt(file.info(pdf_file)$size, 0)
  zip_listing <- utils::unzip(zip_file, list = TRUE)
  expect_true(any(grepl("subject-a_predictive-risk_above-range_30-min_glm.png", zip_listing$Name, fixed = TRUE)))
  expect_true(any(grepl("subject-b_predictive-feature-importance_above-range_30-min_glm.png", zip_listing$Name, fixed = TRUE)))
  expect_false(any(grepl("all-subjects", zip_listing$Name, fixed = TRUE)))
})

test_that("predictive export plots include legends and status pages", {
  data <- export_predictive_test_data(ids = c("A"), days = 3L)
  ready <- compute_complete_predictive_risk_bundle(
    data,
    predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )
  risk_plot <- export_predictive_plot_for_subject(ready, "A", "predictive_risk")

  expect_equal(risk_plot$theme$legend.position, "bottom")
  expect_silent(ggplot2::ggplot_build(risk_plot))

  flat <- data
  flat$glucose <- 120
  not_ready <- compute_complete_predictive_risk_bundle(
    flat,
    predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )
  status_plot <- export_predictive_plot_for_subject(not_ready, "A", "predictive_risk")
  expect_s3_class(status_plot, "ggplot")
  expect_silent(ggplot2::ggplot_build(status_plot))
})
