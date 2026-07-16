predictive_test_data <- function(ids = c("A", "B", "C"), days = 3L, interval_minutes = 5L) {
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

test_that("predictive feature generation creates lagged examples and future targets", {
  data <- predictive_test_data(ids = c("A"), days = 2L)
  params <- predictive_default_parameters(horizon_minutes = 30, target = "high", min_examples = 20)
  examples <- build_predictive_examples(data, params, default_cgm_thresholds())

  expect_gt(nrow(examples), 100)
  expect_true(all(c("lag_5_glucose", "lag_30_glucose", "window_30_mean", "window_60_slope") %in% names(examples)))
  expect_true(all(examples$timestamp <= max(data$timestamp) - 30 * 60))
  expect_true(all(examples$target %in% c(0L, 1L)))
  expect_true(any(examples$target == 1L))
  expect_true(any(examples$target == 0L))
})

test_that("predictive splits are leakage-safe by subject or chronological day", {
  multi <- predictive_split_examples(build_predictive_examples(
    predictive_test_data(ids = c("A", "B", "C"), days = 2L),
    predictive_default_parameters(target = "high", min_examples = 20),
    default_cgm_thresholds()
  ))
  train_ids <- unique(multi$id[multi$split == "train"])
  test_ids <- unique(multi$id[multi$split == "test"])
  expect_length(intersect(train_ids, test_ids), 0)
  expect_gt(length(test_ids), 0)

  single <- predictive_split_examples(build_predictive_examples(
    predictive_test_data(ids = c("A"), days = 4L),
    predictive_default_parameters(target = "high", min_examples = 20),
    default_cgm_thresholds()
  ))
  expect_true(max(single$date[single$split == "train"]) < min(single$date[single$split == "test"]))
})

test_that("predictive logistic model trains and returns performance and scores", {
  data <- filter_predictive_data_by_subject(predictive_test_data(ids = c("A", "B", "C"), days = 3L), "A")
  bundle <- compute_predictive_risk_bundle(
    data,
    predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )

  expect_equal(bundle$fit_status, "ready")
  expect_true(is.data.frame(bundle$scores))
  expect_true(any(is.finite(bundle$scores$risk_probability)))
  expect_true(is.data.frame(bundle$performance))
  expect_gt(bundle$performance$test_rows, 0)
  expect_true(is.data.frame(bundle$importance))
  expect_gt(nrow(bundle$importance), 0)
  expect_equal(bundle$subject_id, "A")
  expect_true(all(bundle$scores$id == "A"))
})

test_that("predictive random forest trains when ranger is available", {
  skip_if_not(predictive_ranger_available(), "ranger is not installed.")
  data <- filter_predictive_data_by_subject(predictive_test_data(ids = c("A", "B", "C"), days = 3L), "B")
  bundle <- compute_predictive_risk_bundle(
    data,
    predictive_default_parameters(target = "high", model = "ranger", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )

  expect_equal(bundle$fit_status, "ready")
  expect_true(any(is.finite(bundle$scores$risk_probability)))
  expect_gt(nrow(bundle$importance), 0)
  expect_equal(bundle$subject_id, "B")
})

test_that("predictive model reports insufficient event states", {
  data <- predictive_test_data(ids = c("A", "B"), days = 1L)
  data$glucose <- 120
  bundle <- compute_predictive_risk_bundle(
    data,
    predictive_default_parameters(target = "high", model = "glm", min_examples = 20, min_events = 2),
    default_cgm_thresholds()
  )

  expect_equal(bundle$fit_status, "insufficient_events")
  expect_match(bundle$fit_message, "events")
})

test_that("predictive displays and exports are user-facing", {
  data <- filter_predictive_data_by_subject(predictive_test_data(ids = c("A", "B", "C"), days = 3L), "A")
  bundle <- compute_predictive_risk_bundle(
    data,
    predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )
  scores <- prepare_predictive_scores_export(bundle)
  performance <- prepare_predictive_performance_export(bundle)
  importance <- prepare_predictive_feature_importance_export(bundle)

  expect_true(all(c("Subject ID", "Timestamp", "Risk probability", "Target", "Model", "Horizon") %in% names(scores)))
  expect_true(all(c("Model", "Target", "AUC", "Brier score") %in% names(performance)))
  expect_true(all(c("Feature", "Model input", "Importance") %in% names(importance)))
  expect_true("Subject ID" %in% names(performance))
  expect_equal(unique(scores$`Subject ID`), "A")
  expect_equal(unique(performance$`Subject ID`), "A")
  expect_equal(unique(importance$`Subject ID`), "A")
  expect_true("Current glucose" %in% importance$Feature)
  expect_true("glucose_current" %in% importance$`Model input`)
  expect_s3_class(create_predictive_risk_plot(bundle$scores, subject = "A", target_label = bundle$target_label, threshold = 0.5), "ggplot")
  expect_s3_class(create_predictive_importance_plot(bundle$importance), "ggplot")
})

test_that("predictive feature labels are readable", {
  labels <- predictive_feature_label(c(
    "glucose_current",
    "lag_5_glucose",
    "window_30_mean",
    "window_60_sd",
    "window_15_slope",
    "time_cos"
  ))

  expect_equal(labels[[1L]], "Current glucose")
  expect_equal(labels[[2L]], "Glucose 5 min ago")
  expect_equal(labels[[3L]], "Mean glucose over previous 30 min")
  expect_equal(labels[[4L]], "Glucose variability over previous 60 min")
  expect_equal(labels[[5L]], "Glucose trend over previous 15 min")
  expect_equal(labels[[6L]], "Time of day pattern (cosine)")
})

test_that("predictive target choices use current glucose thresholds", {
  choices <- predictive_target_choices(list(tir_lower = 65, tir_upper = 175))

  expect_equal(unname(choices), c("low", "high"))
  expect_true("Below range risk (<65 mg/dL)" %in% names(choices))
  expect_true("Above range risk (>175 mg/dL)" %in% names(choices))
  expect_equal(predictive_event_label(predictive_default_parameters(target = "low")), "Observed future below-range event")
  expect_equal(predictive_event_label(predictive_default_parameters(target = "high")), "Observed future above-range event")
})

test_that("predictive risk date axis adapts to plotted date span", {
  timestamps_for_days <- function(days, start = "2026-01-01") {
    as.POSIXct(start, tz = "UTC") + seq(0, by = 86400, length.out = days)
  }

  one_day <- predictive_risk_date_axis(timestamps_for_days(1))
  seven_days <- predictive_risk_date_axis(timestamps_for_days(7))
  fourteen_days <- predictive_risk_date_axis(timestamps_for_days(14))
  thirty_days <- predictive_risk_date_axis(timestamps_for_days(30))
  sixty_days <- predictive_risk_date_axis(timestamps_for_days(60))
  cross_year <- predictive_risk_date_axis(timestamps_for_days(3, start = "2026-12-31"))

  expect_equal(one_day$interval, "day")
  expect_equal(length(one_day$breaks), 1L)
  expect_equal(seven_days$interval, "day")
  expect_equal(length(seven_days$breaks), 7L)
  expect_equal(fourteen_days$interval, "day")
  expect_equal(length(fourteen_days$breaks), 14L)
  expect_equal(thirty_days$interval, "2 days")
  expect_true(all(diff(as.Date(thirty_days$breaks)) == 2))
  expect_equal(sixty_days$interval, "week")
  expect_true(all(diff(as.Date(sixty_days$breaks)) == 7))
  expect_true(any(grepl("2027", cross_year$labels, fixed = TRUE)))
})

test_that("complete predictive exports compute full-dataset deliverables", {
  data <- predictive_test_data(ids = c("A", "B", "C"), days = 3L)
  params <- predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3)

  scores <- prepare_complete_predictive_scores_export(data, parameters = params, thresholds = default_cgm_thresholds())
  performance <- prepare_complete_predictive_performance_export(data, parameters = params, thresholds = default_cgm_thresholds())
  importance <- prepare_complete_predictive_feature_importance_export(data, parameters = params, thresholds = default_cgm_thresholds())

  expect_true(all(c("Subject ID", "Timestamp", "Risk probability", "Model") %in% names(scores)))
  expect_true(all(c("Model", "Target", "Test rows", "Brier score") %in% names(performance)))
  expect_true(all(c("Subject ID", "Feature", "Model input", "Importance") %in% names(importance)))
  expect_true(all(c("A", "B", "C") %in% unique(scores$`Subject ID`)))
  expect_true(all(c("A", "B", "C") %in% unique(performance$`Subject ID`)))
  expect_true(all(c("A", "B", "C") %in% unique(importance$`Subject ID`)))
  expect_true("Current glucose" %in% importance$Feature)
  expect_gt(nrow(scores), 100)
  expect_true(all(performance$`Test rows` > 0))
})

test_that("predictive subject helpers keep modeling patient-specific", {
  data <- predictive_test_data(ids = c("A", "B", "C"), days = 3L)
  choices <- predictive_subject_choices(subject_id_values(data))
  a <- compute_predictive_risk_bundle(
    filter_predictive_data_by_subject(data, "A"),
    predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )
  b <- compute_predictive_risk_bundle(
    filter_predictive_data_by_subject(data, "B"),
    predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )

  expect_false(all_filter_value() %in% unname(choices))
  expect_equal(unname(choices), c("A", "B", "C"))
  expect_equal(preserve_predictive_subject_selection("__all__", subject_id_values(data)), "A")
  expect_equal(a$subject_id, "A")
  expect_equal(b$subject_id, "B")
  expect_false(isTRUE(all.equal(a$scores$risk_probability, b$scores$risk_probability)))
})

test_that("predictive UI exposes controls without local download buttons", {
  html <- paste(as.character(predictive_risk_module_ui("predictive_risk")), collapse = "\n")

  expect_true(grepl("predictive_risk-subject_filter", html, fixed = TRUE))
  expect_true(grepl("predictive_risk-target", html, fixed = TRUE))
  expect_true(grepl("predictive_risk-model", html, fixed = TRUE))
  expect_false(grepl("predictive_risk-download_scores", html, fixed = TRUE))
  expect_false(grepl("predictive_risk-download_performance", html, fixed = TRUE))
  expect_false(grepl("predictive_risk-download_importance", html, fixed = TRUE))
  expect_false(grepl("predictive_risk-download_risk_plot", html, fixed = TRUE))
  expect_false(grepl("predictive_risk-download_importance_plot", html, fixed = TRUE))
  expect_true(grepl("Minimum usable timestamp rows", html, fixed = TRUE))
  expect_true(grepl("Minimum actual target events", html, fixed = TRUE))
  expect_true(grepl("Probability cutoff used for sensitivity and specificity", html, fixed = TRUE))
  expect_true(grepl("predictive_risk-risk_plot_guide", html, fixed = TRUE))
  guide <- paste(as.character(predictive_risk_plot_guide_ui("Observed future above-range event")), collapse = "\n")
  expect_true(grepl("Observed future above-range event", guide, fixed = TRUE))
  expect_true(grepl("Risk threshold", guide, fixed = TRUE))
})

test_that("predictive risk plot uses risk bands and threshold styling", {
  data <- predictive_test_data(ids = c("A", "B", "C"), days = 3L)
  bundle <- compute_predictive_risk_bundle(
    data,
    predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )
  plot <- create_predictive_risk_plot(bundle$scores, subject = "A", target_label = bundle$target_label, threshold = 0.4)

  expect_equal(plot$labels$colour, "Predicted risk")
  expect_equal(plot$labels$fill, "Risk band")
  expect_false(identical(plot$labels$colour, "Split"))
  expect_equal(plot$theme$legend.position, "none")
  expect_s3_class(plot$scales$get_scales("x"), "ScaleContinuousDatetime")
  expect_gte(length(plot$layers), 5)
  expect_silent(ggplot2::ggplot_build(plot))
  converted <- plotly::ggplotly(plot, tooltip = "text")
  expect_length(testthat::capture_warnings(plotly::ggplotly(plot, tooltip = "text")), 0)
  expect_length(testthat::capture_messages(plotly::ggplotly(plot, tooltip = "text")), 0)
  expect_false(any(vapply(converted$x$data, function(trace) isTRUE(trace$showlegend), logical(1))))

  export_plot <- create_predictive_risk_plot(
    bundle$scores,
    subject = "A",
    target_label = bundle$target_label,
    threshold = 0.4,
    event_label = predictive_event_label(bundle$parameters),
    show_legend = TRUE
  )
  expect_equal(export_plot$theme$legend.position, "bottom")
  expect_silent(ggplot2::ggplot_build(export_plot))

  long_data <- predictive_test_data(ids = c("A"), days = 10L)
  long_bundle <- compute_predictive_risk_bundle(
    long_data,
    predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )
  long_export_plot <- create_predictive_risk_plot(
    long_bundle$scores,
    subject = "A",
    target_label = long_bundle$target_label,
    threshold = 0.4,
    event_label = predictive_event_label(long_bundle$parameters),
    show_legend = TRUE
  )
  expect_equal(long_export_plot$theme$axis.text.x$angle, 35)
  expect_silent(ggplot2::ggplot_build(long_export_plot))
})

test_that("predictive plotly conversion runs without warnings", {
  data <- predictive_test_data(ids = c("A", "B", "C"), days = 3L)
  bundle <- compute_predictive_risk_bundle(
    data,
    predictive_default_parameters(target = "high", model = "glm", min_examples = 40, min_events = 3),
    default_cgm_thresholds()
  )

  risk_plot <- create_predictive_risk_plot(bundle$scores, subject = "A", target_label = bundle$target_label, threshold = 0.5)
  importance_plot <- create_predictive_importance_plot(bundle$importance)
  expect_length(testthat::capture_warnings(plotly::ggplotly(risk_plot, tooltip = "text")), 0)
  expect_length(testthat::capture_messages(plotly::ggplotly(risk_plot, tooltip = "text")), 0)
  expect_true(any(grepl("Current glucose|Mean glucose over previous", vapply(importance_plot$data$feature_label, as.character, character(1)))))
  expect_silent(plotly::ggplotly(importance_plot, tooltip = "text"))
})
