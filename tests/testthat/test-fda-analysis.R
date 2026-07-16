functional_test_data <- function(ids = c("A", "B"), days = 2L, interval_minutes = 5L) {
  rows <- list()
  for (id in ids) {
    for (day in seq_len(days)) {
      timestamp <- as.POSIXct("2026-06-01 00:00:00", tz = "UTC") +
        (day - 1L) * 86400 +
        seq(0, by = interval_minutes * 60, length.out = 1440 / interval_minutes)
      phase <- if (identical(id, "A")) 0 else pi / 4
      glucose <- 115 + 25 * sin(seq_along(timestamp) / length(timestamp) * 2 * pi + phase) +
        if (identical(id, "B")) 12 else 0
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

test_that("functional curve gridding builds subject-day matrices", {
  data <- functional_test_data(ids = c("A"), days = 2L)
  rows <- functional_curve_rows(data, functional_default_parameters(grid_minutes = 10))

  expect_equal(length(rows$grid), 144)
  expect_equal(nrow(rows$metadata), 2)
  expect_equal(dim(rows$matrix), c(2, 144))
  expect_true(all(is.finite(rows$matrix)))
})

test_that("functional subject selection helpers avoid all-subject defaults", {
  ids <- c("A", "B", "C")

  expect_equal(functional_profile_subject_selection(ids, NULL), "A")
  expect_equal(functional_profile_subject_selection(ids, "B"), "B")
  expect_equal(functional_profile_subject_selection(ids, "__all__"), "A")
  expect_equal(functional_comparison_subject_selection(ids, NULL), c("A", "B"))
  expect_equal(functional_comparison_subject_selection(ids, c("C", "A")), c("C", "A"))
  expect_equal(functional_comparison_subject_selection(ids, c("Missing", "B")), "B")

  data <- functional_test_data(ids = ids, days = 1L)
  filtered <- filter_functional_data_by_subjects(data, c("B", "C"))
  expect_equal(sort(unique(filtered$id)), c("B", "C"))
  expect_equal(nrow(filter_functional_data_by_subjects(data, character())), 0)
})

test_that("functional smoothing returns derivatives and engine status", {
  data <- functional_test_data(ids = c("A"), days = 1L)
  params <- functional_default_parameters(grid_minutes = 5, min_observed_points = 20)
  curves <- compute_functional_day_curves(data, params)

  expect_equal(nrow(curves), 288)
  expect_true(any(is.finite(curves$smoothed_glucose)))
  expect_true(any(is.finite(curves$rate_mg_dl_per_hour)))
  expect_true(all(curves$smoothing_status %in% c("complete", "fallback")))
  expect_true(all(curves$smoothing_engine %in% c("fda", "stats_fallback")))
})

test_that("functional feature extraction summarizes peak, range, rates, and provenance", {
  data <- functional_test_data(ids = c("A"), days = 2L)
  data$imputed_flag[1:10] <- TRUE
  params <- functional_default_parameters(grid_minutes = 5, min_observed_points = 20)
  bundle <- compute_functional_profile_bundle(data, params)

  expect_equal(nrow(bundle$features), 1)
  expect_equal(bundle$features$id, "A")
  expect_equal(bundle$features$days, 2)
  expect_match(bundle$features$peak_time, "^[0-9]{2}:[0-9]{2}$")
  expect_match(bundle$features$nadir_time, "^[0-9]{2}:[0-9]{2}$")
  expect_true(is.finite(bundle$features$area_above_range_mg_dl_hour))
  expect_true(is.finite(bundle$features$max_rise_rate_mg_dl_hour))
  expect_equal(bundle$features$profile_basis, "Imputed analysis data")
})

test_that("functional scores and phenotype groups remain available without fdapace", {
  skip_if(functional_fdapace_available(), "fdapace is installed; fallback-specific test not applicable.")
  data <- functional_test_data(ids = c("A", "B"), days = 1L)
  params <- functional_default_parameters(grid_minutes = 10, min_observed_points = 20, cluster_count = 2)
  bundle <- compute_functional_profile_bundle(data, params)

  expect_equal(nrow(bundle$features), 2)
  expect_true(all(is.finite(bundle$features$fpca1)))
  expect_true(all(grepl("prcomp_fallback", bundle$features$fpca_engine, fixed = TRUE)))
  expect_true(all(nzchar(bundle$features$phenotype_group)))
})

test_that("functional exports expose feature and curve payloads", {
  data <- functional_test_data(ids = c("A", "B"), days = 1L)
  params <- functional_default_parameters(grid_minutes = 15, min_observed_points = 20)
  bundle <- compute_functional_profile_bundle(data, params)
  features <- prepare_functional_features_export(bundle, data = data)
  curves <- prepare_functional_curves_export(bundle)

  expect_true(all(c("Subject ID", "Peak time", "Phenotype group") %in% names(features)))
  expect_true(all(c("Subject ID", "Time", "Mean smoothed glucose", "Grid minutes") %in% names(curves)))
  expect_equal(unique(curves$`Grid minutes`), 15)
})

test_that("functional plots return ggplot objects for available outputs", {
  data <- functional_test_data(ids = c("A", "B"), days = 1L)
  bundle <- compute_functional_profile_bundle(
    data,
    functional_default_parameters(grid_minutes = 15, min_observed_points = 20)
  )

  expect_s3_class(create_functional_profile_plot(bundle$subject_curves, bundle$day_curves), "ggplot")
  expect_s3_class(create_functional_rate_plot(bundle$subject_curves), "ggplot")
  expect_s3_class(create_functional_score_plot(bundle$features), "ggplot")
  expect_s3_class(create_functional_phenotype_plot(bundle$subject_curves), "ggplot")
})

test_that("implausible smoothed glucose values are invalidated before features", {
  params <- functional_default_parameters(plausible_min_glucose = 0, plausible_max_glucose = 500)
  result <- data.frame(
    time_minutes = c(0, 5, 10),
    smoothed_glucose = c(100, 900, 105),
    rate_mg_dl_per_hour = c(0, 10000, -10000),
    acceleration_mg_dl_per_hour2 = c(0, 1, -1),
    smoothing_engine = "fda",
    smoothing_status = "complete",
    notes = "",
    stringsAsFactors = FALSE
  )

  checked <- validate_functional_smoothing(result, params)

  expect_true(all(is.na(checked$smoothed_glucose)))
  expect_true(all(is.na(checked$rate_mg_dl_per_hour)))
  expect_equal(unique(checked$smoothing_status), "invalid")
  expect_true(any(grepl("implausible values", checked$notes, fixed = TRUE)))
})

test_that("smoothing exclusions are summarized without long compact-table notes", {
  params <- functional_default_parameters(grid_minutes = 15, min_observed_points = 4)
  valid <- data.frame(
    id = "A",
    date = "2026-06-01",
    time_minutes = functional_time_grid(15),
    raw_glucose = 120,
    smoothed_glucose = 120,
    rate_mg_dl_per_hour = 0,
    acceleration_mg_dl_per_hour2 = 0,
    observed_points = 96,
    imputed_points = 0,
    smoothing_engine = "fda",
    smoothing_status = "complete",
    notes = "",
    stringsAsFactors = FALSE
  )
  invalid <- valid
  invalid$date <- "2026-06-02"
  invalid$smoothed_glucose <- NA_real_
  invalid$rate_mg_dl_per_hour <- NA_real_
  invalid$acceleration_mg_dl_per_hour2 <- NA_real_
  invalid$smoothing_status <- "invalid"
  invalid$notes <- "Some FDA-smoothed day profiles were excluded because smoothing produced implausible values outside 0-500 mg/dL. Try B-spline basis, fewer basis functions, or stronger smoothing."
  day_curves <- rbind(valid, invalid)
  subject_curves <- compute_functional_subject_curves(day_curves)
  features <- compute_functional_subject_features(subject_curves, day_curves, parameters = params)
  bundle <- list(features = features, day_curves = day_curves)
  display <- prepare_functional_key_features_display(features)

  expect_equal(features$valid_fda_days, 1)
  expect_equal(features$excluded_fda_days, 1)
  expect_match(functional_smoothing_warning_text(bundle), "1 of 2 FDA day profile")
  expect_equal(display$`Valid FDA days`, 1)
  expect_equal(display$`Excluded FDA days`, 1)
  expect_false("Notes" %in% names(display))
  expect_true("Notes" %in% names(prepare_functional_features_export(bundle)))
})

test_that("single-profile FPCA and phenotype visuals are unavailable without full plot clutter", {
  data <- functional_test_data(ids = c("A"), days = 1L)
  bundle <- compute_functional_profile_bundle(
    data,
    functional_default_parameters(grid_minutes = 15, min_observed_points = 20)
  )

  expect_true(functional_visual_available(bundle, "profile"))
  expect_true(functional_visual_available(bundle, "rate"))
  expect_false(functional_visual_available(bundle, "scores"))
  expect_false(functional_visual_available(bundle, "phenotypes"))
  expect_match(functional_unavailable_visual_message(bundle, "scores"), "At least two")
  expect_match(functional_unavailable_visual_message(bundle, "phenotypes"), "At least two")
})

test_that("multi-profile FDA outputs enable FPCA and phenotype visuals when scores are finite", {
  data <- functional_test_data(ids = c("A", "B", "C"), days = 1L)
  data$glucose[data$id == "C"] <- data$glucose[data$id == "C"] + 20
  bundle <- compute_functional_profile_bundle(
    data,
    functional_default_parameters(grid_minutes = 15, min_observed_points = 20, cluster_count = 2)
  )

  expect_true(functional_visual_available(bundle, "phenotypes"))
  expect_true(functional_visual_available(bundle, "scores"))
})

test_that("functional plots carry readable hover text and clean labels", {
  data <- functional_test_data(ids = c("A", "B"), days = 1L)
  bundle <- compute_functional_profile_bundle(
    data,
    functional_default_parameters(grid_minutes = 15, min_observed_points = 20, cluster_count = 2)
  )

  profile_plot <- create_functional_profile_plot(bundle$subject_curves, bundle$day_curves, subject = "A")
  rate_plot <- create_functional_rate_plot(bundle$subject_curves, subject = "A")
  score_plot <- create_functional_score_plot(bundle$features)
  phenotype_plot <- create_functional_phenotype_plot(bundle$subject_curves)

  expect_null(profile_plot$labels$fill)
  expect_null(phenotype_plot$labels$fill)
  expect_equal(score_plot$labels$colour %||% score_plot$labels$color, "Subject ID")
  expect_true(any(grepl("Individual valid day profile", profile_plot$layers[[1]]$data$hover, fixed = TRUE)))
  expect_true(any(grepl("Day-to-day variability band", profile_plot$layers[[2]]$data$band_hover, fixed = TRUE)))
  expect_true(any(grepl("Mean smoothed profile", profile_plot$layers[[3]]$data$mean_hover, fixed = TRUE)))
  expect_true(any(grepl("Mean smoothed rate of change", rate_plot$data$rate_hover, fixed = TRUE)))
  expect_true(any(grepl("Glycemic curve-shape score", score_plot$data$score_hover, fixed = TRUE)))
  expect_true(any(grepl("Phenotype group profile", phenotype_plot$data$phenotype_hover, fixed = TRUE)))
})

test_that("functional plot guide labels match visible marks", {
  profile_guide <- paste(as.character(functional_plot_guide_ui("profile")), collapse = "")
  scores_guide <- paste(as.character(functional_plot_guide_ui("scores")), collapse = "")

  expect_false(grepl("Pale lines", profile_guide, fixed = TRUE))
  expect_true(grepl("Individual day profiles", profile_guide, fixed = TRUE))
  expect_true(grepl("Point color = Subject ID", scores_guide, fixed = TRUE))
})

test_that("glycemic pattern filenames use user-facing names", {
  expect_equal(glycemic_pattern_features_filename(), "cgm_glycemic_pattern_features.csv")
  expect_equal(glycemic_pattern_curves_filename(), "cgm_glycemic_pattern_curves.csv")
})

test_that("phenotype plotly conversion has visible clean phenotype traces", {
  data <- functional_test_data(ids = c("A", "B", "C"), days = 2L)
  data$glucose[data$id == "B"] <- data$glucose[data$id == "B"] + 35
  data$glucose[data$id == "C"] <- data$glucose[data$id == "C"] - 20
  bundle <- compute_functional_profile_bundle(
    data,
    functional_default_parameters(grid_minutes = 15, min_observed_points = 20, cluster_count = 2)
  )
  plot <- create_functional_phenotype_plot(bundle$subject_curves)
  converted <- NULL

  expect_silent(converted <- clean_functional_plotly_legend(plotly::ggplotly(plot, tooltip = "text")))
  trace_names <- clean_filter_values(vapply(converted$x$data, function(trace) trace$name %||% "", character(1)))
  visible_y <- sum(vapply(converted$x$data, function(trace) {
    if (is.null(trace$y)) {
      return(0L)
    }
    sum(is.finite(unlist(trace$y)))
  }, integer(1)))

  expect_gt(visible_y, 0)
  expect_true(any(trace_names %in% c("Phenotype 1", "Phenotype 2")))
  expect_false(any(grepl("^\\(", trace_names)))
})

test_that("display table rounds values while export keeps full precision", {
  data <- functional_test_data(ids = c("A"), days = 1L)
  bundle <- compute_functional_profile_bundle(
    data,
    functional_default_parameters(grid_minutes = 15, min_observed_points = 20)
  )
  display <- prepare_functional_key_features_display(bundle$features, data = data)
  export <- prepare_functional_features_export(bundle, data = data)

  expect_true("Peak glucose" %in% names(display))
  expect_true("Peak glucose" %in% names(export))
  expect_false("Notes" %in% names(display))
  expect_true("Notes" %in% names(export))
  expect_equal(display$`Peak glucose`, round(export$`Peak glucose`, 2))
  expect_true(length(names(display)) < length(names(export)))
})
