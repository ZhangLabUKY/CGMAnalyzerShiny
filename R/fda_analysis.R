functional_default_parameters <- function(
  grid_minutes = 5L,
  basis_type = "bspline",
  basis_count = 15L,
  smoothing_lambda = 1e-2,
  min_observed_points = 24L,
  min_days = 1L,
  fpca_components = 2L,
  cluster_count = 3L,
  plausible_min_glucose = 0,
  plausible_max_glucose = 500
) {
  list(
    grid_minutes = as.integer(grid_minutes),
    basis_type = as.character(basis_type),
    basis_count = as.integer(basis_count),
    smoothing_lambda = as.numeric(smoothing_lambda),
    min_observed_points = as.integer(min_observed_points),
    min_days = as.integer(min_days),
    fpca_components = as.integer(fpca_components),
    cluster_count = as.integer(cluster_count),
    plausible_min_glucose = as.numeric(plausible_min_glucose),
    plausible_max_glucose = as.numeric(plausible_max_glucose)
  )
}

normalize_functional_parameters <- function(
  parameters = functional_default_parameters()
) {
  defaults <- functional_default_parameters()
  parameters <- utils::modifyList(defaults, parameters %||% list())
  parameters$grid_minutes <- max(
    1L,
    as.integer(parameters$grid_minutes %||% defaults$grid_minutes)
  )
  parameters$basis_type <- normalize_functional_basis_type(
    parameters$basis_type %||% defaults$basis_type
  )
  parameters$basis_count <- max(
    4L,
    as.integer(parameters$basis_count %||% defaults$basis_count)
  )
  parameters$smoothing_lambda <- suppressWarnings(as.numeric(
    parameters$smoothing_lambda %||% defaults$smoothing_lambda
  ))
  if (is.na(parameters$smoothing_lambda) || parameters$smoothing_lambda < 0) {
    parameters$smoothing_lambda <- defaults$smoothing_lambda
  }
  parameters$min_observed_points <- max(
    4L,
    as.integer(parameters$min_observed_points %||% defaults$min_observed_points)
  )
  parameters$min_days <- max(
    1L,
    as.integer(parameters$min_days %||% defaults$min_days)
  )
  parameters$fpca_components <- max(
    1L,
    min(
      4L,
      as.integer(parameters$fpca_components %||% defaults$fpca_components)
    )
  )
  parameters$cluster_count <- max(
    1L,
    min(8L, as.integer(parameters$cluster_count %||% defaults$cluster_count))
  )
  parameters$plausible_min_glucose <- suppressWarnings(as.numeric(
    parameters$plausible_min_glucose %||% defaults$plausible_min_glucose
  ))
  parameters$plausible_max_glucose <- suppressWarnings(as.numeric(
    parameters$plausible_max_glucose %||% defaults$plausible_max_glucose
  ))
  if (
    is.na(parameters$plausible_min_glucose) ||
      is.na(parameters$plausible_max_glucose) ||
      parameters$plausible_min_glucose >= parameters$plausible_max_glucose
  ) {
    parameters$plausible_min_glucose <- defaults$plausible_min_glucose
    parameters$plausible_max_glucose <- defaults$plausible_max_glucose
  }
  parameters
}

functional_basis_choices <- function() {
  c("B-spline" = "bspline", "Fourier" = "fourier")
}

glycemic_pattern_features_filename <- function() {
  "cgm_glycemic_pattern_features.csv"
}

glycemic_pattern_curves_filename <- function() {
  "cgm_glycemic_pattern_curves.csv"
}

functional_profile_subject_selection <- function(values, selected = NULL) {
  ids <- clean_filter_values(values)
  selected <- normalize_filter_value(selected)
  if (nzchar(selected) && selected %in% ids) {
    return(selected)
  }
  ids[[1L]] %||% ""
}

functional_comparison_subject_selection <- function(
  values,
  selected = NULL,
  default_count = 2L
) {
  ids <- clean_filter_values(values)
  selected <- clean_filter_values(selected)
  selected <- selected[selected %in% ids]
  if (length(selected)) {
    return(unique(selected))
  }
  utils::head(ids, max(1L, as.integer(default_count %||% 2L)))
}

filter_functional_data_by_subjects <- function(data, subjects) {
  if (!is.data.frame(data) || !"id" %in% names(data)) {
    return(data.frame())
  }
  subjects <- clean_filter_values(subjects)
  if (!length(subjects)) {
    return(data[0, , drop = FALSE])
  }
  data[as.character(data$id) %in% subjects, , drop = FALSE]
}

normalize_functional_basis_type <- function(basis_type = "bspline") {
  basis_type <- tolower(trimws(as.character(basis_type[[1L]] %||% "bspline")))
  aliases <- c(
    b = "bspline",
    spline = "bspline",
    bspline = "bspline",
    b_spline = "bspline",
    fourier = "fourier"
  )
  aliases[[basis_type]] %||% "bspline"
}

functional_fda_available <- function() {
  requireNamespace("fda", quietly = TRUE)
}

functional_fdapace_available <- function() {
  requireNamespace("fdapace", quietly = TRUE)
}

functional_engine_status <- function() {
  data.frame(
    Engine = c("fda", "fdapace"),
    Role = c("basis smoothing and derivatives", "advanced FPCA scores"),
    Available = c(functional_fda_available(), functional_fdapace_available()),
    stringsAsFactors = FALSE
  )
}

functional_time_grid <- function(grid_minutes = 5L) {
  grid_minutes <- max(1L, as.integer(grid_minutes %||% 5L))
  seq(0, 1440 - grid_minutes, by = grid_minutes)
}

functional_minutes_label <- function(minutes) {
  minutes <- as.integer(round(minutes))
  hour <- floor(minutes / 60) %% 24
  minute <- minutes %% 60
  sprintf("%02d:%02d", hour, minute)
}

functional_profile_basis <- function(data) {
  if (!is.data.frame(data) || !"imputed_flag" %in% names(data)) {
    return("Observed analysis data")
  }
  if (any(data$imputed_flag %in% TRUE, na.rm = TRUE)) {
    "Imputed analysis data"
  } else {
    "Observed analysis data"
  }
}

functional_smoothing_engine_label <- function(engine) {
  labels <- c(
    fda = "FDA smoothing",
    stats_fallback = "Fallback smoothing"
  )
  engine <- clean_filter_values(engine)
  if (!length(engine)) {
    return("Not available")
  }
  paste(
    vapply(engine, function(value) labels[[value]] %||% value, character(1)),
    collapse = ", "
  )
}

functional_fpca_engine_label <- function(engine) {
  engine <- clean_filter_values(engine)
  if (!length(engine)) {
    return("Not run")
  }
  labels <- c(
    fdapace = "FPCA",
    `fdapace+prcomp_pc2` = "FPCA with PCA second score",
    fdapace_not_needed = "Not run: one profile",
    fdapace_unavailable = "Not available",
    prcomp_fallback_fdapace_unavailable = "Fallback PCA",
    prcomp_fallback_after_fdapace = "Fallback PCA",
    prcomp_fallback_after_fdapace_error = "Fallback PCA"
  )
  paste(
    vapply(
      engine,
      function(value) {
        hit <- names(labels)[vapply(
          names(labels),
          startsWith,
          logical(1),
          x = value
        )]
        if (length(hit)) labels[[hit[[1L]]]] else value
      },
      character(1)
    ),
    collapse = ", "
  )
}

functional_smoothing_status_label <- function(status) {
  labels <- c(
    complete = "Complete",
    fallback = "Fallback",
    ineligible = "Ineligible",
    invalid = "Invalid smoothing"
  )
  status <- clean_filter_values(status)
  if (!length(status)) {
    return("Not available")
  }
  paste(
    vapply(status, function(value) labels[[value]] %||% value, character(1)),
    collapse = ", "
  )
}

validate_functional_smoothing <- function(result, parameters) {
  if (!is.data.frame(result) || !"smoothed_glucose" %in% names(result)) {
    return(result)
  }
  values <- result$smoothed_glucose
  finite <- is.finite(values)
  if (!any(finite)) {
    return(result)
  }
  below <- min(values[finite], na.rm = TRUE) < parameters$plausible_min_glucose
  above <- max(values[finite], na.rm = TRUE) > parameters$plausible_max_glucose
  if (!isTRUE(below || above)) {
    return(result)
  }
  result$smoothed_glucose <- NA_real_
  result$rate_mg_dl_per_hour <- NA_real_
  result$acceleration_mg_dl_per_hour2 <- NA_real_
  result$smoothing_status <- "invalid"
  result$notes <- paste0(
    "Some FDA-smoothed day profiles were excluded because smoothing produced implausible values outside ",
    parameters$plausible_min_glucose,
    "-",
    parameters$plausible_max_glucose,
    " mg/dL. Try B-spline basis, fewer basis functions, or stronger smoothing."
  )
  result
}

empty_functional_day_curves <- function() {
  data.frame(
    id = character(),
    date = character(),
    time_minutes = numeric(),
    raw_glucose = numeric(),
    smoothed_glucose = numeric(),
    rate_mg_dl_per_hour = numeric(),
    acceleration_mg_dl_per_hour2 = numeric(),
    observed_points = integer(),
    imputed_points = integer(),
    smoothing_engine = character(),
    smoothing_status = character(),
    notes = character(),
    stringsAsFactors = FALSE
  )
}

empty_functional_subject_curves <- function() {
  data.frame(
    id = character(),
    time_minutes = numeric(),
    mean_glucose = numeric(),
    q25_glucose = numeric(),
    q75_glucose = numeric(),
    mean_rate_mg_dl_per_hour = numeric(),
    days = integer(),
    phenotype_group = character(),
    stringsAsFactors = FALSE
  )
}

empty_functional_features <- function() {
  data.frame(
    id = character(),
    days = integer(),
    valid_fda_days = integer(),
    excluded_fda_days = integer(),
    observed_points = integer(),
    imputed_points = integer(),
    profile_basis = character(),
    peak_time = character(),
    peak_glucose = numeric(),
    nadir_time = character(),
    nadir_glucose = numeric(),
    time_below_range_percent = numeric(),
    time_in_range_percent = numeric(),
    time_above_range_percent = numeric(),
    area_below_range_mg_dl_hour = numeric(),
    area_above_range_mg_dl_hour = numeric(),
    max_rise_rate_mg_dl_hour = numeric(),
    max_fall_rate_mg_dl_hour = numeric(),
    overnight_slope_mg_dl_hour = numeric(),
    morning_rise_mg_dl = numeric(),
    fpca1 = numeric(),
    fpca2 = numeric(),
    phenotype_group = character(),
    smoothing_engine = character(),
    fpca_engine = character(),
    notes = character(),
    stringsAsFactors = FALSE
  )
}

functional_curve_rows <- function(
  data,
  parameters = functional_default_parameters()
) {
  parameters <- normalize_functional_parameters(parameters)
  grid <- functional_time_grid(parameters$grid_minutes)
  if (
    !is.data.frame(data) ||
      !all(c("id", "timestamp", "glucose") %in% names(data))
  ) {
    return(list(
      grid = grid,
      metadata = data.frame(),
      matrix = matrix(numeric(), nrow = 0L, ncol = length(grid))
    ))
  }
  dt <- data.table::as.data.table(data)
  if (!"imputed_flag" %in% names(dt)) {
    dt[, imputed_flag := FALSE]
  }
  dt <- dt[
    !is.na(id) &
      nzchar(as.character(id)) &
      is_finite_cgm_timestamp(timestamp) &
      is.finite(as.numeric(glucose))
  ]
  if (!nrow(dt)) {
    return(list(
      grid = grid,
      metadata = data.frame(),
      matrix = matrix(numeric(), nrow = 0L, ncol = length(grid))
    ))
  }
  dt[, `:=`(
    id = as.character(id),
    date = as.character(as.Date(timestamp)),
    glucose = as.numeric(glucose),
    time_minutes = round(
      time_window_minutes(timestamp) / parameters$grid_minutes
    ) *
      parameters$grid_minutes,
    imputed_flag = imputed_flag %in% TRUE
  )]
  dt[time_minutes >= 1440, time_minutes := 1440 - parameters$grid_minutes]
  dt[time_minutes < 0, time_minutes := 0]

  agg <- dt[,
    list(
      raw_glucose = mean(glucose, na.rm = TRUE),
      observed_points = .N,
      imputed_points = sum(imputed_flag, na.rm = TRUE)
    ),
    by = .(id, date, time_minutes)
  ]
  data.table::setorder(agg, id, date, time_minutes)

  metadata <- agg[,
    list(
      observed_points = sum(observed_points, na.rm = TRUE),
      imputed_points = sum(imputed_points, na.rm = TRUE)
    ),
    by = .(id, date)
  ]
  metadata$row_key <- paste(metadata$id, metadata$date, sep = "\r")

  matrix_values <- matrix(NA_real_, nrow = nrow(metadata), ncol = length(grid))
  rownames(matrix_values) <- metadata$row_key
  colnames(matrix_values) <- as.character(grid)
  agg$row_key <- paste(agg$id, agg$date, sep = "\r")
  idx <- cbind(
    match(agg$row_key, rownames(matrix_values)),
    match(as.character(agg$time_minutes), colnames(matrix_values))
  )
  matrix_values[idx] <- agg$raw_glucose
  list(
    grid = grid,
    metadata = as.data.frame(metadata, stringsAsFactors = FALSE),
    matrix = matrix_values
  )
}

functional_create_basis <- function(
  rangeval,
  basis_type,
  basis_count,
  observed_count
) {
  basis_type <- normalize_functional_basis_type(basis_type)
  observed_count <- max(4L, as.integer(observed_count %||% basis_count))
  basis_count <- max(4L, min(as.integer(basis_count %||% 15L), observed_count))
  if (identical(basis_type, "fourier")) {
    if (basis_count %% 2L == 0L) {
      basis_count <- basis_count - 1L
    }
    basis_count <- max(3L, basis_count)
    return(fda::create.fourier.basis(rangeval = rangeval, nbasis = basis_count))
  }
  fda::create.bspline.basis(
    rangeval = rangeval,
    nbasis = max(4L, basis_count),
    norder = 4L
  )
}

functional_smooth_with_fda <- function(time, glucose, eval_time, parameters) {
  basis <- functional_create_basis(
    rangeval = c(0, 1440),
    basis_type = parameters$basis_type,
    basis_count = parameters$basis_count,
    observed_count = length(time)
  )
  fd_par <- fda::fdPar(basis, Lfdobj = 2, lambda = parameters$smoothing_lambda)
  smooth <- fda::smooth.basis(time, glucose, fd_par)
  data.frame(
    time_minutes = eval_time,
    smoothed_glucose = as.numeric(fda::eval.fd(
      eval_time,
      smooth$fd,
      Lfdobj = 0
    )),
    rate_mg_dl_per_hour = as.numeric(fda::eval.fd(
      eval_time,
      smooth$fd,
      Lfdobj = 1
    )) *
      60,
    acceleration_mg_dl_per_hour2 = as.numeric(fda::eval.fd(
      eval_time,
      smooth$fd,
      Lfdobj = 2
    )) *
      3600,
    smoothing_engine = "fda",
    smoothing_status = "complete",
    notes = "",
    stringsAsFactors = FALSE
  )
}

functional_fallback_derivative <- function(x, y) {
  if (length(x) < 2L) {
    return(rep(NA_real_, length(x)))
  }
  c(NA_real_, diff(y) / diff(x))
}

functional_smooth_fallback <- function(time, glucose, eval_time) {
  interpolated <- stats::approx(
    time,
    glucose,
    xout = eval_time,
    rule = 2,
    ties = "ordered"
  )$y
  smooth <- tryCatch(
    stats::smooth.spline(time, glucose),
    error = function(error) NULL
  )
  if (!is.null(smooth)) {
    smoothed <- as.numeric(stats::predict(smooth, eval_time, deriv = 0)$y)
    rate <- as.numeric(stats::predict(smooth, eval_time, deriv = 1)$y) * 60
    acceleration <- tryCatch(
      as.numeric(stats::predict(smooth, eval_time, deriv = 2)$y) * 3600,
      error = function(error) {
        functional_fallback_derivative(eval_time / 60, rate)
      }
    )
  } else {
    smoothed <- interpolated
    rate <- functional_fallback_derivative(eval_time / 60, smoothed)
    acceleration <- functional_fallback_derivative(eval_time / 60, rate)
  }
  data.frame(
    time_minutes = eval_time,
    smoothed_glucose = smoothed,
    rate_mg_dl_per_hour = rate,
    acceleration_mg_dl_per_hour2 = acceleration,
    smoothing_engine = "stats_fallback",
    smoothing_status = "complete",
    notes = "fda is not available; used a transparent smooth.spline fallback.",
    stringsAsFactors = FALSE
  )
}

functional_smooth_one_curve <- function(
  time,
  glucose,
  eval_time,
  parameters = functional_default_parameters()
) {
  parameters <- normalize_functional_parameters(parameters)
  keep <- is.finite(time) & is.finite(glucose)
  time <- as.numeric(time[keep])
  glucose <- as.numeric(glucose[keep])
  ord <- order(time)
  time <- time[ord]
  glucose <- glucose[ord]
  duplicate <- duplicated(time)
  time <- time[!duplicate]
  glucose <- glucose[!duplicate]

  if (length(time) < parameters$min_observed_points) {
    return(data.frame(
      time_minutes = eval_time,
      smoothed_glucose = NA_real_,
      rate_mg_dl_per_hour = NA_real_,
      acceleration_mg_dl_per_hour2 = NA_real_,
      smoothing_engine = if (functional_fda_available()) {
        "fda"
      } else {
        "stats_fallback"
      },
      smoothing_status = "ineligible",
      notes = paste0(
        "Needs at least ",
        parameters$min_observed_points,
        " observed points for FDA smoothing."
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (functional_fda_available()) {
    result <- tryCatch(
      functional_smooth_with_fda(time, glucose, eval_time, parameters),
      error = function(error) {
        fallback <- functional_smooth_fallback(time, glucose, eval_time)
        fallback$smoothing_status <- "fallback"
        fallback$notes <- paste(
          "fda smoothing failed; used smooth.spline fallback. Details:",
          conditionMessage(error)
        )
        fallback
      }
    )
  } else {
    result <- functional_smooth_fallback(time, glucose, eval_time)
  }
  validate_functional_smoothing(result, parameters)
}

compute_functional_day_curves <- function(
  data,
  parameters = functional_default_parameters()
) {
  parameters <- normalize_functional_parameters(parameters)
  rows <- functional_curve_rows(data, parameters)
  grid <- rows$grid
  metadata <- rows$metadata
  values <- rows$matrix
  if (!nrow(metadata)) {
    return(empty_functional_day_curves())
  }

  out <- lapply(seq_len(nrow(metadata)), function(i) {
    raw <- as.numeric(values[i, ])
    smooth <- functional_smooth_one_curve(grid, raw, grid, parameters)
    smooth$id <- metadata$id[[i]]
    smooth$date <- metadata$date[[i]]
    smooth$raw_glucose <- raw
    smooth$observed_points <- metadata$observed_points[[i]]
    smooth$imputed_points <- metadata$imputed_points[[i]]
    smooth[, c(
      "id",
      "date",
      "time_minutes",
      "raw_glucose",
      "smoothed_glucose",
      "rate_mg_dl_per_hour",
      "acceleration_mg_dl_per_hour2",
      "observed_points",
      "imputed_points",
      "smoothing_engine",
      "smoothing_status",
      "notes"
    )]
  })
  out <- data.table::rbindlist(out, fill = TRUE)
  as.data.frame(out, stringsAsFactors = FALSE)
}

compute_functional_subject_curves <- function(day_curves, phenotype = NULL) {
  if (!is.data.frame(day_curves) || !nrow(day_curves)) {
    return(empty_functional_subject_curves())
  }
  eligible <- day_curves[is.finite(day_curves$smoothed_glucose), , drop = FALSE]
  if (!nrow(eligible)) {
    return(empty_functional_subject_curves())
  }
  dt <- data.table::as.data.table(eligible)
  out <- dt[,
    {
      glucose <- get("smoothed_glucose")
      rate <- get("rate_mg_dl_per_hour")
      list(
        mean_glucose = mean(glucose, na.rm = TRUE),
        q25_glucose = stats::quantile(
          glucose,
          0.25,
          na.rm = TRUE,
          names = FALSE
        ),
        q75_glucose = stats::quantile(
          glucose,
          0.75,
          na.rm = TRUE,
          names = FALSE
        ),
        mean_rate_mg_dl_per_hour = mean(rate, na.rm = TRUE),
        days = length(unique(get("date")))
      )
    },
    by = .(id, time_minutes)
  ]
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  if (
    is.data.frame(phenotype) && nrow(phenotype) && "id" %in% names(phenotype)
  ) {
    out <- merge(
      out,
      phenotype[, c("id", "phenotype_group"), drop = FALSE],
      by = "id",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    out$phenotype_group <- ""
  }
  out[order(out$id, out$time_minutes), , drop = FALSE]
}

functional_area <- function(
  glucose,
  threshold,
  direction = "above",
  step_hours = 5 / 60
) {
  if (identical(direction, "above")) {
    sum(pmax(glucose - threshold, 0), na.rm = TRUE) * step_hours
  } else {
    sum(pmax(threshold - glucose, 0), na.rm = TRUE) * step_hours
  }
}

functional_window_mean <- function(curve, start_minute, end_minute) {
  keep <- curve$time_minutes >= start_minute &
    curve$time_minutes < end_minute &
    is.finite(curve$mean_glucose)
  if (!any(keep)) NA_real_ else mean(curve$mean_glucose[keep], na.rm = TRUE)
}

functional_overnight_slope <- function(curve) {
  keep <- curve$time_minutes < 6 * 60 & is.finite(curve$mean_glucose)
  if (sum(keep) < 2L) {
    return(NA_real_)
  }
  simple_ols_slope(curve$time_minutes[keep] / 60, curve$mean_glucose[keep])
}

compute_functional_subject_features <- function(
  subject_curves,
  day_curves,
  data = NULL,
  thresholds = default_cgm_thresholds(),
  parameters = functional_default_parameters()
) {
  parameters <- normalize_functional_parameters(parameters)
  if (!is.data.frame(subject_curves) || !nrow(subject_curves)) {
    return(empty_functional_features())
  }
  step_hours <- parameters$grid_minutes / 60
  ids <- sort(unique(as.character(subject_curves$id)))
  rows <- lapply(ids, function(subject_id) {
    curve <- subject_curves[
      as.character(subject_curves$id) == subject_id,
      ,
      drop = FALSE
    ]
    curve <- curve[order(curve$time_minutes), , drop = FALSE]
    glucose <- curve$mean_glucose
    peak_idx <- if (any(is.finite(glucose))) which.max(glucose) else NA_integer_
    nadir_idx <- if (any(is.finite(glucose))) {
      which.min(glucose)
    } else {
      NA_integer_
    }
    day_rows <- day_curves[
      as.character(day_curves$id) == subject_id,
      ,
      drop = FALSE
    ]
    valid_day_count <- length(unique(day_rows$date[is.finite(
      day_rows$smoothed_glucose
    )]))
    excluded_day_count <- length(unique(day_rows$date[
      day_rows$smoothing_status == "invalid"
    ]))
    original_rows <- if (is.data.frame(data) && "id" %in% names(data)) {
      data[as.character(data$id) == subject_id, , drop = FALSE]
    } else {
      data.frame()
    }
    observed_points <- if (nrow(day_rows)) {
      max(day_rows$observed_points, na.rm = TRUE)
    } else {
      0L
    }
    imputed_points <- if (nrow(day_rows)) {
      max(day_rows$imputed_points, na.rm = TRUE)
    } else {
      0L
    }
    data.frame(
      id = subject_id,
      days = if (nrow(curve)) max(curve$days, na.rm = TRUE) else 0L,
      valid_fda_days = valid_day_count,
      excluded_fda_days = excluded_day_count,
      observed_points = as.integer(sum(
        unique(day_rows[,
          c("date", "observed_points"),
          drop = FALSE
        ])$observed_points,
        na.rm = TRUE
      )),
      imputed_points = as.integer(sum(
        unique(day_rows[,
          c("date", "imputed_points"),
          drop = FALSE
        ])$imputed_points,
        na.rm = TRUE
      )),
      profile_basis = functional_profile_basis(original_rows),
      peak_time = if (!is.na(peak_idx)) {
        functional_minutes_label(curve$time_minutes[[peak_idx]])
      } else {
        NA_character_
      },
      peak_glucose = if (!is.na(peak_idx)) {
        curve$mean_glucose[[peak_idx]]
      } else {
        NA_real_
      },
      nadir_time = if (!is.na(nadir_idx)) {
        functional_minutes_label(curve$time_minutes[[nadir_idx]])
      } else {
        NA_character_
      },
      nadir_glucose = if (!is.na(nadir_idx)) {
        curve$mean_glucose[[nadir_idx]]
      } else {
        NA_real_
      },
      time_below_range_percent = mean(
        glucose < thresholds$tir_lower,
        na.rm = TRUE
      ) *
        100,
      time_in_range_percent = mean(
        glucose >= thresholds$tir_lower & glucose <= thresholds$tir_upper,
        na.rm = TRUE
      ) *
        100,
      time_above_range_percent = mean(
        glucose > thresholds$tir_upper,
        na.rm = TRUE
      ) *
        100,
      area_below_range_mg_dl_hour = functional_area(
        glucose,
        thresholds$tir_lower,
        "below",
        step_hours
      ),
      area_above_range_mg_dl_hour = functional_area(
        glucose,
        thresholds$tir_upper,
        "above",
        step_hours
      ),
      max_rise_rate_mg_dl_hour = if (
        any(is.finite(curve$mean_rate_mg_dl_per_hour))
      ) {
        max(curve$mean_rate_mg_dl_per_hour, na.rm = TRUE)
      } else {
        NA_real_
      },
      max_fall_rate_mg_dl_hour = if (
        any(is.finite(curve$mean_rate_mg_dl_per_hour))
      ) {
        min(curve$mean_rate_mg_dl_per_hour, na.rm = TRUE)
      } else {
        NA_real_
      },
      overnight_slope_mg_dl_hour = functional_overnight_slope(curve),
      morning_rise_mg_dl = functional_window_mean(curve, 6 * 60, 10 * 60) -
        functional_window_mean(curve, 4 * 60, 6 * 60),
      fpca1 = NA_real_,
      fpca2 = NA_real_,
      phenotype_group = unique(curve$phenotype_group)[[1L]] %||% "",
      smoothing_engine = unique(day_rows$smoothing_engine)[[1L]] %||% "",
      fpca_engine = "",
      notes = paste(
        unique(day_rows$notes[nzchar(day_rows$notes)]),
        collapse = " "
      ),
      stringsAsFactors = FALSE
    )
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  as.data.frame(out, stringsAsFactors = FALSE)
}

functional_subject_curve_matrix <- function(subject_curves) {
  if (!is.data.frame(subject_curves) || !nrow(subject_curves)) {
    return(matrix(numeric(), nrow = 0L, ncol = 0L))
  }
  dt <- data.table::as.data.table(subject_curves)
  wide <- data.table::dcast(dt, id ~ time_minutes, value.var = "mean_glucose")
  ids <- wide$id
  wide$id <- NULL
  mat <- as.matrix(wide)
  rownames(mat) <- as.character(ids)
  storage.mode(mat) <- "numeric"
  mat
}

functional_prcomp_scores <- function(
  mat,
  components = 2L,
  engine = "prcomp_fallback"
) {
  components <- max(1L, min(4L, as.integer(components %||% 2L)))
  out <- data.frame(
    id = rownames(mat),
    fpca1 = NA_real_,
    fpca2 = NA_real_,
    fpca_engine = engine,
    stringsAsFactors = FALSE
  )
  if (nrow(mat) < 2L || ncol(mat) < 2L) {
    return(out)
  }
  pca <- tryCatch(
    stats::prcomp(mat, center = TRUE, scale. = FALSE),
    error = function(error) NULL
  )
  if (is.null(pca) || is.null(pca$x) || !ncol(pca$x)) {
    return(out)
  }
  available <- min(components, ncol(pca$x), 2L)
  for (i in seq_len(available)) {
    out[[paste0("fpca", i)]] <- as.numeric(pca$x[, i])
  }
  out
}

compute_functional_scores <- function(subject_curves, components = 2L) {
  components <- max(1L, min(4L, as.integer(components %||% 2L)))
  mat <- functional_subject_curve_matrix(subject_curves)
  if (!nrow(mat)) {
    return(data.frame(
      id = character(),
      fpca1 = numeric(),
      fpca2 = numeric(),
      fpca_engine = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (nrow(mat) < 2L) {
    return(data.frame(
      id = rownames(mat),
      fpca1 = NA_real_,
      fpca2 = NA_real_,
      fpca_engine = if (functional_fdapace_available()) {
        "fdapace_not_needed"
      } else {
        "fdapace_unavailable"
      },
      stringsAsFactors = FALSE
    ))
  }

  fallback_scores <- functional_prcomp_scores(
    mat,
    components = components,
    engine = if (functional_fdapace_available()) {
      "prcomp_fallback_after_fdapace_unusable"
    } else {
      "prcomp_fallback_fdapace_unavailable"
    }
  )
  fdapace_scores <- NULL
  fdapace_note <- ""
  if (functional_fdapace_available()) {
    fdapace_scores <- tryCatch(
      {
        ids <- rownames(mat)
        time_values <- as.numeric(colnames(mat))
        Ly <- lapply(seq_len(nrow(mat)), function(i) as.numeric(mat[i, ]))
        Lt <- rep(list(time_values), length(Ly))
        fit <- NULL
        invisible(utils::capture.output(
          invisible(utils::capture.output(
            fit <- suppressWarnings(suppressMessages(fdapace::FPCA(
              Ly = Ly,
              Lt = Lt,
              optns = list(dataType = "Dense", error = FALSE, verbose = FALSE)
            ))),
            type = "message"
          )),
          type = "output"
        ))
        scores <- as.data.frame(fit$xiEst[,
          seq_len(min(components, ncol(fit$xiEst))),
          drop = FALSE
        ])
        names(scores) <- paste0("fpca", seq_len(ncol(scores)))
        scores$id <- ids
        scores$fpca_engine <- "fdapace"
        scores
      },
      error = function(error) {
        fdapace_note <<- conditionMessage(error)
        NULL
      }
    )
  }
  if (is.data.frame(fdapace_scores) && nrow(fdapace_scores)) {
    if (!"fpca2" %in% names(fdapace_scores)) {
      fdapace_scores$fpca2 <- NA_real_
    }
    if (
      !any(is.finite(fdapace_scores$fpca2)) &&
        any(is.finite(fallback_scores$fpca2))
    ) {
      fdapace_scores$fpca2 <- fallback_scores$fpca2[match(
        fdapace_scores$id,
        fallback_scores$id
      )]
      fdapace_scores$fpca_engine <- "fdapace+prcomp_pc2"
    }
    return(fdapace_scores[,
      c("id", "fpca1", "fpca2", "fpca_engine"),
      drop = FALSE
    ])
  }

  if (functional_fdapace_available() && nzchar(fdapace_note)) {
    fallback_scores$fpca_engine <- paste(
      "prcomp_fallback_after_fdapace_error",
      fdapace_note
    )
  }
  fallback_scores[, c("id", "fpca1", "fpca2", "fpca_engine"), drop = FALSE]
}

compute_functional_phenotypes <- function(scores, cluster_count = 3L) {
  if (!is.data.frame(scores) || !nrow(scores)) {
    return(data.frame(
      id = character(),
      phenotype_group = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- scores[,
    intersect(c("id", "fpca1", "fpca2", "fpca_engine"), names(scores)),
    drop = FALSE
  ]
  score_cols <- intersect(c("fpca1", "fpca2"), names(out))
  finite <- if (length(score_cols)) {
    stats::complete.cases(out[, score_cols, drop = FALSE])
  } else {
    rep(FALSE, nrow(out))
  }
  out$phenotype_group <- "Phenotype 1"
  if (sum(finite) >= 2L) {
    distinct_scores <- unique(out[finite, score_cols, drop = FALSE])
    k <- min(
      max(1L, as.integer(cluster_count %||% 3L)),
      sum(finite),
      nrow(distinct_scores),
      sum(finite) - 1L
    )
    if (k >= 2L) {
      set.seed(1)
      km <- stats::kmeans(
        out[finite, score_cols, drop = FALSE],
        centers = k,
        nstart = 10
      )
      out$phenotype_group[finite] <- paste0("Phenotype ", km$cluster)
    }
  }
  out
}

compute_functional_profile_bundle <- function(
  data,
  parameters = functional_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  parameters <- normalize_functional_parameters(parameters)
  day_curves <- cgm_suppress_non_cgma_messages(compute_functional_day_curves(
    data,
    parameters
  ))
  if (!nrow(day_curves) || !any(is.finite(day_curves$smoothed_glucose))) {
    return(list(
      parameters = parameters,
      day_curves = day_curves,
      subject_curves = empty_functional_subject_curves(),
      features = empty_functional_features(),
      scores = data.frame(
        id = character(),
        fpca1 = numeric(),
        fpca2 = numeric(),
        fpca_engine = character(),
        stringsAsFactors = FALSE
      ),
      engine_status = functional_engine_status()
    ))
  }
  subject_curves_initial <- compute_functional_subject_curves(day_curves)
  scores <- compute_functional_scores(
    subject_curves_initial,
    components = parameters$fpca_components
  )
  phenotypes <- compute_functional_phenotypes(
    scores,
    cluster_count = parameters$cluster_count
  )
  subject_curves <- compute_functional_subject_curves(
    day_curves,
    phenotype = phenotypes
  )
  features <- compute_functional_subject_features(
    subject_curves,
    day_curves,
    data = data,
    thresholds = thresholds,
    parameters = parameters
  )
  if (nrow(scores)) {
    score_cols <- intersect(
      c("id", "fpca1", "fpca2", "fpca_engine"),
      names(scores)
    )
    features <- merge(
      features,
      scores[, score_cols, drop = FALSE],
      by = "id",
      all.x = TRUE,
      sort = FALSE,
      suffixes = c("", ".score")
    )
    for (col in c("fpca1", "fpca2", "fpca_engine")) {
      score_col <- paste0(col, ".score")
      if (score_col %in% names(features)) {
        features[[col]] <- features[[score_col]]
        features[[score_col]] <- NULL
      }
    }
  }
  if (nrow(phenotypes)) {
    features <- merge(
      features,
      phenotypes[, c("id", "phenotype_group"), drop = FALSE],
      by = "id",
      all.x = TRUE,
      sort = FALSE,
      suffixes = c("", ".phenotype")
    )
    if ("phenotype_group.phenotype" %in% names(features)) {
      features$phenotype_group <- features$phenotype_group.phenotype
      features$phenotype_group.phenotype <- NULL
    }
    subject_curves <- merge(
      subject_curves[,
        setdiff(names(subject_curves), "phenotype_group"),
        drop = FALSE
      ],
      phenotypes[, c("id", "phenotype_group"), drop = FALSE],
      by = "id",
      all.x = TRUE,
      sort = FALSE
    )
  }
  list(
    parameters = parameters,
    day_curves = day_curves,
    subject_curves = subject_curves,
    features = features,
    scores = scores,
    engine_status = functional_engine_status()
  )
}

prepare_functional_features_display <- function(
  features,
  data = NULL,
  show_subject_id = NULL
) {
  if (!is.data.frame(features) || !nrow(features)) {
    return(data.frame())
  }
  out <- features
  if (is.data.frame(data) && "group" %in% names(data) && "id" %in% names(out)) {
    group_lookup <- stats::setNames(
      as.character(data$group),
      as.character(data$id)
    )
    out$Group <- unname(group_lookup[as.character(out$id)])
  }
  labels <- c(
    id = "Subject ID",
    days = "Days",
    valid_fda_days = "Valid FDA days",
    excluded_fda_days = "Excluded FDA days",
    observed_points = "Observed readings",
    imputed_points = "Imputed readings",
    profile_basis = "Profile basis",
    peak_time = "Peak time",
    peak_glucose = "Peak glucose",
    nadir_time = "Nadir time",
    nadir_glucose = "Nadir glucose",
    time_below_range_percent = "Time below range (%)",
    time_in_range_percent = "Time in range (%)",
    time_above_range_percent = "Time above range (%)",
    area_below_range_mg_dl_hour = "Area below range (mg/dL-hour)",
    area_above_range_mg_dl_hour = "Area above range (mg/dL-hour)",
    max_rise_rate_mg_dl_hour = "Max rise rate (mg/dL/hour)",
    max_fall_rate_mg_dl_hour = "Max fall rate (mg/dL/hour)",
    overnight_slope_mg_dl_hour = "Overnight slope (mg/dL/hour)",
    morning_rise_mg_dl = "Morning rise (mg/dL)",
    fpca1 = "FPCA 1",
    fpca2 = "FPCA 2",
    phenotype_group = "Phenotype group",
    smoothing_engine = "Smoothing engine",
    fpca_engine = "FPCA engine",
    notes = "Notes"
  )
  names(out) <- ifelse(
    names(out) %in% names(labels),
    unname(labels[names(out)]),
    names(out)
  )
  if (
    !show_subject_id_for_display(data, show_subject_id) &&
      "Subject ID" %in% names(out)
  ) {
    out$`Subject ID` <- NULL
  }
  out
}

prepare_functional_key_features_display <- function(
  features,
  data = NULL,
  show_subject_id = TRUE
) {
  display <- prepare_functional_features_display(
    features,
    data = data,
    show_subject_id = show_subject_id
  )
  if (!is.data.frame(display) || !nrow(display)) {
    return(display)
  }
  keep <- c(
    "Subject ID",
    "Group",
    "Days",
    "Valid FDA days",
    "Excluded FDA days",
    "Observed readings",
    "Profile basis",
    "Peak time",
    "Peak glucose",
    "Nadir time",
    "Nadir glucose",
    "Time below range (%)",
    "Time in range (%)",
    "Time above range (%)",
    "Max rise rate (mg/dL/hour)",
    "Max fall rate (mg/dL/hour)",
    "Overnight slope (mg/dL/hour)",
    "Morning rise (mg/dL)",
    "Phenotype group",
    "Smoothing engine",
    "FPCA engine"
  )
  display <- display[, intersect(keep, names(display)), drop = FALSE]
  numeric_cols <- names(display)[vapply(display, is.numeric, logical(1))]
  for (col in numeric_cols) {
    display[[col]] <- round(
      display[[col]],
      if (grepl("readings|Days", col, ignore.case = TRUE)) 0 else 2
    )
  }
  if ("Smoothing engine" %in% names(display)) {
    display$`Smoothing engine` <- vapply(
      display$`Smoothing engine`,
      functional_smoothing_engine_label,
      character(1)
    )
  }
  if ("FPCA engine" %in% names(display)) {
    display$`FPCA engine` <- vapply(
      display$`FPCA engine`,
      functional_fpca_engine_label,
      character(1)
    )
  }
  display
}

functional_compact_notes <- function(notes) {
  notes <- trimws(as.character(notes %||% ""))
  if (!nzchar(notes)) {
    return("")
  }
  if (grepl("FDA-smoothed day profiles were excluded", notes, fixed = TRUE)) {
    return("FDA smoothing exclusions; see full feature export.")
  }
  notes
}

prepare_functional_curves_export <- function(bundle) {
  curves <- bundle$subject_curves
  if (!is.data.frame(curves) || !nrow(curves)) {
    return(data.frame())
  }
  out <- curves
  out$Time <- functional_minutes_label(out$time_minutes)
  out$`Grid minutes` <- bundle$parameters$grid_minutes
  labels <- c(
    id = "Subject ID",
    time_minutes = "Time minutes",
    mean_glucose = "Mean smoothed glucose",
    q25_glucose = "25th percentile glucose",
    q75_glucose = "75th percentile glucose",
    mean_rate_mg_dl_per_hour = "Mean rate (mg/dL/hour)",
    days = "Days",
    phenotype_group = "Phenotype group"
  )
  names(out) <- ifelse(
    names(out) %in% names(labels),
    unname(labels[names(out)]),
    names(out)
  )
  leading <- intersect(
    c("Subject ID", "Phenotype group", "Time", "Time minutes"),
    names(out)
  )
  out[, c(leading, setdiff(names(out), leading)), drop = FALSE]
}

prepare_functional_features_export <- function(bundle, data = NULL) {
  prepare_functional_features_display(
    bundle$features,
    data = data,
    show_subject_id = TRUE
  )
}

functional_summary_cards <- function(bundle) {
  features <- bundle$features
  statuses <- if (
    is.data.frame(bundle$day_curves) &&
      "smoothing_status" %in% names(bundle$day_curves)
  ) {
    clean_filter_values(bundle$day_curves$smoothing_status)
  } else {
    character()
  }
  if (!is.data.frame(features) || !nrow(features)) {
    return(data.frame(
      Label = c("Profiles", "Smoothing"),
      Value = c(
        "No eligible profiles",
        functional_smoothing_status_label(statuses)
      ),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Label = c(
      "Profiles",
      "Valid FDA days",
      "Excluded FDA days",
      "Phenotypes",
      "Smoothing",
      "FPCA"
    ),
    Value = c(
      format_count(nrow(features)),
      format_count(sum(features$valid_fda_days %||% 0, na.rm = TRUE)),
      format_count(sum(features$excluded_fda_days %||% 0, na.rm = TRUE)),
      format_count(length(clean_filter_values(features$phenotype_group))),
      functional_smoothing_engine_label(features$smoothing_engine),
      functional_fpca_engine_label(features$fpca_engine)
    ),
    stringsAsFactors = FALSE
  )
}

functional_smoothing_warning_text <- function(bundle) {
  day_curves <- bundle$day_curves
  if (
    !is.data.frame(day_curves) || !"smoothing_status" %in% names(day_curves)
  ) {
    return("")
  }
  invalid <- day_curves[
    day_curves$smoothing_status == "invalid",
    ,
    drop = FALSE
  ]
  if (!nrow(invalid)) {
    return("")
  }
  invalid_days <- unique(invalid[,
    intersect(c("id", "date"), names(invalid)),
    drop = FALSE
  ])
  all_days <- unique(day_curves[,
    intersect(c("id", "date"), names(day_curves)),
    drop = FALSE
  ])
  excluded_count <- nrow(invalid_days)
  total_count <- nrow(all_days)
  parameters <- bundle$parameters %||% functional_default_parameters()
  lower <- parameters$plausible_min_glucose %||% 0
  upper <- parameters$plausible_max_glucose %||% 500
  if (total_count > 0L) {
    paste0(
      format_count(excluded_count),
      " of ",
      format_count(total_count),
      " FDA day profile(s) were excluded after smoothing produced implausible values outside ",
      lower,
      "-",
      upper,
      " mg/dL."
    )
  } else {
    paste0(
      format_count(excluded_count),
      " FDA day profile(s) were excluded after smoothing produced implausible values outside ",
      lower,
      "-",
      upper,
      " mg/dL."
    )
  }
}

functional_visual_available <- function(bundle, mode = "profile") {
  mode <- mode %||% "profile"
  if (identical(mode, "scores")) {
    features <- bundle$features
    return(
      is.data.frame(features) &&
        nrow(features) >= 2L &&
        all(c("fpca1", "fpca2") %in% names(features)) &&
        sum(is.finite(features$fpca1) & is.finite(features$fpca2)) >= 2L
    )
  }
  if (identical(mode, "phenotypes")) {
    curves <- bundle$subject_curves
    return(
      is.data.frame(curves) && length(clean_filter_values(curves$id)) >= 2L
    )
  }
  curves <- bundle$subject_curves
  is.data.frame(curves) &&
    nrow(curves) > 0L &&
    any(is.finite(curves$mean_glucose))
}

clean_functional_plotly_legend <- function(plotly_object) {
  if (is.null(plotly_object$x$data) || !length(plotly_object$x$data)) {
    return(plotly_object)
  }
  plotly_object$x$data <- lapply(plotly_object$x$data, function(trace) {
    name <- trace$name %||% ""
    cleaned <- sub("^\\((Phenotype [^,]+),[^)]*\\)$", "\\1", name)
    cleaned <- sub("^\\(([^,]+),[^)]*\\)$", "\\1", cleaned)
    if (nzchar(cleaned)) {
      trace$name <- cleaned
      trace$legendgroup <- cleaned
    }
    trace
  })
  plotly_object
}

functional_unavailable_visual_message <- function(bundle, mode = "profile") {
  switch(
    mode %||% "profile",
    scores = "At least two eligible profiles are needed for the FPCA score plot.",
    phenotypes = "At least two eligible profiles are needed for phenotype profile comparison.",
    rate = "No valid rate-of-change profile is available for the current selection.",
    "No valid glycemic pattern profile is available for the current selection."
  )
}

create_functional_profile_plot <- function(
  subject_curves,
  day_curves = NULL,
  subject = ""
) {
  subject <- normalize_filter_value(subject)
  curves <- subject_curves
  days <- day_curves
  if (nzchar(subject)) {
    curves <- curves[as.character(curves$id) == subject, , drop = FALSE]
    days <- days[as.character(days$id) == subject, , drop = FALSE]
  }
  if (!is.data.frame(curves) || !nrow(curves)) {
    return(empty_plot(
      "No glycemic pattern profile is available for the current selection."
    ))
  }
  phenotype_count <- length(clean_filter_values(curves$phenotype_group))
  plot <- ggplot2::ggplot()
  if (is.data.frame(days) && nrow(days)) {
    day_rows <- days[is.finite(days$smoothed_glucose), , drop = FALSE]
    if (nrow(day_rows)) {
      day_rows$time_label <- functional_minutes_label(day_rows$time_minutes)
      day_rows$hover <- paste0(
        "Individual valid day profile",
        "<br>Subject ID: ",
        day_rows$id,
        "<br>Date: ",
        day_rows$date,
        "<br>Time: ",
        day_rows$time_label,
        "<br>Smoothed glucose: ",
        round(day_rows$smoothed_glucose, 2),
        " mg/dL"
      )
      plot <- plot +
        suppressWarnings(ggplot2::geom_line(
          data = day_rows,
          ggplot2::aes(
            x = time_minutes,
            y = smoothed_glucose,
            group = interaction(id, date),
            text = hover
          ),
          color = "#3B82A0",
          alpha = 0.32,
          linewidth = 0.38,
          na.rm = TRUE
        ))
    }
  }
  curves$time_label <- functional_minutes_label(curves$time_minutes)
  phenotype <- if ("phenotype_group" %in% names(curves)) {
    as.character(curves$phenotype_group)
  } else {
    rep("", nrow(curves))
  }
  phenotype[is.na(phenotype)] <- ""
  curves$band_hover <- paste0(
    "Day-to-day variability band",
    "<br>Subject ID: ",
    curves$id,
    "<br>Time: ",
    curves$time_label,
    "<br>25th percentile: ",
    round(curves$q25_glucose, 2),
    " mg/dL",
    "<br>75th percentile: ",
    round(curves$q75_glucose, 2),
    " mg/dL",
    "<br>Valid FDA days: ",
    curves$days
  )
  curves$mean_hover <- paste0(
    "Mean smoothed profile",
    "<br>Subject ID: ",
    curves$id,
    "<br>Time: ",
    curves$time_label,
    "<br>Mean glucose: ",
    round(curves$mean_glucose, 2),
    " mg/dL",
    "<br>Valid FDA days: ",
    curves$days,
    ifelse(nzchar(phenotype), paste0("<br>Phenotype group: ", phenotype), "")
  )
  plot +
    suppressWarnings(ggplot2::geom_ribbon(
      data = curves,
      ggplot2::aes(
        x = time_minutes,
        ymin = q25_glucose,
        ymax = q75_glucose,
        text = band_hover
      ),
      fill = "#93C5FD",
      color = "#2563EB",
      linewidth = 0.15,
      alpha = 0.34,
      show.legend = FALSE,
      na.rm = TRUE
    )) +
    suppressWarnings(ggplot2::geom_line(
      data = curves,
      ggplot2::aes(
        x = time_minutes,
        y = mean_glucose,
        group = id,
        text = mean_hover
      ),
      color = "#D95F59",
      linewidth = 0.9,
      na.rm = TRUE
    )) +
    time_of_day_scale() +
    ggplot2::labs(x = "Time of day", y = "Glucose (mg/dL)") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = if (phenotype_count > 1L) "bottom" else "none"
    )
}

create_functional_rate_plot <- function(subject_curves, subject = "") {
  subject <- normalize_filter_value(subject)
  curves <- subject_curves
  if (nzchar(subject)) {
    curves <- curves[as.character(curves$id) == subject, , drop = FALSE]
  }
  if (!is.data.frame(curves) || !nrow(curves)) {
    return(empty_plot(
      "No rate-of-change profile is available for the current selection."
    ))
  }
  phenotype_count <- length(clean_filter_values(curves$phenotype_group))
  curves$time_label <- functional_minutes_label(curves$time_minutes)
  curves$rate_hover <- paste0(
    "Mean smoothed rate of change",
    "<br>Subject ID: ",
    curves$id,
    "<br>Time: ",
    curves$time_label,
    "<br>Rate: ",
    round(curves$mean_rate_mg_dl_per_hour, 2),
    " mg/dL/hour",
    "<br>Direction: ",
    ifelse(
      curves$mean_rate_mg_dl_per_hour > 0,
      "rising",
      ifelse(curves$mean_rate_mg_dl_per_hour < 0, "falling", "flat")
    ),
    "<br>Valid FDA days: ",
    curves$days
  )
  ggplot2::ggplot(
    curves,
    ggplot2::aes(
      x = time_minutes,
      y = mean_rate_mg_dl_per_hour,
      group = id,
      text = rate_hover
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "#666666",
      linetype = "dotted"
    ) +
    suppressWarnings(ggplot2::geom_line(
      color = "#D95F59",
      linewidth = 0.9,
      na.rm = TRUE
    )) +
    time_of_day_scale() +
    ggplot2::labs(x = "Time of day", y = "Smoothed rate (mg/dL/hour)") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = if (phenotype_count > 1L) "bottom" else "none"
    )
}

create_functional_score_plot <- function(features) {
  if (
    !is.data.frame(features) ||
      nrow(features) < 2L ||
      !all(c("fpca1", "fpca2") %in% names(features))
  ) {
    return(empty_plot(
      "At least two eligible profiles are needed for the FPCA score plot."
    ))
  }
  rows <- features[
    is.finite(features$fpca1) & is.finite(features$fpca2),
    ,
    drop = FALSE
  ]
  if (nrow(rows) < 2L) {
    return(empty_plot(
      "FPCA scores are not available for the current selection."
    ))
  }
  score_phenotype <- if ("phenotype_group" %in% names(rows)) {
    as.character(rows$phenotype_group)
  } else {
    rep("", nrow(rows))
  }
  score_phenotype[is.na(score_phenotype)] <- ""
  rows$score_hover <- paste0(
    "Glycemic curve-shape score",
    "<br>Subject ID: ",
    rows$id,
    "<br>FPCA 1: ",
    round(rows$fpca1, 3),
    "<br>FPCA 2: ",
    round(rows$fpca2, 3),
    ifelse(
      nzchar(score_phenotype),
      paste0("<br>Phenotype group: ", score_phenotype),
      ""
    ),
    ifelse(
      "valid_fda_days" %in% names(rows),
      paste0("<br>Valid FDA days: ", rows$valid_fda_days),
      ""
    )
  )
  ggplot2::ggplot(
    rows,
    ggplot2::aes(x = fpca1, y = fpca2, color = id, text = score_hover)
  ) +
    suppressWarnings(ggplot2::geom_point(size = 2.5, alpha = 0.9)) +
    ggplot2::labs(x = "FPCA 1", y = "FPCA 2", color = "Subject ID") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

create_functional_phenotype_plot <- function(subject_curves) {
  if (!is.data.frame(subject_curves) || !nrow(subject_curves)) {
    return(empty_plot(
      "No phenotype profiles are available for the current selection."
    ))
  }
  valid_subjects <- length(clean_filter_values(subject_curves$id))
  if (valid_subjects < 2L) {
    return(empty_plot(
      "At least two eligible profiles are needed for phenotype profile comparison."
    ))
  }
  dt <- data.table::as.data.table(subject_curves)
  rows <- dt[,
    list(
      mean_glucose = mean(mean_glucose, na.rm = TRUE),
      q25_glucose = stats::quantile(
        mean_glucose,
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      q75_glucose = stats::quantile(
        mean_glucose,
        0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      subjects = length(unique(id))
    ),
    by = .(phenotype_group, time_minutes)
  ]
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  rows <- rows[
    is.finite(rows$time_minutes) & is.finite(rows$mean_glucose),
    ,
    drop = FALSE
  ]
  if (!nrow(rows)) {
    return(empty_plot(
      "No visible phenotype profile curves are available for the current selection."
    ))
  }
  rows$phenotype_group <- as.character(rows$phenotype_group)
  rows$phenotype_group[
    is.na(rows$phenotype_group) | !nzchar(rows$phenotype_group)
  ] <- "Phenotype 1"
  rows$phenotype_group <- factor(
    rows$phenotype_group,
    levels = sort(unique(rows$phenotype_group))
  )
  phenotype_count <- length(levels(rows$phenotype_group))
  rows$time_label <- functional_minutes_label(rows$time_minutes)
  rows$phenotype_hover <- paste0(
    "Phenotype group profile",
    "<br>Phenotype group: ",
    rows$phenotype_group,
    "<br>Time: ",
    rows$time_label,
    "<br>Mean glucose: ",
    round(rows$mean_glucose, 2),
    " mg/dL",
    "<br>25th percentile: ",
    round(rows$q25_glucose, 2),
    " mg/dL",
    "<br>75th percentile: ",
    round(rows$q75_glucose, 2),
    " mg/dL",
    "<br>Subjects: ",
    rows$subjects
  )
  ggplot2::ggplot(
    rows,
    ggplot2::aes(
      x = time_minutes,
      y = mean_glucose,
      color = phenotype_group,
      group = phenotype_group
    )
  ) +
    suppressWarnings(ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = q25_glucose,
        ymax = q75_glucose,
        text = phenotype_hover
      ),
      fill = "#93C5FD",
      alpha = 0.18,
      color = NA,
      show.legend = FALSE,
      na.rm = TRUE
    )) +
    suppressWarnings(ggplot2::geom_line(
      ggplot2::aes(text = phenotype_hover),
      linewidth = 1.05,
      na.rm = TRUE
    )) +
    time_of_day_scale() +
    ggplot2::labs(
      x = "Time of day",
      y = "Mean smoothed glucose (mg/dL)",
      color = "Phenotype"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = if (phenotype_count > 1L) "bottom" else "none"
    )
}
