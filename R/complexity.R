complexity_metric_catalog <- function() {
  data.frame(
    raw_name = c(
      "shannon_entropy",
      "hurst_exponent"
    ),
    metric = c(
      "Shannon entropy",
      "Hurst exponent"
    ),
    units = c("0-1 normalized", "Unitless"),
    scale = c(
      "10 mg/dL glucose bins by default",
      "Scalar Hrs long-range dependence estimate"
    ),
    definition = c(
      "Normalized entropy of glucose values across bins; higher values indicate a wider glucose distribution.",
      "Long-range dependence estimate; values above 0.5 suggest more persistent glucose trends."
    ),
    order = seq_len(2),
    stringsAsFactors = FALSE
  )
}

complexity_curve_catalog <- function() {
  data.frame(
    curve_metric = c("mse", "dfa", "higuchi"),
    label = c("Multiscale sample entropy", "DFA fluctuation", "Higuchi curve length"),
    scale_variable = c("Scale", "Window size", "k"),
    value_label = c("Sample entropy", "Fluctuation", "Curve length"),
    stringsAsFactors = FALSE
  )
}

complexity_default_parameters <- function(
    min_points = 100,
    entropy_bin_width = 10,
    embedding_dimension = 2,
    mse_scale_max = 5,
    higuchi_kmax = 8,
    max_gap_intervals = 4) {
  list(
    min_points = as.integer(min_points),
    entropy_bin_width = as.numeric(entropy_bin_width),
    embedding_dimension = as.integer(embedding_dimension),
    mse_scale_max = as.integer(mse_scale_max),
    higuchi_kmax = as.integer(higuchi_kmax),
    max_gap_intervals = as.integer(max_gap_intervals)
  )
}

complexity_parameter_label <- function(parameters) {
  paste0(
    "min ", parameters$min_points,
    " points; bin ", format(parameters$entropy_bin_width, trim = TRUE),
    " mg/dL; m=", parameters$embedding_dimension,
    "; MSE scales 1-", parameters$mse_scale_max,
    "; Higuchi kmax=", parameters$higuchi_kmax
  )
}

shannon_entropy_normalized <- function(x, bin_width = 10) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  if (length(unique(x)) <= 1L) {
    return(0)
  }
  bin_width <- as.numeric(bin_width)
  if (is.na(bin_width) || bin_width <= 0) {
    bin_width <- 10
  }
  lower <- floor(min(x) / bin_width) * bin_width
  upper <- ceiling(max(x) / bin_width) * bin_width
  if (lower == upper) {
    upper <- lower + bin_width
  }
  breaks <- seq(lower, upper, by = bin_width)
  if (length(breaks) < 2L) {
    breaks <- c(lower, lower + bin_width)
  }
  counts <- graphics::hist(x, breaks = breaks, plot = FALSE, include.lowest = TRUE, right = TRUE)$counts
  probabilities <- counts[counts > 0] / sum(counts)
  if (length(probabilities) <= 1L) {
    return(0)
  }
  entropy <- -sum(probabilities * log(probabilities))
  entropy / log(length(probabilities))
}

safe_hurst_exponent <- function(x) {
  result <- tryCatch(pracma::hurstexp(x, display = FALSE), error = function(error) NULL)
  if (is.null(result)) {
    return(NA_real_)
  }
  value <- result$Hrs %||% result$Hs %||% NA_real_
  if (length(value) && is.finite(value[[1L]])) {
    as.numeric(value[[1L]])
  } else {
    NA_real_
  }
}

simple_ols_slope <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  x_centered <- x - mean(x)
  denominator <- sum(x_centered^2)
  if (!is.finite(denominator) || denominator <= 0) {
    return(NA_real_)
  }
  as.numeric(sum(x_centered * (y - mean(y))) / denominator)
}

rms_detrended_linear <- function(y) {
  y <- as.numeric(y)
  if (length(y) < 2L || !any(is.finite(y))) {
    return(NA_real_)
  }
  x <- seq_along(y)
  slope <- simple_ols_slope(x, y)
  if (!is.finite(slope)) {
    return(NA_real_)
  }
  intercept <- mean(y) - slope * mean(x)
  sqrt(mean((y - (intercept + slope * x))^2))
}

compute_cgmanalyzer_mse <- function(x, scale_max = 5, embedding_dimension = 2, tolerance = 0.15) {
  x <- x[is.finite(x)]
  empty_scales <- data.frame(
    Scale = numeric(),
    SampleEntropy = numeric(),
    stringsAsFactors = FALSE
  )
  if (!length(x)) {
    return(list(
      value = NA_real_,
      note = "No finite glucose values available for multiscale entropy.",
      scales = empty_scales
    ))
  }
  scale_max <- as.integer(scale_max)
  if (is.na(scale_max) || scale_max < 1L) {
    scale_max <- 5L
  }
  if (length(x) < scale_max * 10L) {
    return(list(
      value = NA_real_,
      note = paste0("Needs at least ", scale_max * 10L, " usable points for multiscale entropy."),
      scales = empty_scales
    ))
  }
  if (!requireNamespace("CGManalyzer", quietly = TRUE) || !"MSEbyC.fn" %in% getNamespaceExports("CGManalyzer")) {
    return(list(
      value = NA_real_,
      note = "Multiscale sample entropy is not available in this R session.",
      scales = empty_scales
    ))
  }

  old_wd <- getwd()
  temp_dir <- tempfile("cgm_mse_")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    setwd(old_wd)
    unlink(temp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  result <- tryCatch({
    setwd(temp_dir)
    CGManalyzer::MSEbyC.fn(
      x,
      scaleMax = scale_max,
      scaleStep = 1,
      mMin = embedding_dimension,
      mMax = embedding_dimension,
      mStep = 1,
      rMin = tolerance,
      rMax = tolerance
    )
  }, error = function(error) error)

  if (inherits(result, "error")) {
    return(list(
      value = NA_real_,
      note = "Multiscale sample entropy could not be computed for this series.",
      scales = empty_scales
    ))
  }
  scale_values <- data.frame(
    Scale = as.numeric(result$Scale),
    SampleEntropy = as.numeric(result$SampleEntropy),
    stringsAsFactors = FALSE
  )
  finite_values <- scale_values$SampleEntropy
  finite_values <- finite_values[is.finite(finite_values)]
  if (!length(finite_values)) {
    return(list(
      value = NA_real_,
      note = "Multiscale sample entropy did not return finite values.",
      scales = empty_scales
    ))
  }
  list(value = NA_real_, note = "", scales = scale_values)
}

compute_dfa_details <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  empty <- list(
    value = NA_real_,
    curve = data.frame(
      scale_value = numeric(),
      metric_value = numeric(),
      stringsAsFactors = FALSE
    )
  )
  if (n < 32L || length(unique(x)) <= 1L) {
    return(empty)
  }
  y <- cumsum(x - mean(x))
  windows <- unique(floor(exp(seq(log(8), log(floor(n / 4)), length.out = 8))))
  windows <- windows[windows >= 8L & windows < n / 2]
  if (!length(windows)) {
    return(empty)
  }
  fluctuation <- vapply(windows, function(window) {
    segments <- floor(n / window)
    if (segments < 2L) {
      return(NA_real_)
    }
    rms <- vapply(seq_len(segments), function(segment) {
      idx <- ((segment - 1L) * window + 1L):(segment * window)
      rms_detrended_linear(y[idx])
    }, numeric(1))
    sqrt(mean(rms^2, na.rm = TRUE))
  }, numeric(1))
  keep <- is.finite(fluctuation) & fluctuation > 0 & windows > 0
  curve <- data.frame(
    scale_value = as.numeric(windows[keep]),
    metric_value = as.numeric(fluctuation[keep]),
    stringsAsFactors = FALSE
  )
  if (sum(keep) < 2L) {
    return(list(value = NA_real_, curve = curve))
  }
  list(
    value = simple_ols_slope(log(windows[keep]), log(fluctuation[keep])),
    curve = curve
  )
}

safe_dfa_alpha <- function(x) {
  compute_dfa_details(x)$value
}

compute_higuchi_details <- function(x, kmax = 8) {
  x <- x[is.finite(x)]
  n <- length(x)
  kmax <- as.integer(kmax)
  empty <- list(
    value = NA_real_,
    curve = data.frame(
      scale_value = numeric(),
      metric_value = numeric(),
      stringsAsFactors = FALSE
    )
  )
  if (is.na(kmax) || kmax < 2L) {
    kmax <- 8L
  }
  if (n < (kmax * 2L) || length(unique(x)) <= 1L) {
    return(empty)
  }
  k_values <- seq_len(kmax)
  lengths <- vapply(k_values, function(k) {
    lm_values <- vapply(seq_len(k), function(m) {
      idx <- seq(m, n, by = k)
      if (length(idx) < 2L) {
        return(NA_real_)
      }
      curve_length <- sum(abs(diff(x[idx])))
      curve_length * (n - 1L) / ((length(idx) - 1L) * k)
    }, numeric(1))
    mean(lm_values, na.rm = TRUE)
  }, numeric(1))
  keep <- is.finite(lengths) & lengths > 0
  curve <- data.frame(
    scale_value = as.numeric(k_values[keep]),
    metric_value = as.numeric(lengths[keep]),
    stringsAsFactors = FALSE
  )
  if (sum(keep) < 2L) {
    return(list(value = NA_real_, curve = curve))
  }
  list(
    value = simple_ols_slope(log(1 / k_values[keep]), log(lengths[keep])),
    curve = curve
  )
}

safe_higuchi_fd <- function(x, kmax = 8) {
  compute_higuchi_details(x, kmax = kmax)$value
}

empty_complexity_curve_rows <- function() {
  data.frame(
    id = character(),
    curve_metric = character(),
    scale_variable = character(),
    scale_value = numeric(),
    metric_value = numeric(),
    value_label = character(),
    derived_scalar_label = character(),
    derived_scalar_value = numeric(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

make_complexity_curve_rows <- function(
    id,
    curve_metric,
    scale_value,
    metric_value,
    note = "",
    derived_scalar_label = "",
    derived_scalar_value = NA_real_) {
  catalog <- complexity_curve_catalog()
  idx <- match(curve_metric, catalog$curve_metric)
  if (is.na(idx)) {
    return(empty_complexity_curve_rows())
  }
  keep <- is.finite(scale_value) & is.finite(metric_value)
  if (!any(keep)) {
    return(empty_complexity_curve_rows())
  }
  data.frame(
    id = as.character(id),
    curve_metric = curve_metric,
    scale_variable = catalog$scale_variable[[idx]],
    scale_value = as.numeric(scale_value[keep]),
    metric_value = as.numeric(metric_value[keep]),
    value_label = catalog$value_label[[idx]],
    derived_scalar_label = derived_scalar_label %||% "",
    derived_scalar_value = as.numeric(derived_scalar_value %||% NA_real_),
    note = note %||% "",
    stringsAsFactors = FALSE
  )
}

complexity_mse_pending_note <- function() {
  "Pending background MSE calculation."
}

complexity_mse_failed_note <- function() {
  "MSE could not compute in the background for this selection."
}

complexity_pending_note <- function() {
  "Pending background complexity calculation."
}

complexity_failed_note <- function() {
  "Complexity could not compute in the background for this selection."
}

complexity_hurst_pending_note <- function() {
  "Hurst exponent is calculating."
}

complexity_hurst_failed_note <- function() {
  "Hurst exponent could not compute in the background for this selection."
}

complexity_curve_pending_note <- function() {
  "Pending DFA/Higuchi curve calculation."
}

complexity_curve_failed_note <- function() {
  "DFA/Higuchi curves could not compute in the background for this selection."
}

lightweight_complexity_gap_count <- function(data, interval_minutes = NULL) {
  if (!all(c("timestamp", "glucose") %in% names(data))) {
    return(NA_integer_)
  }
  x <- data[!is.na(data$timestamp) & !is.na(data$glucose), c("timestamp", "glucose"), drop = FALSE]
  x <- x[order(x$timestamp), , drop = FALSE]
  x <- x[!duplicated(x$timestamp), , drop = FALSE]
  if (nrow(x) < 2L) {
    return(0L)
  }
  if (is.null(interval_minutes) || is.na(interval_minutes) || interval_minutes <= 0) {
    interval_minutes <- median_sampling_interval(x$timestamp)
  }
  if (is.na(interval_minutes) || interval_minutes <= 0) {
    return(NA_integer_)
  }
  diffs <- as.numeric(diff(x$timestamp), units = "mins")
  as.integer(sum(is.finite(diffs) & diffs > (interval_minutes * 1.5), na.rm = TRUE))
}

complexity_result_columns <- function() {
  data.frame(
    id = character(),
    readings = integer(),
    finite_glucose_rows = integer(),
    first_timestamp = as.POSIXct(character()),
    last_timestamp = as.POSIXct(character()),
    regularized_points = integer(),
    usable_points = integer(),
    interval_minutes = numeric(),
    gap_count = integer(),
    eligible = logical(),
    notes = character(),
    shannon_entropy = numeric(),
    sample_entropy = numeric(),
    approximate_entropy = numeric(),
    multiscale_sample_entropy = numeric(),
    hurst_exponent = numeric(),
    dfa_alpha = numeric(),
    higuchi_fractal_dimension = numeric(),
    shannon_entropy_note = character(),
    sample_entropy_note = character(),
    approximate_entropy_note = character(),
    multiscale_sample_entropy_note = character(),
    hurst_exponent_note = character(),
    dfa_alpha_note = character(),
    higuchi_fractal_dimension_note = character(),
    stringsAsFactors = FALSE
  )
}

split_complexity_subjects <- function(data) {
  if (!is.data.frame(data) || !nrow(data) || !"id" %in% names(data)) {
    return(list())
  }
  data <- data[!is.na(data$id) & nzchar(as.character(data$id)), , drop = FALSE]
  if (!nrow(data)) {
    return(list())
  }
  ids <- sort(unique(as.character(data$id)))
  stats::setNames(lapply(ids, function(id) data[as.character(data$id) == id, , drop = FALSE]), ids)
}

compute_complexity_pending_summary <- function(data, parameters, status = "running") {
  required <- c("id", "timestamp", "glucose")
  if (!all(required %in% names(data))) {
    stop("Complexity metrics require standardized columns: id, timestamp, and glucose.", call. = FALSE)
  }
  subjects <- split_complexity_subjects(data)
  if (!length(subjects)) {
    return(complexity_result_columns())
  }
  metric_note <- switch(
    status %||% "running",
    failed = complexity_failed_note(),
    complete = "",
    complexity_pending_note()
  )
  rows <- lapply(subjects, function(x) {
    subject_id <- as.character(x$id[[1L]])
    finite_rows <- sum(is.finite(as.numeric(x$glucose)), na.rm = TRUE)
    timestamps <- x$timestamp[!is.na(x$timestamp)]
    first_timestamp <- if (length(timestamps)) min(timestamps) else as.POSIXct(NA)
    last_timestamp <- if (length(timestamps)) max(timestamps) else as.POSIXct(NA)
    interval <- tryCatch(median_sampling_interval(timestamps), error = function(error) NA_real_)
    eligible <- finite_rows >= parameters$min_points
    note <- if (eligible) {
      if (identical(status, "failed")) complexity_failed_note() else complexity_pending_note()
    } else {
      paste0("Needs at least ", parameters$min_points, " usable regularized points.")
    }
    data.frame(
      id = subject_id,
      readings = nrow(x),
      finite_glucose_rows = finite_rows,
      first_timestamp = first_timestamp,
      last_timestamp = last_timestamp,
      regularized_points = NA_integer_,
      usable_points = finite_rows,
      interval_minutes = as.numeric(interval),
      gap_count = lightweight_complexity_gap_count(x, interval),
      eligible = eligible,
      notes = note,
      shannon_entropy = NA_real_,
      sample_entropy = NA_real_,
      approximate_entropy = NA_real_,
      multiscale_sample_entropy = NA_real_,
      hurst_exponent = NA_real_,
      dfa_alpha = NA_real_,
      higuchi_fractal_dimension = NA_real_,
      shannon_entropy_note = if (eligible) metric_note else "",
      sample_entropy_note = "",
      approximate_entropy_note = "",
      multiscale_sample_entropy_note = if (eligible && !identical(status, "failed")) complexity_mse_pending_note() else if (eligible) complexity_mse_failed_note() else "",
      hurst_exponent_note = if (eligible) metric_note else "",
      dfa_alpha_note = if (eligible) metric_note else "",
      higuchi_fractal_dimension_note = if (eligible) metric_note else "",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

compute_complexity_subject_result <- function(data, parameters, include_mse = FALSE) {
  subject_id <- as.character(data$id[[1L]])
  readings <- nrow(data)
  regular <- regularize_cgm_series(data, max_gap_intervals = parameters$max_gap_intervals)
  interval <- attr(regular, "interval_minutes", exact = TRUE)
  gap_count <- lightweight_complexity_gap_count(data, interval)
  values <- regular$glucose[is.finite(regular$glucose)]
  usable_points <- length(values)
  timestamps <- data$timestamp[!is.na(data$timestamp)]
  eligible <- usable_points >= parameters$min_points
  note <- if (eligible) {
    "Eligible"
  } else {
    paste0("Needs at least ", parameters$min_points, " usable regularized points.")
  }

  out <- data.frame(
    id = subject_id,
    readings = readings,
    finite_glucose_rows = sum(is.finite(as.numeric(data$glucose)), na.rm = TRUE),
    first_timestamp = if (length(timestamps)) min(timestamps) else as.POSIXct(NA),
    last_timestamp = if (length(timestamps)) max(timestamps) else as.POSIXct(NA),
    regularized_points = nrow(regular),
    usable_points = usable_points,
    interval_minutes = as.numeric(interval),
    gap_count = gap_count,
    eligible = eligible,
    notes = note,
    shannon_entropy = NA_real_,
    sample_entropy = NA_real_,
    approximate_entropy = NA_real_,
    multiscale_sample_entropy = NA_real_,
    hurst_exponent = NA_real_,
    dfa_alpha = NA_real_,
    higuchi_fractal_dimension = NA_real_,
    shannon_entropy_note = "",
    sample_entropy_note = "",
    approximate_entropy_note = "",
    multiscale_sample_entropy_note = if (isTRUE(include_mse)) "" else complexity_mse_pending_note(),
    hurst_exponent_note = "",
    dfa_alpha_note = "",
    higuchi_fractal_dimension_note = "",
    stringsAsFactors = FALSE
  )
  curves <- empty_complexity_curve_rows()

  if (!eligible) {
    out$multiscale_sample_entropy_note <- ""
    return(list(metrics = out, curves = curves))
  }

  out$shannon_entropy <- shannon_entropy_normalized(values, parameters$entropy_bin_width)
  if (isTRUE(include_mse)) {
    mse <- compute_cgmanalyzer_mse(
      values,
      scale_max = parameters$mse_scale_max,
      embedding_dimension = parameters$embedding_dimension,
      tolerance = 0.15
    )
    out$multiscale_sample_entropy_note <- mse$note
    scales <- mse$scales
    if (is.data.frame(scales) && nrow(scales)) {
      scales <- scales[is.finite(scales$Scale) & is.finite(scales$SampleEntropy), , drop = FALSE]
      if (nrow(scales)) {
        curves <- rbind(
          curves,
          make_complexity_curve_rows(
            id = subject_id,
            curve_metric = "mse",
            scale_value = scales$Scale,
            metric_value = scales$SampleEntropy,
            note = mse$note %||% ""
          )
        )
      }
    }
  }
  out$hurst_exponent <- safe_hurst_exponent(values)
  out$hurst_exponent_note <- if (is.na(out$hurst_exponent)) "Hurst exponent could not be computed for this series." else ""
  dfa <- compute_dfa_details(values)
  out$dfa_alpha <- dfa$value
  out$dfa_alpha_note <- if (is.na(out$dfa_alpha)) "DFA alpha could not be computed for this series." else ""
  if (is.data.frame(dfa$curve) && nrow(dfa$curve)) {
    curves <- rbind(
      curves,
      make_complexity_curve_rows(
        id = subject_id,
        curve_metric = "dfa",
        scale_value = dfa$curve$scale_value,
        metric_value = dfa$curve$metric_value,
        note = out$dfa_alpha_note,
        derived_scalar_label = "DFA alpha",
        derived_scalar_value = out$dfa_alpha
      )
    )
  }
  higuchi <- compute_higuchi_details(values, parameters$higuchi_kmax)
  out$higuchi_fractal_dimension <- higuchi$value
  out$higuchi_fractal_dimension_note <- if (is.na(out$higuchi_fractal_dimension)) "Higuchi fractal dimension could not be computed for this series." else ""
  if (is.data.frame(higuchi$curve) && nrow(higuchi$curve)) {
    curves <- rbind(
      curves,
      make_complexity_curve_rows(
        id = subject_id,
        curve_metric = "higuchi",
        scale_value = higuchi$curve$scale_value,
        metric_value = higuchi$curve$metric_value,
        note = out$higuchi_fractal_dimension_note,
        derived_scalar_label = "Higuchi fractal dimension",
        derived_scalar_value = out$higuchi_fractal_dimension
      )
    )
  }
  row.names(curves) <- NULL
  list(metrics = out, curves = curves)
}

compute_complexity_quick_subject <- function(data, parameters) {
  subject_id <- as.character(data$id[[1L]])
  readings <- nrow(data)
  regular <- regularize_cgm_series(data, max_gap_intervals = parameters$max_gap_intervals)
  interval <- attr(regular, "interval_minutes", exact = TRUE)
  gap_count <- lightweight_complexity_gap_count(data, interval)
  values <- regular$glucose[is.finite(regular$glucose)]
  usable_points <- length(values)
  timestamps <- data$timestamp[!is.na(data$timestamp)]
  eligible <- usable_points >= parameters$min_points
  note <- if (eligible) {
    "Eligible"
  } else {
    paste0("Needs at least ", parameters$min_points, " usable regularized points.")
  }

  out <- data.frame(
    id = subject_id,
    readings = readings,
    finite_glucose_rows = sum(is.finite(as.numeric(data$glucose)), na.rm = TRUE),
    first_timestamp = if (length(timestamps)) min(timestamps) else as.POSIXct(NA),
    last_timestamp = if (length(timestamps)) max(timestamps) else as.POSIXct(NA),
    regularized_points = nrow(regular),
    usable_points = usable_points,
    interval_minutes = as.numeric(interval),
    gap_count = gap_count,
    eligible = eligible,
    notes = note,
    shannon_entropy = NA_real_,
    sample_entropy = NA_real_,
    approximate_entropy = NA_real_,
    multiscale_sample_entropy = NA_real_,
    hurst_exponent = NA_real_,
    dfa_alpha = NA_real_,
    higuchi_fractal_dimension = NA_real_,
    shannon_entropy_note = "",
    sample_entropy_note = "",
    approximate_entropy_note = "",
    multiscale_sample_entropy_note = if (eligible) complexity_mse_pending_note() else "",
    hurst_exponent_note = "",
    dfa_alpha_note = if (eligible) complexity_curve_pending_note() else "",
    higuchi_fractal_dimension_note = if (eligible) complexity_curve_pending_note() else "",
    stringsAsFactors = FALSE
  )

  if (!eligible) {
    return(out)
  }

  out$shannon_entropy <- shannon_entropy_normalized(values, parameters$entropy_bin_width)
  out$hurst_exponent_note <- complexity_hurst_pending_note()
  out
}

compute_complexity_quick_metrics <- function(data, parameters) {
  required <- c("id", "timestamp", "glucose")
  if (!all(required %in% names(data))) {
    stop("Complexity metrics require standardized columns: id, timestamp, and glucose.", call. = FALSE)
  }
  subjects <- split_complexity_subjects(data)
  if (!length(subjects)) {
    return(complexity_result_columns())
  }
  rows <- lapply(subjects, function(subject_data) {
    compute_complexity_quick_subject(subject_data, parameters)
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

empty_complexity_hurst_rows <- function() {
  data.frame(
    id = character(),
    hurst_exponent = numeric(),
    hurst_exponent_note = character(),
    stringsAsFactors = FALSE
  )
}

compute_complexity_hurst_one_subject <- function(data, parameters) {
  subject_id <- as.character(data$id[[1L]])
  regular <- regularize_cgm_series(data, max_gap_intervals = parameters$max_gap_intervals)
  values <- regular$glucose[is.finite(regular$glucose)]
  out <- data.frame(
    id = subject_id,
    hurst_exponent = NA_real_,
    hurst_exponent_note = "",
    stringsAsFactors = FALSE
  )
  if (length(values) < parameters$min_points) {
    return(out)
  }
  out$hurst_exponent <- safe_hurst_exponent(values)
  out$hurst_exponent_note[is.na(out$hurst_exponent)] <- "Hurst exponent could not be computed for this series."
  out
}

compute_complexity_hurst_metrics <- function(data, parameters) {
  required <- c("id", "timestamp", "glucose")
  if (!all(required %in% names(data))) {
    stop("Complexity Hurst metrics require standardized columns: id, timestamp, and glucose.", call. = FALSE)
  }
  subjects <- split_complexity_subjects(data)
  if (!length(subjects)) {
    return(empty_complexity_hurst_rows())
  }
  rows <- lapply(subjects, function(subject_data) {
    compute_complexity_hurst_one_subject(subject_data, parameters)
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

merge_complexity_hurst_results <- function(base, hurst_results = NULL, status = "idle") {
  if (!is.data.frame(base) || !nrow(base)) {
    return(base)
  }
  out <- base
  status <- status %||% "idle"
  eligible <- out$eligible %in% TRUE

  if (identical(status, "complete") && is.data.frame(hurst_results) && nrow(hurst_results)) {
    idx <- match(out$id, hurst_results$id)
    matched <- !is.na(idx)
    out$hurst_exponent[matched] <- hurst_results$hurst_exponent[idx[matched]]
    out$hurst_exponent_note[matched] <- hurst_results$hurst_exponent_note[idx[matched]]
    empty_note <- is.na(out$hurst_exponent_note) | !nzchar(out$hurst_exponent_note)
    missing_note <- matched & eligible & is.na(out$hurst_exponent) & empty_note
    out$hurst_exponent_note[missing_note] <- "Hurst exponent could not be computed for this series."
    out$hurst_exponent_note[!matched & eligible] <- complexity_hurst_failed_note()
    return(out)
  }

  if (identical(status, "failed")) {
    out$hurst_exponent[eligible] <- NA_real_
    out$hurst_exponent_note[eligible] <- complexity_hurst_failed_note()
  } else if (identical(status, "running")) {
    out$hurst_exponent[eligible] <- NA_real_
    out$hurst_exponent_note[eligible] <- complexity_hurst_pending_note()
  }
  out
}

compute_dfa_higuchi_curve_one_subject <- function(data, parameters) {
  subject_id <- as.character(data$id[[1L]])
  regular <- regularize_cgm_series(data, max_gap_intervals = parameters$max_gap_intervals)
  values <- regular$glucose[is.finite(regular$glucose)]
  if (length(values) < parameters$min_points) {
    return(empty_complexity_curve_rows())
  }
  curves <- empty_complexity_curve_rows()

  dfa <- compute_dfa_details(values)
  if (is.data.frame(dfa$curve) && nrow(dfa$curve)) {
    curves <- rbind(
      curves,
      make_complexity_curve_rows(
        id = subject_id,
        curve_metric = "dfa",
        scale_value = dfa$curve$scale_value,
        metric_value = dfa$curve$metric_value,
        note = if (is.na(dfa$value)) "DFA alpha could not be computed for this series." else "",
        derived_scalar_label = "DFA alpha",
        derived_scalar_value = dfa$value
      )
    )
  }

  higuchi <- compute_higuchi_details(values, parameters$higuchi_kmax)
  if (is.data.frame(higuchi$curve) && nrow(higuchi$curve)) {
    curves <- rbind(
      curves,
      make_complexity_curve_rows(
        id = subject_id,
        curve_metric = "higuchi",
        scale_value = higuchi$curve$scale_value,
        metric_value = higuchi$curve$metric_value,
        note = if (is.na(higuchi$value)) "Higuchi fractal dimension could not be computed for this series." else "",
        derived_scalar_label = "Higuchi fractal dimension",
        derived_scalar_value = higuchi$value
      )
    )
  }
  row.names(curves) <- NULL
  curves
}

compute_complexity_dfa_higuchi_curves <- function(data, parameters) {
  required <- c("id", "timestamp", "glucose")
  if (!all(required %in% names(data))) {
    stop("Complexity curve metrics require standardized columns: id, timestamp, and glucose.", call. = FALSE)
  }
  subjects <- split_complexity_subjects(data)
  if (!length(subjects)) {
    return(empty_complexity_curve_rows())
  }
  rows <- lapply(subjects, function(subject_data) {
    compute_dfa_higuchi_curve_one_subject(subject_data, parameters)
  })
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) {
    return(empty_complexity_curve_rows())
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

compute_one_complexity_subject <- function(data, parameters, include_mse = FALSE) {
  compute_complexity_subject_result(data, parameters, include_mse = include_mse)$metrics
}

compute_mse_curve_one_subject <- function(data, parameters) {
  subject_id <- as.character(data$id[[1L]])
  regular <- regularize_cgm_series(data, max_gap_intervals = parameters$max_gap_intervals)
  values <- regular$glucose[is.finite(regular$glucose)]
  if (length(values) < parameters$min_points) {
    return(empty_complexity_curve_rows())
  }
  glucose_sd <- stats::sd(values, na.rm = TRUE)
  if (!is.finite(glucose_sd) || glucose_sd <= 0) {
    return(empty_complexity_curve_rows())
  }
  mse <- compute_cgmanalyzer_mse(
    values,
    scale_max = parameters$mse_scale_max,
    embedding_dimension = parameters$embedding_dimension,
    tolerance = 0.15
  )
  scales <- mse$scales
  if (!is.data.frame(scales) || !nrow(scales)) {
    return(empty_complexity_curve_rows())
  }
  scales <- scales[is.finite(scales$Scale) & is.finite(scales$SampleEntropy), , drop = FALSE]
  if (!nrow(scales)) {
    return(empty_complexity_curve_rows())
  }
  make_complexity_curve_rows(
    id = subject_id,
    curve_metric = "mse",
    scale_value = scales$Scale,
    metric_value = scales$SampleEntropy,
    note = mse$note %||% ""
  )
}

#' Compute core complexity metrics
#'
#' @param data Standardized CGM analysis data.
#' @param min_points Minimum finite regularized glucose values required.
#' @param entropy_bin_width Glucose bin width for Shannon entropy.
#' @param embedding_dimension Embedding dimension for entropy metrics.
#' @param mse_scale_max Maximum scale for multiscale sample entropy.
#' @param higuchi_kmax Maximum k for Higuchi fractal dimension.
#' @param max_gap_intervals Maximum gap intervals to interpolate during regularization.
#' @param include_mse Whether to compute multiscale sample entropy synchronously.
#'
#' @return One row per Subject ID with complexity metrics and eligibility details.
#' @noRd
compute_complexity_metrics <- function(
    data,
    min_points = 100,
    entropy_bin_width = 10,
    embedding_dimension = 2,
    mse_scale_max = 5,
    higuchi_kmax = 8,
    max_gap_intervals = 4,
    include_mse = FALSE) {
  required <- c("id", "timestamp", "glucose")
  if (!all(required %in% names(data))) {
    stop("Complexity metrics require standardized columns: id, timestamp, and glucose.", call. = FALSE)
  }
  parameters <- complexity_default_parameters(
    min_points = min_points,
    entropy_bin_width = entropy_bin_width,
    embedding_dimension = embedding_dimension,
    mse_scale_max = mse_scale_max,
    higuchi_kmax = higuchi_kmax,
    max_gap_intervals = max_gap_intervals
  )
  if (!nrow(data)) {
    return(complexity_result_columns())
  }
  subjects <- split_complexity_subjects(data)
  if (!length(subjects)) {
    return(complexity_result_columns())
  }
  rows <- lapply(subjects, function(subject_data) {
    compute_one_complexity_subject(
      subject_data,
      parameters,
      include_mse = include_mse
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

compute_complexity_mse_metrics <- function(
    data,
    min_points = 100,
    entropy_bin_width = 10,
    embedding_dimension = 2,
    mse_scale_max = 5,
    higuchi_kmax = 8,
    max_gap_intervals = 4) {
  results <- compute_complexity_metrics(
    data,
    min_points = min_points,
    entropy_bin_width = entropy_bin_width,
    embedding_dimension = embedding_dimension,
    mse_scale_max = mse_scale_max,
    higuchi_kmax = higuchi_kmax,
    max_gap_intervals = max_gap_intervals,
    include_mse = TRUE
  )
  if (!is.data.frame(results) || !nrow(results)) {
    return(data.frame(
      id = character(),
      multiscale_sample_entropy = numeric(),
      multiscale_sample_entropy_note = character(),
      stringsAsFactors = FALSE
    ))
  }
  results[, c("id", "multiscale_sample_entropy", "multiscale_sample_entropy_note"), drop = FALSE]
}

compute_complexity_mse_bundle <- function(data, parameters) {
  compute_complexity_bundle(data, parameters, include_mse = TRUE)
}

compute_complexity_bundle <- function(data, parameters, include_mse = TRUE) {
  required <- c("id", "timestamp", "glucose")
  if (!all(required %in% names(data))) {
    stop("Complexity metrics require standardized columns: id, timestamp, and glucose.", call. = FALSE)
  }
  subjects <- split_complexity_subjects(data)
  if (!length(subjects)) {
    return(list(metrics = complexity_result_columns(), curves = data.frame()))
  }
  subject_results <- lapply(subjects, function(subject_data) {
    compute_complexity_subject_result(subject_data, parameters, include_mse = include_mse)
  })
  metrics <- do.call(rbind, lapply(subject_results, `[[`, "metrics"))
  row.names(metrics) <- NULL
  curves <- lapply(subject_results, `[[`, "curves")
  curves <- curves[vapply(curves, nrow, integer(1)) > 0L]
  curves <- if (length(curves)) {
    out <- do.call(rbind, curves)
    row.names(out) <- NULL
    out
  } else {
    empty_complexity_curve_rows()
  }
  list(metrics = metrics, curves = curves)
}

merge_complexity_mse_results <- function(results, mse_metrics = NULL, status = "idle") {
  if (!is.data.frame(results) || !nrow(results)) {
    return(results)
  }
  out <- results
  status <- status %||% "idle"
  if (identical(status, "complete") && is.data.frame(mse_metrics) && nrow(mse_metrics)) {
    idx <- match(out$id, mse_metrics$id)
    matched <- !is.na(idx)
    out$multiscale_sample_entropy <- NA_real_
    out$multiscale_sample_entropy_note[matched] <- mse_metrics$multiscale_sample_entropy_note[idx[matched]]
    empty_note <- is.na(out$multiscale_sample_entropy_note) | !nzchar(out$multiscale_sample_entropy_note)
    out$multiscale_sample_entropy_note[matched & empty_note] <- ""
    out$multiscale_sample_entropy_note[!matched] <- complexity_mse_failed_note()
  } else if (identical(status, "failed")) {
    out$multiscale_sample_entropy <- NA_real_
    out$multiscale_sample_entropy_note <- complexity_mse_failed_note()
  } else if (identical(status, "running")) {
    out$multiscale_sample_entropy <- NA_real_
    out$multiscale_sample_entropy_note <- complexity_mse_pending_note()
  }
  out
}

compute_complexity_mse_curves <- function(
    data,
    min_points = 100,
    entropy_bin_width = 10,
    embedding_dimension = 2,
    mse_scale_max = 5,
    higuchi_kmax = 8,
    max_gap_intervals = 4) {
  required <- c("id", "timestamp", "glucose")
  if (!all(required %in% names(data))) {
    stop("Complexity MSE curves require standardized columns: id, timestamp, and glucose.", call. = FALSE)
  }
  parameters <- complexity_default_parameters(
    min_points = min_points,
    entropy_bin_width = entropy_bin_width,
    embedding_dimension = embedding_dimension,
    mse_scale_max = mse_scale_max,
    higuchi_kmax = higuchi_kmax,
    max_gap_intervals = max_gap_intervals
  )
  if (!nrow(data)) {
    return(data.frame())
  }
  subjects <- split_complexity_subjects(data)
  if (!length(subjects)) {
    return(data.frame())
  }
  rows <- lapply(subjects, function(subject_data) {
    compute_mse_curve_one_subject(subject_data, parameters)
  })
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) {
    return(data.frame())
  }
  out <- do.call(rbind, rows)
  out <- out[out$curve_metric == "mse", , drop = FALSE]
  row.names(out) <- NULL
  out
}

filter_complexity_data <- function(
  data,
  subject = "",
  group = ""
) {
  if (!is.data.frame(data) || !nrow(data)) {
    return(data)
  }
  group <- normalize_filter_value(group)
  if (nzchar(group) && "group" %in% names(data)) {
    data <- data[as.character(data$group) == group, , drop = FALSE]
  }
  filter_data_by_subject_selection(data, subject)
}

complexity_filtered_subject_ids <- function(
  data,
  subject = "",
  group = ""
) {
  if (!is.data.frame(data) || !"id" %in% names(data)) {
    return(character())
  }
  data <- filter_complexity_data(
    data,
    subject = subject,
    group = group
  )
  subject_id_values(data)
}

filter_complexity_results <- function(
  results,
  data = NULL,
  subject = "",
  group = ""
) {
  if (!is.data.frame(results) || !nrow(results) || !"id" %in% names(results)) {
    return(results)
  }
  subject <- normalize_filter_value(subject)
  group <- normalize_filter_value(group)
  if (!nzchar(subject) && !nzchar(group)) {
    return(results)
  }
  ids <- complexity_filtered_subject_ids(
    data,
    subject = subject,
    group = group
  )
  if (!length(ids)) {
    return(results[0L, , drop = FALSE])
  }
  results[as.character(results$id) %in% ids, , drop = FALSE]
}

filter_complexity_curves <- function(
  curves,
  data = NULL,
  subject = "",
  group = ""
) {
  if (!is.data.frame(curves) || !nrow(curves) || !"id" %in% names(curves)) {
    return(curves)
  }
  subject <- normalize_filter_value(subject)
  group <- normalize_filter_value(group)
  if (!nzchar(subject) && !nzchar(group)) {
    return(curves)
  }
  ids <- complexity_filtered_subject_ids(
    data,
    subject = subject,
    group = group
  )
  if (!length(ids)) {
    return(curves[0L, , drop = FALSE])
  }
  curves[as.character(curves$id) %in% ids, , drop = FALSE]
}

complexity_compute_key <- function(data, parameters) {
  paste(
    utils::capture.output(utils::str(list(
      data = cgm_data_signature(data),
      parameters = parameters
    ))),
    collapse = "\n"
  )
}

complexity_group_lookup <- function(data) {
  if (!is.data.frame(data) || !"id" %in% names(data) || !"group" %in% names(data)) {
    return(NULL)
  }
  values <- data.frame(
    id = as.character(data$id),
    group = as.character(data$group),
    stringsAsFactors = FALSE
  )
  values$group <- trimws(values$group)
  values <- values[!is.na(values$id) & nzchar(values$id) & !is.na(values$group) & nzchar(values$group), , drop = FALSE]
  if (!nrow(values)) {
    return(NULL)
  }
  values <- values[!duplicated(values$id), , drop = FALSE]
  stats::setNames(values$group, values$id)
}

prepare_complexity_metrics_display <- function(results, data = NULL, show_subject_id = NULL) {
  catalog <- complexity_metric_catalog()
  if (!is.data.frame(results) || !nrow(results)) {
    return(data.frame(
      `Subject ID` = character(),
      Metric = character(),
      Value = numeric(),
      `Units / scale` = character(),
      `Scale / parameters` = character(),
      Definition = character(),
      Notes = character(),
      check.names = FALSE
    ))
  }
  group_lookup <- complexity_group_lookup(data)
  rows <- lapply(seq_len(nrow(results)), function(i) {
    result <- results[i, , drop = FALSE]
    values <- as.numeric(unlist(result[1L, catalog$raw_name], use.names = FALSE))
    metric_notes <- vapply(catalog$raw_name, function(metric) {
      note_col <- paste0(metric, "_note")
      specific_note <- if (note_col %in% names(result)) as.character(result[[note_col]][[1L]]) else ""
      if (nzchar(specific_note)) {
        specific_note
      } else if (!result$eligible) {
        result$notes
      } else if (is.na(as.numeric(result[[metric]][[1L]]))) {
        "Could not compute for this series."
      } else {
        ""
      }
    }, character(1))
    subject <- as.character(result$id)
    row <- data.frame(
      `Subject ID` = result$id,
      Metric = catalog$metric,
      Value = round(values, 4),
      `Units / scale` = catalog$units,
      `Scale / parameters` = catalog$scale,
      Definition = catalog$definition,
      Notes = metric_notes,
      order = catalog$order,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (!is.null(group_lookup)) {
      row$Group <- unname(group_lookup[subject])
    }
    row
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$`Subject ID`, out$order), , drop = FALSE]
  out$order <- NULL
  if ("Group" %in% names(out)) {
    out <- out[, c("Subject ID", "Group", setdiff(names(out), c("Subject ID", "Group"))), drop = FALSE]
  }
  if (!is.null(data) && !show_subject_id_for_display(data, show_subject_id)) {
    out <- out[, setdiff(names(out), "Subject ID"), drop = FALSE]
  }
  row.names(out) <- NULL
  out
}

prepare_complexity_export <- function(results, curves = NULL, data = NULL, show_subject_id = NULL) {
  scalar <- prepare_complexity_metrics_display(results, data, show_subject_id = show_subject_id)
  if (nrow(scalar)) {
    scalar$Output <- "Scalar metric"
    scalar$`Scale variable` <- ""
    scalar$`Scale value` <- NA_real_
    scalar$`Derived scalar` <- ""
    scalar$`Derived scalar value` <- NA_real_
    if (!"Scale / parameters" %in% names(scalar)) {
      scalar$`Scale / parameters` <- ""
    }
    scalar <- scalar[, c(
      intersect(c("Subject ID", "Group"), names(scalar)),
      "Output",
      "Metric",
      "Value",
      "Units / scale",
      "Scale / parameters",
      "Scale variable",
      "Scale value",
      "Derived scalar",
      "Derived scalar value",
      "Definition",
      "Notes"
    ), drop = FALSE]
  }

  scale_values <- prepare_complexity_curve_plot_data(curves, data, show_subject_id = show_subject_id)
  if (nrow(scale_values)) {
    scale_values$Output <- "Scale curve"
    scale_values$Metric <- scale_values[["Curve metric"]]
    scale_values$Value <- scale_values[["Metric value"]]
    scale_values$`Units / scale` <- scale_values[["Value label"]]
    scale_values$`Scale / parameters` <- scale_values[["Scale variable"]]
    scale_values$Definition <- "Scale-level complexity curve value."
    scale_values$Notes <- ""
    scale_values <- scale_values[, c(
      intersect(c("Subject ID", "Group"), names(scale_values)),
      "Output",
      "Metric",
      "Value",
      "Units / scale",
      "Scale / parameters",
      "Scale variable",
      "Scale value",
      "Derived scalar",
      "Derived scalar value",
      "Definition",
      "Notes"
    ), drop = FALSE]
  }

  rows <- list()
  if (exists("scalar") && is.data.frame(scalar) && nrow(scalar)) {
    rows[[length(rows) + 1L]] <- scalar
  }
  if (exists("scale_values") && is.data.frame(scale_values) && nrow(scale_values)) {
    rows[[length(rows) + 1L]] <- scale_values
  }
  if (!length(rows)) {
    return(data.frame())
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

complexity_metric_filter_choices <- function() {
  metrics <- complexity_metric_catalog()$metric
  c(stats::setNames(all_filter_value(), "All metrics"), stats::setNames(metrics, metrics))
}

complexity_visual_mode_choices <- function() {
  c(
    "Metric summary" = "metric_summary",
    "Scale curves" = "scale_curves"
  )
}

complexity_curve_filter_choices <- function() {
  catalog <- complexity_curve_catalog()
  c(stats::setNames(all_filter_value(), "All scale curves"), stats::setNames(catalog$curve_metric, catalog$label))
}

complexity_mse_status_text <- function(status = "idle") {
  switch(
    status %||% "idle",
    running = "MSE running: multiscale entropy is calculating in the background.",
    complete = "MSE available: multiscale entropy scale curves have been added to the current results.",
    failed = "MSE could not compute for the current selection. Fast complexity metrics remain available.",
    ""
  )
}

complexity_status_text <- function(status = "idle") {
  switch(
    status %||% "idle",
    running = "Complexity summary running: eligibility and Shannon entropy are calculating.",
    complete = "Complexity summary available: eligibility and Shannon entropy are available.",
    failed = "Complexity summary could not compute for the current selection.",
    ""
  )
}

complexity_scalar_status_text <- function(status = "idle") {
  switch(
    status %||% "idle",
    running = "Hurst exponent is calculating.",
    complete = "Hurst exponent is available.",
    failed = "Hurst exponent could not compute for the current selection.",
    ""
  )
}

complexity_curve_status_text <- function(status = "idle") {
  switch(
    status %||% "idle",
    running = "DFA/Higuchi curves running: scale curves are calculating.",
    complete = "DFA/Higuchi curves available.",
    failed = "DFA/Higuchi curves could not compute for the current selection.",
    ""
  )
}

complexity_metric_note <- function(result, metric) {
  note_col <- paste0(metric, "_note")
  specific_note <- if (note_col %in% names(result)) as.character(result[[note_col]][[1L]]) else ""
  if (nzchar(specific_note)) {
    specific_note
  } else if (!isTRUE(result$eligible[[1L]])) {
    as.character(result$notes[[1L]])
  } else if (is.na(as.numeric(result[[metric]][[1L]]))) {
    "Could not compute for this series."
  } else {
    ""
  }
}

prepare_complexity_plot_data <- function(
  results,
  data = NULL,
  metric = all_filter_value(),
  show_subject_id = NULL
) {
  catalog <- complexity_metric_catalog()
  if (!is.data.frame(results) || !nrow(results)) {
    return(data.frame(
      `Subject ID` = character(),
      Metric = character(),
      Value = numeric(),
      `Units / scale` = character(),
      Notes = character(),
      Tooltip = character(),
      check.names = FALSE
    ))
  }

  show_subject_id <- is.null(data) || show_subject_id_for_display(data, show_subject_id)
  group_lookup <- complexity_group_lookup(data)
  rows <- lapply(seq_len(nrow(results)), function(i) {
    result <- results[i, , drop = FALSE]
    values <- as.numeric(unlist(result[1L, catalog$raw_name], use.names = FALSE))
    notes <- vapply(catalog$raw_name, function(name) complexity_metric_note(result, name), character(1))
    subject_id <- as.character(result$id[[1L]])
    subject <- if (show_subject_id) subject_id else "Analysis data"
    row <- data.frame(
      `Subject ID` = subject,
      Metric = catalog$metric,
      Value = values,
      `Units / scale` = catalog$units,
      `Scale / parameters` = catalog$scale,
      Notes = notes,
      order = catalog$order,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (!is.null(group_lookup)) {
      row$Group <- unname(group_lookup[subject_id])
    }
    row
  })
  out <- do.call(rbind, rows)
  out <- out[is.finite(out$Value), , drop = FALSE]

  selected <- normalize_filter_value(metric)
  if (nzchar(selected)) {
    out <- out[out$Metric == selected, , drop = FALSE]
  }
  if (!nrow(out)) {
    out$Tooltip <- character()
    out$order <- NULL
    return(out)
  }

  out$Metric <- factor(out$Metric, levels = catalog$metric)
  out$Tooltip <- paste0(
    "Subject ID: ", out[["Subject ID"]],
    if ("Group" %in% names(out)) paste0("<br>Group: ", out$Group) else "",
    "<br>Metric: ", as.character(out$Metric),
    "<br>Value: ", round(out$Value, 4),
    "<br>Units / scale: ", out[["Units / scale"]],
    "<br>Scale / parameters: ", out[["Scale / parameters"]],
    ifelse(nzchar(out$Notes), paste0("<br>Notes: ", out$Notes), "")
  )
  out <- out[order(out$order, out[["Subject ID"]]), , drop = FALSE]
  out$order <- NULL
  row.names(out) <- NULL
  out
}

prepare_complexity_curve_plot_data <- function(
  curves,
  data = NULL,
  curve_metric = all_filter_value(),
  show_subject_id = NULL
) {
  if (!is.data.frame(curves) || !nrow(curves)) {
    return(data.frame(
      `Subject ID` = character(),
      `Curve metric` = character(),
      `Scale variable` = character(),
      `Scale value` = numeric(),
      `Metric value` = numeric(),
      `Value label` = character(),
      `Derived scalar` = character(),
      `Derived scalar value` = numeric(),
      Tooltip = character(),
      check.names = FALSE
    ))
  }
  selected <- normalize_filter_value(curve_metric)
  if (nzchar(selected) && "curve_metric" %in% names(curves)) {
    curves <- curves[curves$curve_metric == selected, , drop = FALSE]
  }
  if (!nrow(curves)) {
    return(data.frame(
      `Subject ID` = character(),
      `Curve metric` = character(),
      `Scale variable` = character(),
      `Scale value` = numeric(),
      `Metric value` = numeric(),
      `Value label` = character(),
      `Derived scalar` = character(),
      `Derived scalar value` = numeric(),
      Tooltip = character(),
      check.names = FALSE
    ))
  }
  show_subject_id <- is.null(data) || show_subject_id_for_display(data, show_subject_id)
  group_lookup <- complexity_group_lookup(data)
  subject_ids <- as.character(curves$id)
  catalog <- complexity_curve_catalog()
  curve_labels <- stats::setNames(catalog$label, catalog$curve_metric)
  derived_labels <- if ("derived_scalar_label" %in% names(curves)) {
    as.character(curves$derived_scalar_label)
  } else {
    rep("", nrow(curves))
  }
  derived_values <- if ("derived_scalar_value" %in% names(curves)) {
    as.numeric(curves$derived_scalar_value)
  } else {
    rep(NA_real_, nrow(curves))
  }
  out <- data.frame(
    `Subject ID` = if (show_subject_id) subject_ids else "Analysis data",
    `Curve metric` = unname(curve_labels[as.character(curves$curve_metric)]),
    `Scale variable` = as.character(curves$scale_variable),
    `Scale value` = as.numeric(curves$scale_value),
    `Metric value` = as.numeric(curves$metric_value),
    `Value label` = as.character(curves$value_label),
    `Derived scalar` = derived_labels,
    `Derived scalar value` = derived_values,
    Notes = as.character(curves$note %||% ""),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!is.null(group_lookup)) {
    out$Group <- unname(group_lookup[subject_ids])
  }
  out <- out[is.finite(out[["Scale value"]]) & is.finite(out[["Metric value"]]), , drop = FALSE]
  if (!nrow(out)) {
    out$Tooltip <- character()
    keep <- intersect(c("Subject ID", "Group", "Curve metric", "Scale variable", "Scale value", "Metric value", "Value label", "Derived scalar", "Derived scalar value", "Tooltip"), names(out))
    return(out[, keep, drop = FALSE])
  }
  out$Tooltip <- paste0(
    "Subject ID: ", out[["Subject ID"]],
    if ("Group" %in% names(out)) paste0("<br>Group: ", out$Group) else "",
    "<br>Curve metric: ", out[["Curve metric"]],
    "<br>", out[["Scale variable"]], ": ", out[["Scale value"]],
    "<br>", out[["Value label"]], ": ", round(out[["Metric value"]], 4),
    ifelse(
      nzchar(out[["Derived scalar"]]) & is.finite(out[["Derived scalar value"]]),
      paste0("<br>", out[["Derived scalar"]], ": ", round(out[["Derived scalar value"]], 4)),
      ""
    ),
    ifelse(nzchar(out$Notes), paste0("<br>Notes: ", out$Notes), "")
  )
  out <- out[order(out[["Curve metric"]], out[["Subject ID"]], out[["Scale value"]]), , drop = FALSE]
  row.names(out) <- NULL
  keep <- intersect(c("Subject ID", "Group", "Curve metric", "Scale variable", "Scale value", "Metric value", "Value label", "Derived scalar", "Derived scalar value", "Tooltip"), names(out))
  out[, keep, drop = FALSE]
}

prepare_mse_curve_plot_data <- function(curves, data = NULL, show_subject_id = NULL) {
  prepare_complexity_curve_plot_data(curves, data, curve_metric = "mse", show_subject_id = show_subject_id)
}

create_complexity_summary_plot <- function(plot_data, metric = all_filter_value()) {
  if (!is.data.frame(plot_data) || !nrow(plot_data)) {
    return(empty_plot("No finite complexity metric values are available for the selected data."))
  }

  selected <- normalize_filter_value(metric)
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = `Subject ID`,
      y = Value,
      color = `Subject ID`,
      text = Tooltip
    )
  ) +
    ggplot2::geom_point(size = 2.8, alpha = 0.9, na.rm = TRUE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.18, add = 0.02)) +
    ggplot2::labs(x = "Subject ID", y = "Metric value", color = "Subject ID") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
    )

  if (!nzchar(selected)) {
    plot <- plot +
      ggplot2::facet_wrap(~Metric, scales = "free_y")
  } else {
    plot <- plot +
      ggplot2::labs(y = selected) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "Subject ID"))
  }

  plot
}

complexity_curve_annotation_text <- function(plot_data) {
  required <- c("Subject ID", "Derived scalar", "Derived scalar value")
  if (!is.data.frame(plot_data) || !all(required %in% names(plot_data))) {
    return("")
  }
  annotations <- plot_data[
    nzchar(plot_data[["Derived scalar"]]) & is.finite(plot_data[["Derived scalar value"]]),
    required,
    drop = FALSE
  ]
  if (!nrow(annotations)) {
    return("")
  }
  annotations <- unique(annotations)
  labels <- unique(annotations[["Derived scalar"]])
  parts <- vapply(labels, function(label) {
    values <- annotations[annotations[["Derived scalar"]] == label, , drop = FALSE]
    values <- values[order(values[["Subject ID"]]), , drop = FALSE]
    if (nrow(values) <= 4L) {
      formatted <- paste0(
        values[["Subject ID"]],
        "=",
        format(round(values[["Derived scalar value"]], 4), trim = TRUE)
      )
      paste0(label, ": ", paste(formatted, collapse = "; "))
    } else {
      finite <- values[["Derived scalar value"]]
      paste0(
        label,
        " range: ",
        format(round(min(finite), 4), trim = TRUE),
        "-",
        format(round(max(finite), 4), trim = TRUE),
        " across ",
        nrow(values),
        " Subject IDs"
      )
    }
  }, character(1))
  paste(parts, collapse = " | ")
}

create_complexity_scale_curve_plot <- function(plot_data) {
  if (!is.data.frame(plot_data) || !nrow(plot_data)) {
    return(empty_plot("No finite scale-curve values are available for the selected data."))
  }

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = `Scale value`,
      y = `Metric value`,
      color = `Subject ID`,
      group = `Subject ID`,
      text = Tooltip
    )
  ) +
    ggplot2::geom_line(linewidth = 0.55, alpha = 0.85, na.rm = TRUE) +
    ggplot2::geom_point(size = 2.4, alpha = 0.95, na.rm = TRUE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.12, add = 0.02)) +
    ggplot2::labs(x = "Scale", y = "Metric value", color = "Subject ID") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  if (length(unique(plot_data[["Curve metric"]])) > 1L) {
    plot <- plot + ggplot2::facet_wrap(~`Curve metric`, scales = "free")
  } else {
    plot <- plot + ggplot2::labs(
      x = unique(plot_data[["Scale variable"]])[[1L]],
      y = unique(plot_data[["Value label"]])[[1L]]
    )
  }

  annotation <- complexity_curve_annotation_text(plot_data)
  if (nzchar(annotation)) {
    plot <- plot + ggplot2::labs(subtitle = annotation)
  }

  plot
}

create_mse_curve_plot <- function(plot_data) {
  create_complexity_scale_curve_plot(plot_data)
}

complexity_plot_height <- function(metric = all_filter_value()) {
  selected <- normalize_filter_value(metric)
  if (nzchar(selected)) 440L else 620L
}

complexity_visual_plot_height <- function(mode = "metric_summary", metric = all_filter_value()) {
  if (identical(mode, "scale_curves")) 500L else complexity_plot_height(metric)
}

complexity_summary_cards <- function(results, parameters) {
  if (!is.data.frame(results) || !nrow(results)) {
    return(data.frame(Label = character(), Value = character(), stringsAsFactors = FALSE))
  }
  intervals <- results$interval_minutes[is.finite(results$interval_minutes)]
  data.frame(
    Label = c("Eligible Subject IDs", "Needs review", "Median interval", "Parameters"),
    Value = c(
      format_count(sum(results$eligible, na.rm = TRUE)),
      format_count(sum(!results$eligible, na.rm = TRUE)),
      if (length(intervals)) paste0(format(round(stats::median(intervals), 2), trim = TRUE), " min") else "Not available",
      complexity_parameter_label(parameters)
    ),
    stringsAsFactors = FALSE
  )
}
