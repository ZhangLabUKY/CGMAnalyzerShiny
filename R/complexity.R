complexity_metric_catalog <- function() {
  data.frame(
    raw_name = c(
      "shannon_entropy",
      "sample_entropy",
      "approximate_entropy",
      "multiscale_sample_entropy",
      "hurst_exponent",
      "dfa_alpha",
      "higuchi_fractal_dimension"
    ),
    metric = c(
      "Shannon entropy",
      "Sample entropy",
      "Approximate entropy",
      "Multiscale sample entropy",
      "Hurst exponent",
      "DFA alpha",
      "Higuchi fractal dimension"
    ),
    units = c("0-1 normalized", "Unitless", "Unitless", "Unitless", "Unitless", "Unitless", "Fractal dimension"),
    definition = c(
      "Normalized entropy of glucose values across bins; higher values indicate a wider glucose distribution.",
      "Regularity measure; higher values indicate less predictable glucose patterns.",
      "Regularity measure similar to sample entropy; higher values indicate less predictable glucose patterns.",
      "Mean sample entropy across coarse-grained time scales; higher values indicate less predictable multi-scale patterns.",
      "Long-range dependence estimate; values above 0.5 suggest more persistent glucose trends.",
      "Scaling exponent from detrended fluctuation analysis; higher values suggest stronger long-range correlation.",
      "Curve-complexity estimate from Higuchi's method; higher values indicate a more irregular glucose trace."
    ),
    order = seq_len(7),
    stringsAsFactors = FALSE
  )
}

complexity_default_parameters <- function(
    min_points = 100,
    entropy_bin_width = 10,
    embedding_dimension = 2,
    tolerance_multiplier = 0.2,
    mse_scale_max = 5,
    higuchi_kmax = 8,
    max_gap_intervals = 4) {
  list(
    min_points = as.integer(min_points),
    entropy_bin_width = as.numeric(entropy_bin_width),
    embedding_dimension = as.integer(embedding_dimension),
    tolerance_multiplier = as.numeric(tolerance_multiplier),
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
    "; r=", format(parameters$tolerance_multiplier, trim = TRUE), " x SD",
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
  counts <- hist(x, breaks = breaks, plot = FALSE, include.lowest = TRUE, right = TRUE)$counts
  probabilities <- counts[counts > 0] / sum(counts)
  if (length(probabilities) <= 1L) {
    return(0)
  }
  entropy <- -sum(probabilities * log(probabilities))
  entropy / log(length(probabilities))
}

safe_sample_entropy <- function(x, embedding_dimension, tolerance) {
  tryCatch(
    pracma::sample_entropy(x, edim = embedding_dimension, r = tolerance, tau = 1),
    error = function(error) NA_real_
  )
}

safe_approximate_entropy <- function(x, embedding_dimension, tolerance) {
  tryCatch(
    pracma::approx_entropy(x, edim = embedding_dimension, r = tolerance, elag = 1),
    error = function(error) NA_real_
  )
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

compute_cgmanalyzer_mse <- function(x, scale_max = 5, embedding_dimension = 2, tolerance = 0.15) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(list(value = NA_real_, note = "No finite glucose values available for multiscale entropy."))
  }
  scale_max <- as.integer(scale_max)
  if (is.na(scale_max) || scale_max < 1L) {
    scale_max <- 5L
  }
  if (length(x) < scale_max * 10L) {
    return(list(value = NA_real_, note = paste0("Needs at least ", scale_max * 10L, " usable points for multiscale entropy.")))
  }
  if (!requireNamespace("CGManalyzer", quietly = TRUE) || !"MSEbyC.fn" %in% getNamespaceExports("CGManalyzer")) {
    return(list(value = NA_real_, note = "Multiscale sample entropy is not available in this R session."))
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
    return(list(value = NA_real_, note = "Multiscale sample entropy could not be computed for this series."))
  }
  values <- result$SampleEntropy
  values <- values[is.finite(values)]
  if (!length(values)) {
    return(list(value = NA_real_, note = "Multiscale sample entropy did not return finite values."))
  }
  list(value = mean(values), note = "")
}

safe_dfa_alpha <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 32L || length(unique(x)) <= 1L) {
    return(NA_real_)
  }
  y <- cumsum(x - mean(x))
  windows <- unique(floor(exp(seq(log(8), log(floor(n / 4)), length.out = 8))))
  windows <- windows[windows >= 8L & windows < n / 2]
  fluctuation <- vapply(windows, function(window) {
    segments <- floor(n / window)
    if (segments < 2L) {
      return(NA_real_)
    }
    rms <- vapply(seq_len(segments), function(segment) {
      idx <- ((segment - 1L) * window + 1L):(segment * window)
      fit <- stats::lm(y[idx] ~ idx)
      sqrt(mean(stats::residuals(fit)^2))
    }, numeric(1))
    sqrt(mean(rms^2, na.rm = TRUE))
  }, numeric(1))
  keep <- is.finite(fluctuation) & fluctuation > 0 & windows > 0
  if (sum(keep) < 2L) {
    return(NA_real_)
  }
  as.numeric(stats::coef(stats::lm(log(fluctuation[keep]) ~ log(windows[keep])))[[2L]])
}

safe_higuchi_fd <- function(x, kmax = 8) {
  x <- x[is.finite(x)]
  n <- length(x)
  kmax <- as.integer(kmax)
  if (is.na(kmax) || kmax < 2L) {
    kmax <- 8L
  }
  if (n < (kmax * 2L) || length(unique(x)) <= 1L) {
    return(NA_real_)
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
  if (sum(keep) < 2L) {
    return(NA_real_)
  }
  as.numeric(stats::coef(stats::lm(log(lengths[keep]) ~ log(1 / k_values[keep])))[[2L]])
}

compute_one_complexity_subject <- function(data, parameters) {
  subject_id <- as.character(data$id[[1L]])
  readings <- nrow(data)
  gaps <- tryCatch(detect_gap_periods(data), error = function(error) empty_gap_periods())
  gap_count <- if (is.data.frame(gaps)) nrow(gaps) else NA_integer_
  regular <- regularize_cgm_series(data, max_gap_intervals = parameters$max_gap_intervals)
  interval <- attr(regular, "interval_minutes", exact = TRUE)
  values <- regular$glucose[is.finite(regular$glucose)]
  usable_points <- length(values)
  eligible <- usable_points >= parameters$min_points
  note <- if (eligible) {
    "Eligible"
  } else {
    paste0("Needs at least ", parameters$min_points, " usable regularized points.")
  }

  out <- data.frame(
    id = subject_id,
    readings = readings,
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
    multiscale_sample_entropy_note = "",
    dfa_alpha_note = "",
    higuchi_fractal_dimension_note = "",
    stringsAsFactors = FALSE
  )

  if (!eligible) {
    return(out)
  }

  tolerance <- parameters$tolerance_multiplier * stats::sd(values, na.rm = TRUE)
  if (!is.finite(tolerance) || tolerance <= 0) {
    out$notes <- "Eligible, but entropy tolerance could not be computed because glucose variability is zero."
    out$shannon_entropy <- shannon_entropy_normalized(values, parameters$entropy_bin_width)
    return(out)
  }

  out$shannon_entropy <- shannon_entropy_normalized(values, parameters$entropy_bin_width)
  out$sample_entropy <- safe_sample_entropy(values, parameters$embedding_dimension, tolerance)
  out$approximate_entropy <- safe_approximate_entropy(values, parameters$embedding_dimension, tolerance)
  mse <- compute_cgmanalyzer_mse(
    values,
    scale_max = parameters$mse_scale_max,
    embedding_dimension = parameters$embedding_dimension,
    tolerance = 0.15
  )
  out$multiscale_sample_entropy <- mse$value
  out$multiscale_sample_entropy_note <- mse$note
  out$hurst_exponent <- safe_hurst_exponent(values)
  out$dfa_alpha <- safe_dfa_alpha(values)
  out$dfa_alpha_note <- if (is.na(out$dfa_alpha)) "DFA alpha could not be computed for this series." else ""
  out$higuchi_fractal_dimension <- safe_higuchi_fd(values, parameters$higuchi_kmax)
  out$higuchi_fractal_dimension_note <- if (is.na(out$higuchi_fractal_dimension)) "Higuchi fractal dimension could not be computed for this series." else ""
  out
}

#' Compute core complexity metrics
#'
#' @param data Standardized CGM analysis data.
#' @param min_points Minimum finite regularized glucose values required.
#' @param entropy_bin_width Glucose bin width for Shannon entropy.
#' @param embedding_dimension Embedding dimension for entropy metrics.
#' @param tolerance_multiplier Tolerance as a multiplier of glucose SD.
#' @param mse_scale_max Maximum scale for multiscale sample entropy.
#' @param higuchi_kmax Maximum k for Higuchi fractal dimension.
#' @param max_gap_intervals Maximum gap intervals to interpolate during regularization.
#'
#' @return One row per Subject ID with complexity metrics and eligibility details.
#' @noRd
compute_complexity_metrics <- function(
    data,
    min_points = 100,
    entropy_bin_width = 10,
    embedding_dimension = 2,
    tolerance_multiplier = 0.2,
    mse_scale_max = 5,
    higuchi_kmax = 8,
    max_gap_intervals = 4) {
  required <- c("id", "timestamp", "glucose")
  if (!all(required %in% names(data))) {
    stop("Complexity metrics require standardized columns: id, timestamp, and glucose.", call. = FALSE)
  }
  parameters <- complexity_default_parameters(
    min_points = min_points,
    entropy_bin_width = entropy_bin_width,
    embedding_dimension = embedding_dimension,
    tolerance_multiplier = tolerance_multiplier,
    mse_scale_max = mse_scale_max,
    higuchi_kmax = higuchi_kmax,
    max_gap_intervals = max_gap_intervals
  )
  if (!nrow(data)) {
    return(data.frame())
  }
  data <- data[!is.na(data$id) & nzchar(as.character(data$id)), , drop = FALSE]
  ids <- sort(unique(as.character(data$id)))
  if (!length(ids)) {
    return(data.frame())
  }
  rows <- lapply(ids, function(id) {
    compute_one_complexity_subject(data[data$id == id, , drop = FALSE], parameters)
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

prepare_complexity_metrics_display <- function(results, data = NULL) {
  catalog <- complexity_metric_catalog()
  if (!is.data.frame(results) || !nrow(results)) {
    return(data.frame(
      `Subject ID` = character(),
      Metric = character(),
      Value = numeric(),
      `Units / scale` = character(),
      Definition = character(),
      Notes = character(),
      check.names = FALSE
    ))
  }
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
    data.frame(
      `Subject ID` = result$id,
      Metric = catalog$metric,
      Value = round(values, 4),
      `Units / scale` = catalog$units,
      Definition = catalog$definition,
      Notes = metric_notes,
      order = catalog$order,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$`Subject ID`, out$order), , drop = FALSE]
  out$order <- NULL
  if (!is.null(data) && !subject_id_filter_available(data)) {
    out <- out[, setdiff(names(out), "Subject ID"), drop = FALSE]
  }
  row.names(out) <- NULL
  out
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
