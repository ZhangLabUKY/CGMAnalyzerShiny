default_metric_groups <- function(data) {
  groups <- c("id", "id_source", "group", "metric_period")
  groups[groups %in% names(data)]
}

split_by_columns <- function(data, columns) {
  columns <- columns[columns %in% names(data)]
  key <- do.call(paste, c(data[columns], sep = "\r"))
  split(data, key, drop = TRUE)
}

time_in_range_percent <- function(glucose, condition) {
  observed <- !is.na(glucose)
  if (!any(observed)) {
    return(NA_real_)
  }
  100 * sum(condition(glucose[observed])) / sum(observed)
}

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_real_ else min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) NA_real_ else max(x)
}

summarize_one_metric_group <- function(x, thresholds, group_columns) {
  glucose <- x$glucose
  mean_glucose <- mean(glucose, na.rm = TRUE)
  sd_glucose <- stats::sd(glucose, na.rm = TRUE)

  group_values <- x[1L, group_columns, drop = FALSE]
  data.frame(
    group_values,
    readings = sum(!is.na(glucose)),
    mean_glucose = mean_glucose,
    median_glucose = stats::median(glucose, na.rm = TRUE),
    sd_glucose = sd_glucose,
    cv_percent = if (!is.na(mean_glucose) && mean_glucose != 0) 100 * sd_glucose / mean_glucose else NA_real_,
    min_glucose = safe_min(glucose),
    max_glucose = safe_max(glucose),
    tir_percent = time_in_range_percent(glucose, function(g) g >= thresholds$tir_lower & g <= thresholds$tir_upper),
    tbr_percent = time_in_range_percent(glucose, function(g) g < thresholds$tir_lower),
    tar_percent = time_in_range_percent(glucose, function(g) g > thresholds$tir_upper),
    tbr_level2_percent = time_in_range_percent(glucose, function(g) g < thresholds$tbr_level2),
    tbr_level1_percent = time_in_range_percent(glucose, function(g) g >= thresholds$tbr_level2 & g < thresholds$tir_lower),
    tar_level1_percent = time_in_range_percent(glucose, function(g) g > thresholds$tir_upper & g <= thresholds$tar_level2),
    tar_level2_percent = time_in_range_percent(glucose, function(g) g > thresholds$tar_level2),
    gmi_percent = if (!is.na(mean_glucose)) 3.31 + 0.02392 * mean_glucose else NA_real_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

compute_base_metrics_dt <- function(data, thresholds, by) {
  dt <- data.table::as.data.table(data)
  result <- dt[, {
    glucose_values <- get("glucose")
    observed <- !is.na(glucose_values)
    n_observed <- sum(observed)
    mean_glucose <- mean(glucose_values, na.rm = TRUE)
    sd_glucose <- stats::sd(glucose_values, na.rm = TRUE)
    observed_glucose <- glucose_values[observed]

    list(
      readings = n_observed,
      mean_glucose = mean_glucose,
      median_glucose = stats::median(glucose_values, na.rm = TRUE),
      sd_glucose = sd_glucose,
      cv_percent = if (!is.na(mean_glucose) && mean_glucose != 0) 100 * sd_glucose / mean_glucose else NA_real_,
      min_glucose = safe_min(glucose_values),
      max_glucose = safe_max(glucose_values),
      tir_percent = if (n_observed) 100 * sum(observed_glucose >= thresholds$tir_lower & observed_glucose <= thresholds$tir_upper) / n_observed else NA_real_,
      tbr_percent = if (n_observed) 100 * sum(observed_glucose < thresholds$tir_lower) / n_observed else NA_real_,
      tar_percent = if (n_observed) 100 * sum(observed_glucose > thresholds$tir_upper) / n_observed else NA_real_,
      tbr_level2_percent = if (n_observed) 100 * sum(observed_glucose < thresholds$tbr_level2) / n_observed else NA_real_,
      tbr_level1_percent = if (n_observed) 100 * sum(observed_glucose >= thresholds$tbr_level2 & observed_glucose < thresholds$tir_lower) / n_observed else NA_real_,
      tar_level1_percent = if (n_observed) 100 * sum(observed_glucose > thresholds$tir_upper & observed_glucose <= thresholds$tar_level2) / n_observed else NA_real_,
      tar_level2_percent = if (n_observed) 100 * sum(observed_glucose > thresholds$tar_level2) / n_observed else NA_real_,
      gmi_percent = if (!is.na(mean_glucose)) 3.31 + 0.02392 * mean_glucose else NA_real_
    )
  }, by = by]

  as.data.frame(result, stringsAsFactors = FALSE, check.names = FALSE)
}

compute_base_core_metrics <- function(
  data,
  thresholds = default_cgm_thresholds(),
  by = default_metric_groups(data),
  periods = time_window_values()
) {
  if (!all(c("id", "timestamp", "glucose") %in% names(data))) {
    stop("Metrics require standardized columns: id, timestamp, glucose.", call. = FALSE)
  }

  by <- by[by %in% names(data)]
  if (!length(by)) {
    by <- "id"
  }

  periods <- unique(vapply(periods %||% default_time_window(), normalize_time_window, character(1)))
  period_rows <- lapply(periods, function(period) {
    period_data <- filter_time_window_data(data, period)
    if (!nrow(period_data)) {
      return(NULL)
    }
    out <- compute_base_metrics_dt(period_data, thresholds = thresholds, by = by)
    if (!nrow(out)) {
      return(NULL)
    }
    out$metric_period <- period
    out
  })
  period_rows <- Filter(Negate(is.null), period_rows)
  if (!length(period_rows)) {
    out <- data.frame()
  } else {
    out <- do.call(rbind, period_rows)
  }
  row.names(out) <- NULL
  out$metric_engine <- "base_fallback"
  out
}

metric_state <- function(status, data = NULL, base = data.frame(), display = empty_metrics_display(), error = NULL, message = NULL) {
  messages <- list(
    needs_mapping = "Select timestamp and glucose columns to calculate metrics.",
    no_analysis_rows = "No CGM rows are available for the selected analysis date range.",
    base_ready = "",
    base_error = "Metrics could not be calculated from the current analysis data. Check timestamp and glucose mappings."
  )
  list(
    status = status,
    data = data,
    base = base,
    display = display,
    error = error,
    message = message %||% messages[[status]] %||% ""
  )
}

compute_base_metric_state <- function(data, thresholds = default_cgm_thresholds()) {
  if (is.null(data) || !is.data.frame(data)) {
    return(metric_state("needs_mapping"))
  }

  missing_columns <- setdiff(c("id", "timestamp", "glucose"), names(data))
  if (length(missing_columns)) {
    return(metric_state(
      "needs_mapping",
      data = data,
      error = paste("Missing standardized column(s):", paste(missing_columns, collapse = ", "))
    ))
  }

  if (!nrow(data)) {
    return(metric_state("no_analysis_rows", data = data))
  }
  if (!valid_metric_thresholds(thresholds)) {
    return(metric_state("base_error", data = data, error = "Invalid metric threshold settings."))
  }

  tryCatch({
    base <- compute_base_core_metrics(data, thresholds = thresholds)
    display <- prepare_metrics_display(base, thresholds = thresholds)
    if (!nrow(base) || !nrow(display)) {
      return(metric_state("no_analysis_rows", data = data, base = base, display = display))
    }
    metric_state("base_ready", data = data, base = base, display = display)
  }, error = function(error) {
    metric_state("base_error", data = data, error = conditionMessage(error))
  })
}

should_start_additional_metrics <- function(state) {
  identical(state$status, "base_ready") && is.data.frame(state$base) && nrow(state$base) > 0L
}

valid_metric_thresholds <- function(thresholds) {
  required <- names(default_cgm_thresholds())
  if (!all(required %in% names(thresholds))) {
    return(FALSE)
  }
  all(vapply(required, function(name) {
    value <- thresholds[[name]]
    is.numeric(value) && length(value) == 1L && is.finite(value)
  }, logical(1)))
}

compute_metric_adapters <- function(
  data,
  by = default_metric_groups(data),
  periods = time_window_values()
) {
  by <- by[by %in% names(data)]
  if (!length(by)) {
    by <- "id"
  }

  periods <- unique(vapply(periods %||% default_time_window(), normalize_time_window, character(1)))
  period_rows <- lapply(periods, function(period) {
    period_data <- filter_time_window_data(data, period)
    if (!nrow(period_data)) {
      return(NULL)
    }
    out <- compute_cgmanalyzer_metrics(period_data, by = by)
    out <- merge_metric_adapter(out, compute_iglu_metrics(period_data, by = by), by = by)
    if (!nrow(out)) {
      return(NULL)
    }
    out$metric_period <- period
    out
  })
  period_rows <- Filter(Negate(is.null), period_rows)
  if (!length(period_rows)) {
    return(data.frame())
  }
  out <- do.call(rbind, period_rows)
  row.names(out) <- NULL
  out
}

merge_core_metric_outputs <- function(base, adapters, by = default_metric_groups(base)) {
  by <- by[by %in% names(base)]
  if (!length(by)) {
    by <- "id"
  }
  out <- merge_metric_adapter(base, adapters, by = by)
  out$metric_engine <- paste(
    c(
      "base_fallback",
      if (requireNamespace("CGManalyzer", quietly = TRUE)) "CGManalyzer",
      if (requireNamespace("iglu", quietly = TRUE)) "iglu"
    ),
    collapse = "+"
  )
  out
}

#' Compute core CGM metrics
#'
#' `CGManalyzer` is the preferred CGM analysis engine for future metric
#' expansion. This foundational helper computes transparent core summaries in
#' base R so the Shiny app has a stable fallback while package-specific metric
#' adapters are added and tested.
#'
#' @param data Standardized CGM data.
#' @param thresholds Named threshold list in mg/dL.
#' @param by Grouping columns.
#'
#' @return Data frame of metric summaries.
#' @noRd
compute_core_metrics <- function(
  data,
  thresholds = default_cgm_thresholds(),
  by = default_metric_groups(data),
  periods = time_window_values()
) {
  by <- by[by %in% names(data)]
  if (!length(by)) {
    by <- "id"
  }

  merge_by <- unique(c(by, "metric_period"))

  merge_core_metric_outputs(
    compute_base_core_metrics(data, thresholds = thresholds, by = by, periods = periods),
    compute_metric_adapters(data, by = by, periods = periods),
    by = merge_by
  )
}
