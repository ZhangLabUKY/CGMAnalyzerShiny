time_window_values <- function() {
  c("full_day", "daytime", "nighttime")
}

default_time_window <- function() {
  "full_day"
}

time_window_labels <- function() {
  c(
    full_day = "All",
    daytime = "Daytime (06:00-23:59)",
    nighttime = "Nighttime (00:00-05:59)"
  )
}

normalize_time_window <- function(time_window = default_time_window()) {
  value <- time_window %||% default_time_window()
  value <- as.character(value[[1L]])
  value <- trimws(value)
  if (!nzchar(value)) {
    return(default_time_window())
  }

  labels <- time_window_labels()
  label_match <- names(labels)[match(value, labels)]
  if (!is.na(label_match)) {
    return(label_match)
  }

  value <- tolower(gsub("[^a-z0-9]+", "_", value))
  value <- gsub("^_|_$", "", value)
  aliases <- c(
    all = "full_day",
    full = "full_day",
    full_day = "full_day",
    day = "daytime",
    daytime = "daytime",
    night = "nighttime",
    nighttime = "nighttime"
  )
  out <- aliases[[value]] %||% value
  if (out %in% time_window_values()) out else default_time_window()
}

time_window_label <- function(time_window = default_time_window()) {
  labels <- time_window_labels()
  labels[[normalize_time_window(time_window)]] %||%
    labels[[default_time_window()]]
}

time_window_filter_choices <- function() {
  labels <- time_window_labels()
  stats::setNames(names(labels), unname(labels))
}

time_window_minutes <- function(timestamp) {
  timestamp <- as.POSIXlt(timestamp)
  timestamp$hour * 60 + timestamp$min + timestamp$sec / 60
}

filter_time_window_data <- function(data, time_window = default_time_window()) {
  time_window <- normalize_time_window(time_window)
  if (
    !is.data.frame(data) || !nrow(data) || identical(time_window, "full_day")
  ) {
    return(data)
  }
  if (!"timestamp" %in% names(data)) {
    return(data[0, , drop = FALSE])
  }

  minutes <- time_window_minutes(data$timestamp)
  keep <- !is.na(minutes) & is.finite(minutes)
  if (identical(time_window, "daytime")) {
    keep <- keep & minutes >= 6 * 60
  } else if (identical(time_window, "nighttime")) {
    keep <- keep & minutes < 6 * 60
  }
  data[keep, , drop = FALSE]
}

filter_metrics_by_period <- function(metrics, period = default_time_window()) {
  if (!is.data.frame(metrics) || !"metric_period" %in% names(metrics)) {
    return(metrics)
  }
  period <- normalize_time_window(period)
  metrics[as.character(metrics$metric_period) == period, , drop = FALSE]
}

time_window_filename_label <- function(time_window = default_time_window()) {
  switch(
    normalize_time_window(time_window),
    full_day = "time-full-day",
    daytime = "time-daytime",
    nighttime = "time-nighttime",
    "time-full-day"
  )
}
