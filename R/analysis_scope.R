available_analysis_date_range <- function(data) {
  if (is.null(data) || !is.data.frame(data) || !"timestamp" %in% names(data)) {
    return(c(start = NA_character_, end = NA_character_))
  }

  dates <- as.Date(data$timestamp[is_finite_cgm_timestamp(data$timestamp)])
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    return(c(start = NA_character_, end = NA_character_))
  }

  c(start = as.character(min(dates)), end = as.character(max(dates)))
}

normalize_analysis_date_range <- function(date_range, data = NULL) {
  available <- available_analysis_date_range(data)
  start <- as.Date(date_range[[1L]] %||% available[["start"]])
  end <- as.Date(date_range[[2L]] %||% available[["end"]])

  if (is.na(start)) {
    start <- as.Date(available[["start"]])
  }
  if (is.na(end)) {
    end <- as.Date(available[["end"]])
  }
  if (!is.na(start) && !is.na(end) && start > end) {
    tmp <- start
    start <- end
    end <- tmp
  }

  c(start = as.character(start), end = as.character(end))
}

#' Filter standardized CGM data to the global analysis date range
#'
#' @param data Standardized CGM data with a POSIXct `timestamp` column.
#' @param date_range Length-two vector or list with start and end dates.
#'
#' @return Filtered standardized CGM data.
filter_analysis_date_range <- function(data, date_range = NULL) {
  if (is.null(date_range) || !nrow(data) || !"timestamp" %in% names(data)) {
    return(data)
  }

  range <- normalize_analysis_date_range(date_range, data)
  start <- as.Date(range[["start"]])
  end <- as.Date(range[["end"]])
  if (is.na(start) && is.na(end)) {
    return(data)
  }

  dates <- as.Date(data$timestamp)
  keep <- !is.na(dates)
  if (!is.na(start)) {
    keep <- keep & dates >= start
  }
  if (!is.na(end)) {
    keep <- keep & dates <= end
  }
  data[keep, , drop = FALSE]
}

analysis_date_range_signature <- function(settings) {
  range <- settings$analysis_date_range %||% c(start = NA_character_, end = NA_character_)
  c(start = as.character(range[[1L]]), end = as.character(range[[2L]]))
}
