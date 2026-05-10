format_count <- function(value) {
  format(value %||% 0L, big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_date_span <- function(timestamp) {
  if (is.null(timestamp) || !length(timestamp) || !any(is_finite_cgm_timestamp(timestamp))) {
    return("Not available")
  }
  dates <- as.Date(timestamp[is_finite_cgm_timestamp(timestamp)])
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    return("Not available")
  }
  paste(format(min(dates), "%Y-%m-%d"), "to", format(max(dates), "%Y-%m-%d"))
}

has_uploaded_data <- function(upload) {
  is.list(upload) &&
    is.data.frame(upload$data) &&
    nrow(upload$data) > 0L
}

setup_status_badge <- function(label, ready, detail = "") {
  data.frame(
    Step = label,
    Status = if (isTRUE(ready)) "Ready" else "Needs attention",
    Detail = detail,
    stringsAsFactors = FALSE
  )
}

data_setup_status <- function(upload = NULL, mapping = NULL, standardized_data = NULL, standardization_error = NULL, settings = NULL) {
  has_upload <- is.list(upload) && is.data.frame(upload$data) && nrow(upload$data) > 0L
  has_timestamp <- is.list(mapping) && nzchar(mapping$timestamp %||% "")
  has_glucose <- is.list(mapping) && nzchar(mapping$glucose %||% "")
  parsed_timestamps <- is.data.frame(standardized_data) &&
    "timestamp" %in% names(standardized_data) &&
    any(is_finite_cgm_timestamp(standardized_data$timestamp))
  has_date_range <- is.list(settings) &&
    !is.null(settings$analysis_date_range) &&
    !is.na(settings$analysis_date_range[["start"]]) &&
    !is.na(settings$analysis_date_range[["end"]])

  rows <- rbind(
    setup_status_badge(
      "Files loaded",
      has_upload,
      if (has_upload) {
        paste(format_count(length(upload$files)), "file(s),", format_count(nrow(upload$data)), "row(s)")
      } else {
        "Upload CGM files or load demo data"
      }
    ),
    setup_status_badge(
      "Required mappings",
      has_timestamp && has_glucose,
      paste(
        if (has_timestamp) "Timestamp selected" else "Select timestamp",
        "|",
        if (has_glucose) "Glucose selected" else "Select glucose"
      )
    ),
    setup_status_badge(
      "Timestamps parsed",
      parsed_timestamps && is.null(standardization_error),
      if (!is.null(standardization_error)) standardization_error else if (parsed_timestamps) format_date_span(standardized_data$timestamp) else "Waiting for valid mapped timestamps"
    ),
    setup_status_badge(
      "Analysis date range",
      has_date_range,
      if (has_date_range) {
        paste(format(as.Date(settings$analysis_date_range[["start"]]), "%Y-%m-%d"), "to", format(as.Date(settings$analysis_date_range[["end"]]), "%Y-%m-%d"))
      } else {
        "Available after timestamps parse"
      }
    )
  )
  row.names(rows) <- NULL
  rows
}

data_upload_summary <- function(upload = NULL, standardized_data = NULL) {
  upload_data <- if (is.list(upload)) upload$data else NULL
  if (!is.data.frame(upload_data) || !nrow(upload_data)) {
    return(data.frame(Label = character(), Value = character(), stringsAsFactors = FALSE))
  }

  subject_count <- if (is.data.frame(standardized_data) && "id" %in% names(standardized_data)) {
    length(subject_id_values(standardized_data))
  } else {
    NA_integer_
  }
  date_span <- if (is.data.frame(standardized_data) && "timestamp" %in% names(standardized_data)) {
    format_date_span(standardized_data$timestamp)
  } else {
    "Map timestamp to calculate"
  }
  missing_glucose <- if (is.data.frame(standardized_data) && "glucose" %in% names(standardized_data)) {
    sum(is.na(standardized_data$glucose))
  } else {
    NA_integer_
  }

  data.frame(
    Label = c("Rows", "Files", "Subject IDs", "Date span", "Missing glucose"),
    Value = c(
      format_count(nrow(upload_data)),
      format_count(length(upload$files)),
      ifelse(is.na(subject_count), "Map columns to calculate", format_count(subject_count)),
      date_span,
      ifelse(is.na(missing_glucose), "Map glucose to calculate", format_count(missing_glucose))
    ),
    stringsAsFactors = FALSE
  )
}

quality_summary_cards <- function(data, qc_summary, missingness_comparison) {
  if (!is.data.frame(data) || !nrow(data)) {
    return(data.frame(Label = character(), Value = character(), stringsAsFactors = FALSE))
  }
  valid_days <- if (is.data.frame(qc_summary) && "valid_days" %in% names(qc_summary)) sum(qc_summary$valid_days, na.rm = TRUE) else NA_integer_
  timestamp_gaps <- if (is.data.frame(missingness_comparison) && "Timestamp gaps" %in% names(missingness_comparison)) {
    sum(missingness_comparison[["Timestamp gaps"]], na.rm = TRUE)
  } else {
    NA_integer_
  }
  missing_rows <- if (is.data.frame(missingness_comparison) && "Missing glucose after preprocessing" %in% names(missingness_comparison)) {
    sum(missingness_comparison[["Missing glucose after preprocessing"]], na.rm = TRUE)
  } else {
    sum(is.na(data$glucose), na.rm = TRUE)
  }
  filled_rows <- if (is.data.frame(missingness_comparison) && "Filled glucose rows" %in% names(missingness_comparison)) {
    sum(missingness_comparison[["Filled glucose rows"]], na.rm = TRUE)
  } else {
    sum(data$imputed_flag %in% TRUE, na.rm = TRUE)
  }

  data.frame(
    Label = c("Subject IDs", "Date span", "Valid days", "Timestamp gaps", "Missing glucose", "Filled rows"),
    Value = c(
      format_count(length(subject_id_values(data))),
      format_date_span(data$timestamp),
      format_count(valid_days),
      format_count(timestamp_gaps),
      format_count(missing_rows),
      format_count(filled_rows)
    ),
    stringsAsFactors = FALSE
  )
}

plot_selection_summary <- function(data, plot_type = "trace", participant = "", group = "", visit = "", day = "") {
  if (!is.data.frame(data) || !nrow(data)) {
    return(data.frame(Label = character(), Value = character(), stringsAsFactors = FALSE))
  }
  day_filter <- if (identical(plot_type, "daily_overlay")) normalize_plot_days(day) else all_filter_value()
  filtered <- filter_plot_data(data, participant = participant, group = group, visit = visit, day = day_filter)
  filtered <- filtered[is_finite_cgm_timestamp(filtered$timestamp), , drop = FALSE]

  data.frame(
    Label = c("Rows plotted", "Subject IDs", "Days", "Date span"),
    Value = c(
      format_count(nrow(filtered)),
      format_count(length(subject_id_values(filtered))),
      format_count(length(unique(as.Date(filtered$timestamp)))),
      format_date_span(filtered$timestamp)
    ),
    stringsAsFactors = FALSE
  )
}

summary_card_ui <- function(summary, compact = FALSE) {
  if (!is.data.frame(summary) || !nrow(summary)) {
    return(NULL)
  }
  shiny::div(
    class = "d-flex flex-wrap gap-2 mb-3",
    lapply(seq_len(nrow(summary)), function(i) {
      shiny::div(
        class = "card",
        style = paste0(
          "min-width:", if (compact) "132px" else "150px",
          "; padding: 10px 12px;"
        ),
        shiny::div(style = "font-size: 0.8rem; color: #555;", summary$Label[[i]]),
        shiny::div(style = "font-size: 1.1rem; font-weight: 600;", summary$Value[[i]])
      )
    })
  )
}
