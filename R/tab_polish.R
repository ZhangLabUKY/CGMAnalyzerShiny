format_count <- function(value) {
  format(value %||% 0L, big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_date_span <- function(timestamp) {
  if (
    is.null(timestamp) ||
      !length(timestamp) ||
      !any(is_finite_cgm_timestamp(timestamp))
  ) {
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

glucose_parse_summary <- function(
  x,
  units = "mg/dL",
  glucose_min = 40,
  glucose_max = 400
) {
  raw_chr <- trimws(as.character(x))
  non_missing <- !is.na(raw_chr) & nzchar(raw_chr)
  glucose_raw <- suppressWarnings(coerce_glucose(x))
  numeric_rows <- non_missing & !is.na(glucose_raw)
  converted <- convert_glucose_to_mg_dl(glucose_raw, units)
  observed <- converted[numeric_rows]
  raw_observed <- glucose_raw[numeric_rows]
  suspicious_units <- FALSE
  if (length(raw_observed)) {
    units_norm <- tolower(trimws(units %||% "mg/dL"))
    suspicious_units <- if (
      units_norm %in% c("mmol/l", "mmol", "mmol/liter", "mmoll")
    ) {
      stats::median(raw_observed, na.rm = TRUE) > 40
    } else {
      stats::median(raw_observed, na.rm = TRUE) < 25
    }
  }

  data.frame(
    rows = length(x),
    non_missing_glucose = sum(non_missing),
    numeric_glucose = sum(numeric_rows),
    missing_glucose = sum(!non_missing),
    non_numeric_glucose = sum(non_missing & is.na(glucose_raw)),
    min_glucose = if (length(observed)) {
      min(observed, na.rm = TRUE)
    } else {
      NA_real_
    },
    max_glucose = if (length(observed)) {
      max(observed, na.rm = TRUE)
    } else {
      NA_real_
    },
    implausible_glucose = sum(
      !is.na(observed) & (observed < glucose_min | observed > glucose_max)
    ),
    suspicious_units = suspicious_units,
    units = units %||% "mg/dL",
    stringsAsFactors = FALSE
  )
}

timestamp_validation_summary <- function(
  upload = NULL,
  mapping = NULL,
  tz = "UTC"
) {
  data <- if (is.list(upload)) upload$data else NULL
  timestamp_col <- if (is.list(mapping)) {
    clean_mapping_value(mapping$timestamp)
  } else {
    NA_character_
  }
  if (
    !is.data.frame(data) ||
      !nrow(data) ||
      is.na(timestamp_col) ||
      !timestamp_col %in% names(data)
  ) {
    return(NULL)
  }
  summary <- timestamp_parse_summary(data[[timestamp_col]], tz = tz)
  summary$timestamp_column <- timestamp_col
  failed <- !is.na(trimws(as.character(data[[timestamp_col]]))) &
    nzchar(trimws(as.character(data[[timestamp_col]]))) &
    is.na(parse_cgm_timestamp(data[[timestamp_col]], tz = tz))
  examples <- unique(as.character(data[[timestamp_col]][failed]))
  summary$example_failed_values <- paste(
    examples[seq_len(min(length(examples), 3L))],
    collapse = ", "
  )
  summary
}

glucose_validation_summary <- function(
  upload = NULL,
  mapping = NULL,
  glucose_min = 40,
  glucose_max = 400
) {
  data <- if (is.list(upload)) upload$data else NULL
  glucose_col <- if (is.list(mapping)) {
    clean_mapping_value(mapping$glucose)
  } else {
    NA_character_
  }
  units <- if (is.list(mapping)) mapping$source_units %||% "mg/dL" else "mg/dL"
  if (
    !is.data.frame(data) ||
      !nrow(data) ||
      is.na(glucose_col) ||
      !glucose_col %in% names(data)
  ) {
    return(NULL)
  }
  summary <- glucose_parse_summary(
    data[[glucose_col]],
    units = units,
    glucose_min = glucose_min,
    glucose_max = glucose_max
  )
  summary$glucose_column <- glucose_col
  summary
}

validation_status <- function(ok, warning = FALSE) {
  if (isTRUE(ok) && !isTRUE(warning)) {
    "OK"
  } else {
    "Review"
  }
}

data_validation_rows <- function(
  upload = NULL,
  mapping = NULL,
  standardization_error = NULL,
  settings = NULL
) {
  timestamp <- timestamp_validation_summary(upload, mapping)
  glucose <- glucose_validation_summary(upload, mapping)

  timestamp_ready <- !is.null(timestamp) && timestamp$failed_timestamps == 0L
  glucose_ready <- !is.null(glucose) && glucose$non_numeric_glucose == 0L
  date_ready <- is.list(settings) &&
    !is.null(settings$analysis_date_range) &&
    !is.na(settings$analysis_date_range[["start"]]) &&
    !is.na(settings$analysis_date_range[["end"]])

  rows <- rbind(
    setup_status_badge(
      "Files loaded",
      has_uploaded_data(upload),
      if (has_uploaded_data(upload)) {
        "Ready for mapping"
      } else {
        "Upload CGM files or load example data"
      }
    ),
    setup_status_badge(
      "Timestamp column",
      !is.null(timestamp),
      if (is.null(timestamp)) {
        "Select a timestamp column"
      } else {
        paste("Using", timestamp$timestamp_column)
      }
    ),
    setup_status_badge(
      "Glucose column",
      !is.null(glucose),
      if (is.null(glucose)) {
        "Select a glucose column"
      } else {
        paste("Using", glucose$glucose_column)
      }
    ),
    setup_status_badge(
      "Timestamp parsing",
      timestamp_ready && is.null(standardization_error),
      if (!is.null(standardization_error)) {
        standardization_error
      } else if (is.null(timestamp)) {
        "Waiting for timestamp mapping"
      } else if (timestamp$failed_timestamps > 0L) {
        paste(
          "Review",
          format_count(timestamp$failed_timestamps),
          "unparsed timestamp value(s)"
        )
      } else {
        "All non-missing timestamp values parsed"
      }
    ),
    setup_status_badge(
      "Glucose parsing",
      glucose_ready,
      if (is.null(glucose)) {
        "Waiting for glucose mapping"
      } else if (glucose$non_numeric_glucose > 0L) {
        paste(
          "Review",
          format_count(glucose$non_numeric_glucose),
          "non-numeric glucose value(s)"
        )
      } else {
        "All non-missing glucose values are numeric"
      }
    ),
    setup_status_badge(
      "Analysis date range",
      date_ready,
      if (date_ready) {
        paste(
          format(as.Date(settings$analysis_date_range[["start"]]), "%Y-%m-%d"),
          "to",
          format(as.Date(settings$analysis_date_range[["end"]]), "%Y-%m-%d")
        )
      } else {
        "Available after timestamps parse"
      }
    )
  )
  row.names(rows) <- NULL
  rows
}

timestamp_summary_display <- function(summary) {
  if (is.null(summary)) {
    return(data.frame(
      Label = character(),
      Value = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Label = c(
      "Rows",
      "Parsed timestamps",
      "Invalid timestamps",
      "Ambiguous date values",
      "Date span"
    ),
    Value = c(
      format_count(summary$rows),
      format_count(summary$parsed_timestamps),
      format_count(summary$failed_timestamps),
      format_count(summary$ambiguous_timestamps),
      if (is.na(summary$first_timestamp)) {
        "Not available"
      } else {
        paste(
          format(summary$first_timestamp, "%Y-%m-%d"),
          "to",
          format(summary$last_timestamp, "%Y-%m-%d")
        )
      }
    ),
    stringsAsFactors = FALSE
  )
}

glucose_summary_display <- function(summary) {
  if (is.null(summary)) {
    return(data.frame(
      Label = character(),
      Value = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Label = c(
      "Rows",
      "Numeric glucose",
      "Missing glucose",
      "Non-numeric glucose",
      "Minimum",
      "Maximum"
    ),
    Value = c(
      format_count(summary$rows),
      format_count(summary$numeric_glucose),
      format_count(summary$missing_glucose),
      format_count(summary$non_numeric_glucose),
      ifelse(
        is.na(summary$min_glucose),
        "Not available",
        round(summary$min_glucose, 1)
      ),
      ifelse(
        is.na(summary$max_glucose),
        "Not available",
        round(summary$max_glucose, 1)
      )
    ),
    stringsAsFactors = FALSE
  )
}

data_validation_warnings <- function(
  timestamp_summary = NULL,
  glucose_summary = NULL
) {
  warnings <- character()
  if (!is.null(timestamp_summary) && timestamp_summary$failed_timestamps > 0L) {
    warnings <- c(
      warnings,
      paste0(
        "Timestamp parsing needs review. Example value(s): ",
        timestamp_summary$example_failed_values
      )
    )
  }
  if (
    !is.null(timestamp_summary) && timestamp_summary$ambiguous_timestamps > 0L
  ) {
    warnings <- c(
      warnings,
      paste(
        "Ambiguous day/month timestamps were parsed using the app's day-first rule."
      )
    )
  }
  if (!is.null(glucose_summary) && glucose_summary$non_numeric_glucose > 0L) {
    warnings <- c(
      warnings,
      paste(
        format_count(glucose_summary$non_numeric_glucose),
        "non-numeric glucose value(s) will be treated as missing."
      )
    )
  }
  if (!is.null(glucose_summary) && glucose_summary$implausible_glucose > 0L) {
    warnings <- c(
      warnings,
      paste(
        format_count(glucose_summary$implausible_glucose),
        "glucose value(s) are outside the initial 40-400 mg/dL review range."
      )
    )
  }
  if (!is.null(glucose_summary) && isTRUE(glucose_summary$suspicious_units)) {
    warnings <- c(
      warnings,
      paste(
        "Selected source units may not match the glucose scale. Review the Source units setting."
      )
    )
  }
  warnings
}

data_setup_status <- function(
  upload = NULL,
  mapping = NULL,
  standardized_data = NULL,
  standardization_error = NULL,
  settings = NULL
) {
  has_upload <- has_uploaded_data(upload)
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
        paste(
          format_count(length(upload$files)),
          "file(s),",
          format_count(nrow(upload$data)),
          "row(s)"
        )
      } else {
        "Upload CGM files or load example data"
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
      if (!is.null(standardization_error)) {
        standardization_error
      } else if (parsed_timestamps) {
        format_date_span(standardized_data$timestamp)
      } else {
        "Waiting for valid mapped timestamps"
      }
    ),
    setup_status_badge(
      "Analysis date range",
      has_date_range,
      if (has_date_range) {
        paste(
          format(as.Date(settings$analysis_date_range[["start"]]), "%Y-%m-%d"),
          "to",
          format(as.Date(settings$analysis_date_range[["end"]]), "%Y-%m-%d")
        )
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
    return(data.frame(
      Label = character(),
      Value = character(),
      stringsAsFactors = FALSE
    ))
  }

  subject_count <- if (
    is.data.frame(standardized_data) && "id" %in% names(standardized_data)
  ) {
    length(subject_id_values(standardized_data))
  } else {
    NA_integer_
  }
  date_span <- if (
    is.data.frame(standardized_data) &&
      "timestamp" %in% names(standardized_data)
  ) {
    format_date_span(standardized_data$timestamp)
  } else {
    "Map timestamp to calculate"
  }
  missing_glucose <- if (
    is.data.frame(standardized_data) && "glucose" %in% names(standardized_data)
  ) {
    sum(is.na(standardized_data$glucose))
  } else {
    NA_integer_
  }

  data.frame(
    Label = c("Rows", "Files", "Subject IDs", "Date span", "Missing glucose"),
    Value = c(
      format_count(nrow(upload_data)),
      format_count(length(upload$files)),
      ifelse(
        is.na(subject_count),
        "Map columns to calculate",
        format_count(subject_count)
      ),
      date_span,
      ifelse(
        is.na(missing_glucose),
        "Map glucose to calculate",
        format_count(missing_glucose)
      )
    ),
    stringsAsFactors = FALSE
  )
}

imputation_missingness_summary <- function(data) {
  if (!is.data.frame(data) || !nrow(data) || !"glucose" %in% names(data)) {
    return(data.frame(
      rows = 0L,
      missing_glucose = 0L,
      missing_percent = NA_real_,
      estimated_missing_readings = NA_integer_,
      subjects_affected = 0L,
      severity = "Not available",
      severity_class = "secondary",
      message = "Map timestamp and glucose columns to review imputation needs.",
      stringsAsFactors = FALSE
    ))
  }

  missing <- is.na(data$glucose)
  missing_rows <- sum(missing)
  missing_percent <- round(100 * missing_rows / nrow(data), 2)
  affected_subjects <- if ("id" %in% names(data)) {
    length(subject_id_values(data[missing, , drop = FALSE]))
  } else {
    0L
  }
  estimated_missing <- NA_integer_
  if (all(c("id", "timestamp") %in% names(data)) && any(is_finite_cgm_timestamp(data$timestamp))) {
    gaps <- tryCatch(detect_gap_periods(data), error = function(e) NULL)
    if (is.data.frame(gaps) && "estimated_missing_readings" %in% names(gaps)) {
      estimated_missing <- sum(gaps$estimated_missing_readings, na.rm = TRUE)
    }
  }

  if (missing_rows == 0L) {
    severity <- "No missing glucose"
    severity_class <- "success"
    message <- "No missing glucose rows were detected in the current analysis date range. Estimated missing readings from timestamp gaps are shown separately."
  } else if (missing_percent <= 5) {
    severity <- "Acceptable missingness"
    severity_class <- "success"
    message <- "Explicit missing glucose rows are within the acceptable range. Estimated missing readings from timestamp gaps are shown separately."
  } else if (missing_percent <= 25) {
    severity <- "Moderate missingness"
    severity_class <- "warning"
    message <- "Explicit missing glucose rows may affect summaries. Review timestamp gaps separately before deciding whether to impute."
  } else {
    severity <- "Severe missingness"
    severity_class <- "danger"
    message <- "Explicit missing glucose rows are severe. Interpret imputed results cautiously and review timestamp gaps before final analysis."
  }

  data.frame(
    rows = nrow(data),
    missing_glucose = missing_rows,
    missing_percent = missing_percent,
    estimated_missing_readings = estimated_missing,
    subjects_affected = affected_subjects,
    severity = severity,
    severity_class = severity_class,
    message = message,
    stringsAsFactors = FALSE
  )
}

imputation_summary_cards <- function(summary) {
  if (!is.data.frame(summary) || !nrow(summary)) {
    return(data.frame(Label = character(), Value = character(), stringsAsFactors = FALSE))
  }
  percent <- summary$missing_percent[[1L]]
  data.frame(
    Label = c("Missing glucose rows", "Missing glucose (%)", "Estimated gap readings", "Subject IDs affected", "Review level"),
    Value = c(
      format_count(summary$missing_glucose[[1L]]),
      if (is.na(percent)) "Not available" else paste0(format(round(percent, 2), trim = TRUE), "%"),
      if ("estimated_missing_readings" %in% names(summary) && !is.na(summary$estimated_missing_readings[[1L]])) {
        format_count(summary$estimated_missing_readings[[1L]])
      } else {
        "Not available"
      },
      format_count(summary$subjects_affected[[1L]]),
      summary$severity[[1L]]
    ),
    stringsAsFactors = FALSE
  )
}

imputation_summary_box_ui <- function(summary) {
  if (!is.data.frame(summary) || !nrow(summary)) {
    return(NULL)
  }
  severity_class <- summary$severity_class[[1L]]
  shiny::div(
    class = paste("alert", paste0("alert-", severity_class), "mb-3"),
    shiny::div(
      style = "display:flex; justify-content:space-between; gap:12px; align-items:flex-start;",
      shiny::div(
        shiny::strong(summary$severity[[1L]]),
        shiny::div(summary$message[[1L]])
      ),
      shiny::span(
        class = paste("badge", paste0("bg-", if (identical(severity_class, "warning")) "warning text-dark" else severity_class)),
        if (is.na(summary$missing_percent[[1L]])) {
          "NA"
        } else {
          paste0(format(round(summary$missing_percent[[1L]], 2), trim = TRUE), "% missing")
        }
      )
    ),
    summary_card_ui(imputation_summary_cards(summary), compact = TRUE)
  )
}

quality_summary_cards <- function(data, qc_summary, missingness_comparison) {
  if (!is.data.frame(data) || !nrow(data)) {
    return(data.frame(
      Label = character(),
      Value = character(),
      stringsAsFactors = FALSE
    ))
  }
  valid_days <- if (
    is.data.frame(qc_summary) && "valid_days" %in% names(qc_summary)
  ) {
    sum(qc_summary$valid_days, na.rm = TRUE)
  } else {
    NA_integer_
  }
  timestamp_gaps <- if (
    is.data.frame(missingness_comparison) &&
      "Timestamp gaps" %in% names(missingness_comparison)
  ) {
    sum(missingness_comparison[["Timestamp gaps"]], na.rm = TRUE)
  } else {
    NA_integer_
  }
  missing_rows <- if (
    is.data.frame(missingness_comparison) &&
      "Missing glucose rows after preprocessing" %in% names(missingness_comparison)
  ) {
    sum(
      missingness_comparison[["Missing glucose rows after preprocessing"]],
      na.rm = TRUE
    )
  } else if (
    is.data.frame(missingness_comparison) &&
      "Missing glucose rows" %in% names(missingness_comparison)
  ) {
    sum(
      missingness_comparison[["Missing glucose rows"]],
      na.rm = TRUE
    )
  } else {
    sum(is.na(data$glucose), na.rm = TRUE)
  }
  filled_rows <- if (
    is.data.frame(missingness_comparison) &&
      "Filled glucose rows" %in% names(missingness_comparison)
  ) {
    sum(missingness_comparison[["Filled glucose rows"]], na.rm = TRUE)
  } else {
    sum(data$imputed_flag %in% TRUE, na.rm = TRUE)
  }

  data.frame(
    Label = c(
      "Subject IDs",
      "Date span",
      "Valid days",
      "Timestamp gaps",
      "Missing glucose",
      "Filled rows"
    ),
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

plot_selection_summary <- function(
  data,
  plot_type = "trace",
  participant = "",
  group = "",
  visit = "",
  day = ""
) {
  if (!is.data.frame(data) || !nrow(data)) {
    return(data.frame(
      Label = character(),
      Value = character(),
      stringsAsFactors = FALSE
    ))
  }
  filtered <- plot_filtered_data(
    data,
    plot_type = plot_type,
    participant = participant,
    group = group,
    visit = visit,
    day = day
  )

  summary <- data.frame(
    Label = c("Rows plotted", "Subject IDs", "Days", "Date span"),
    Value = c(
      format_count(nrow(filtered)),
      format_count(length(subject_id_values(filtered))),
      format_count(length(unique(as.Date(filtered$timestamp)))),
      format_date_span(filtered$timestamp)
    ),
    stringsAsFactors = FALSE
  )
  if (identical(plot_type, "daily_overlay")) {
    summary <- rbind(summary, daily_overlay_summary_rows(filtered, day = day))
  }
  summary
}

summary_card_ui <- function(summary, compact = FALSE) {
  if (!is.data.frame(summary) || !nrow(summary)) {
    return(NULL)
  }
  shiny::div(
    class = "cgm-summary-cards d-flex flex-wrap gap-2 mb-3",
    lapply(seq_len(nrow(summary)), function(i) {
      shiny::div(
        class = "card cgm-summary-card",
        style = paste0(
          "min-width:",
          if (compact) "132px" else "150px",
          "; padding: 10px 12px;"
        ),
        shiny::div(
          style = "font-size: 0.8rem; color: #555;",
          summary$Label[[i]]
        ),
        shiny::div(
          style = "font-size: 1.1rem; font-weight: 600;",
          summary$Value[[i]]
        )
      )
    })
  )
}

validation_checklist_ui <- function(rows) {
  if (!is.data.frame(rows) || !nrow(rows)) {
    return(NULL)
  }
  shiny::div(
    class = "list-group mb-3",
    lapply(seq_len(nrow(rows)), function(i) {
      ready <- identical(rows$Status[[i]], "Ready")
      shiny::div(
        class = "list-group-item d-flex justify-content-between align-items-start",
        shiny::div(
          shiny::div(style = "font-weight: 600;", rows$Step[[i]]),
          shiny::tags$small(class = "text-muted", rows$Detail[[i]])
        ),
        shiny::span(
          class = if (ready) {
            "badge bg-success rounded-pill"
          } else {
            "badge bg-warning text-dark rounded-pill"
          },
          if (ready) "OK" else "Review"
        )
      )
    })
  )
}

data_validation_panel_ui <- function(
  upload = NULL,
  mapping = NULL,
  standardization_error = NULL,
  settings = NULL
) {
  timestamp <- timestamp_validation_summary(upload, mapping)
  glucose <- glucose_validation_summary(upload, mapping)
  warnings <- data_validation_warnings(timestamp, glucose)
  shiny::tagList(
    shiny::h4("Data validation"),
    validation_checklist_ui(data_validation_rows(
      upload,
      mapping,
      standardization_error,
      settings
    )),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::h5("Timestamp summary"),
        summary_card_ui(timestamp_summary_display(timestamp), compact = TRUE)
      ),
      shiny::column(
        6,
        shiny::h5("Glucose summary"),
        summary_card_ui(glucose_summary_display(glucose), compact = TRUE)
      )
    ),
    if (length(warnings)) {
      shiny::div(
        class = "alert alert-warning",
        shiny::strong("Review recommended"),
        shiny::tags$ul(lapply(warnings, shiny::tags$li))
      )
    } else {
      shiny::div(
        class = "alert alert-success",
        "Mapped timestamp and glucose columns look ready for analysis."
      )
    }
  )
}
