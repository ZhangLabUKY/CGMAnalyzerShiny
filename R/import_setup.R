import_probe_row_limit <- function() {
  50L
}

import_preview_row_limit <- function() {
  8L
}

cgm_import_profile_patterns <- function() {
  list(
    dexcom = c("dexcom", "\\bg6\\b", "\\bg7\\b", "clarity"),
    abbott_libre = c("abbott", "libre", "freestyle"),
    medtronic = c("medtronic", "carelink", "guardian", "minimed")
  )
}

cgm_header_keyword_patterns <- function() {
  c(
    "timestamp",
    "time stamp",
    "\\btime\\b",
    "\\bdate\\b",
    "glucose",
    "sensor glucose",
    "glucose value",
    "historic.*glucose",
    "scan.*glucose",
    "sg value",
    "mg/dl",
    "mmol",
    "patient",
    "subject",
    "usubjid",
    "identifier",
    "event type",
    "device"
  )
}

cgm_time_header_patterns <- function() {
  c("timestamp", "time stamp", "\\btime\\b", "\\bdate\\b", "device timestamp", "display time")
}

cgm_glucose_header_patterns <- function() {
  c(
    "glucose",
    "glucose value",
    "sensor glucose",
    "historic.*glucose",
    "scan.*glucose",
    "interstitial.*glucose",
    "\\bsg\\b",
    "sg value",
    "mg/dl",
    "mmol"
  )
}

cgm_reading_indicator_patterns <- function() {
  c("\\begv\\b", "sensor", "historic", "scan", "reading", "glucose")
}

normalize_import_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x
}

cgm_fread_separator <- function(filename) {
  if (identical(tolower(tools::file_ext(filename)), "csv")) {
    return(",")
  }
  "auto"
}

column_indices_matching <- function(headers, patterns) {
  headers <- normalize_import_text(headers)
  which(vapply(headers, function(header) {
    any(vapply(patterns, function(pattern) grepl(pattern, header, perl = TRUE), logical(1)))
  }, logical(1)))
}

parseable_import_timestamp <- function(x) {
  parsed <- parse_cgm_timestamp(x, tz = "UTC", timestamp_parser = "compatibility")
  !is.na(parsed)
}

numeric_import_value <- function(x) {
  !is.na(suppressWarnings(coerce_glucose(x)))
}

score_cgm_reading_row <- function(values, headers = character(), profile = "Unknown CGM") {
  values <- trimws(as.character(values))
  values[is.na(values)] <- ""
  if (!any(nzchar(values))) {
    return(0)
  }

  time_idx <- column_indices_matching(headers, cgm_time_header_patterns())
  glucose_idx <- column_indices_matching(headers, cgm_glucose_header_patterns())
  reading_idx <- column_indices_matching(headers, c("event type", "type", "record type"))

  score <- 0
  if (length(time_idx)) {
    score <- score + 8 * any(parseable_import_timestamp(values[intersect(time_idx, seq_along(values))]))
  } else {
    score <- score + 4 * any(parseable_import_timestamp(values))
  }

  if (length(glucose_idx)) {
    score <- score + 8 * any(numeric_import_value(values[intersect(glucose_idx, seq_along(values))]))
  } else {
    score <- score + 3 * any(numeric_import_value(values))
  }

  normalized <- normalize_import_text(values)
  score <- score + 3 * any(vapply(cgm_reading_indicator_patterns(), function(pattern) {
    any(grepl(pattern, normalized, perl = TRUE))
  }, logical(1)))

  if (length(reading_idx)) {
    indicators <- normalize_import_text(values[intersect(reading_idx, seq_along(values))])
    score <- score + 3 * any(grepl("\\begv\\b|sensor|historic|scan|reading", indicators, perl = TRUE))
  }

  numeric_count <- sum(numeric_import_value(values[nzchar(values)]))
  score <- score + min(numeric_count, 3L)
  score
}

detect_cgm_first_reading_row <- function(probe, header_row = 1L, profile = "Unknown CGM") {
  if (!is.data.frame(probe) || !nrow(probe)) {
    return(list(row = max(2L, as.integer(header_row) + 1L), score = 0, status = "Review"))
  }
  header_row <- suppressWarnings(as.integer(header_row %||% 1L))
  if (is.na(header_row) || header_row < 1L) {
    header_row <- 1L
  }
  default_row <- min(nrow(probe), header_row + 1L)
  if (header_row >= nrow(probe)) {
    return(list(row = header_row + 1L, score = 0, status = "Review"))
  }

  headers <- import_row_values(probe, header_row)
  candidate_rows <- seq.int(header_row + 1L, nrow(probe))
  scores <- vapply(
    candidate_rows,
    function(i) score_cgm_reading_row(import_row_values(probe, i), headers = headers, profile = profile),
    numeric(1)
  )
  best_index <- which.max(scores)
  best_score <- scores[[best_index]]
  list(
    row = if (is.finite(best_score) && best_score >= 12) as.integer(candidate_rows[[best_index]]) else as.integer(default_row),
    score = best_score,
    status = if (is.finite(best_score) && best_score >= 12) "Ready" else "Review"
  )
}

read_cgm_file_probe <- function(datapath, filename, n_max = import_probe_row_limit()) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("csv", "txt")) {
    data <- data.table::fread(
      datapath,
      sep = cgm_fread_separator(filename),
      header = FALSE,
      nrows = n_max,
      fill = TRUE,
      blank.lines.skip = FALSE,
      data.table = FALSE,
      showProgress = FALSE
    )
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel files.", call. = FALSE)
    }
    data <- readxl::read_excel(
      datapath,
      col_names = FALSE,
      n_max = n_max,
      .name_repair = "minimal"
    )
  } else {
    stop("Unsupported file type: ", ext, call. = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  names(data) <- paste0("Column ", seq_along(data))
  data[] <- lapply(data, as.character)
  data
}

import_row_values <- function(probe, row_index) {
  if (!is.data.frame(probe) || !nrow(probe) || row_index < 1L || row_index > nrow(probe)) {
    return(character())
  }
  values <- as.character(unlist(probe[row_index, , drop = TRUE], use.names = FALSE))
  values[is.na(values)] <- ""
  trimws(values)
}

score_cgm_header_row <- function(values) {
  values <- trimws(as.character(values))
  values[is.na(values)] <- ""
  non_empty <- nzchar(values)
  if (!any(non_empty)) {
    return(0)
  }

  observed <- values[non_empty]
  normalized <- normalize_import_text(observed)
  numeric_like <- grepl("^[-+]?\\d+(\\.\\d+)?$", normalized)
  keyword_hits <- sum(vapply(
    cgm_header_keyword_patterns(),
    function(pattern) any(grepl(pattern, normalized, perl = TRUE)),
    logical(1)
  ))
  label_like <- sum(!numeric_like)

  length(observed) +
    keyword_hits * 5 +
    label_like * 0.5 -
    sum(numeric_like) * 0.75
}

detect_cgm_import_profile <- function(probe) {
  text <- normalize_import_text(unlist(probe, use.names = FALSE))
  text <- text[nzchar(text)]
  if (!length(text)) {
    return("Unknown CGM")
  }
  collapsed <- paste(text, collapse = " ")
  profiles <- cgm_import_profile_patterns()
  if (any(grepl(paste(profiles$dexcom, collapse = "|"), collapsed, perl = TRUE))) {
    return("Dexcom")
  }
  if (any(grepl(paste(profiles$abbott_libre, collapse = "|"), collapsed, perl = TRUE))) {
    return("Abbott/Libre")
  }
  if (any(grepl(paste(profiles$medtronic, collapse = "|"), collapsed, perl = TRUE))) {
    return("Medtronic")
  }
  if (max(vapply(seq_len(nrow(probe)), function(i) score_cgm_header_row(import_row_values(probe, i)), numeric(1)), 0) >= 8) {
    return("Generic CGM")
  }
  "Unknown CGM"
}

detect_cgm_header_row <- function(probe) {
  if (!is.data.frame(probe) || !nrow(probe)) {
    return(list(row = 1L, score = 0, status = "Review"))
  }
  scores <- vapply(
    seq_len(nrow(probe)),
    function(i) score_cgm_header_row(import_row_values(probe, i)),
    numeric(1)
  )
  best <- which.max(scores)
  score <- scores[[best]]
  list(
    row = if (is.finite(score) && score >= 6) as.integer(best) else 1L,
    score = score,
    status = if (is.finite(score) && score >= 8) "Ready" else "Review"
  )
}

probe_cgm_file_import <- function(datapath, filename) {
  probe <- read_cgm_file_probe(datapath, filename)
  detected <- detect_cgm_header_row(probe)
  profile <- detect_cgm_import_profile(probe)
  first_reading <- detect_cgm_first_reading_row(probe, detected$row, profile = profile)
  list(
    file = filename,
    datapath = datapath,
    profile = profile,
    suggested_header_row = detected$row,
    suggested_first_data_row = first_reading$row,
    detection_score = detected$score,
    first_data_score = first_reading$score,
    detection_status = detected$status,
    first_data_status = first_reading$status,
    preview = probe
  )
}

clean_import_column_names <- function(columns) {
  columns <- trimws(as.character(columns))
  missing <- is.na(columns) | !nzchar(columns)
  columns[missing] <- paste0("Column ", which(missing))
  make.unique(columns, sep = " ")
}

import_setup_needed <- function(probes) {
  if (!length(probes)) {
    return(FALSE)
  }
  any(vapply(probes, function(probe) {
    !identical(probe$detection_status, "Ready") ||
      !identical(probe$first_data_status, "Ready") ||
      !identical(as.integer(probe$suggested_first_data_row), as.integer(probe$suggested_header_row) + 1L) ||
      !identical(as.integer(probe$suggested_header_row), 1L)
  }, logical(1)))
}

normalize_first_data_row <- function(header_row, first_data_row) {
  header_row <- suppressWarnings(as.integer(header_row %||% 1L))
  first_data_row <- suppressWarnings(as.integer(first_data_row %||% (header_row + 1L)))
  if (is.na(header_row) || header_row < 1L) {
    header_row <- 1L
  }
  if (is.na(first_data_row) || first_data_row <= header_row) {
    first_data_row <- header_row + 1L
  }
  first_data_row
}

selected_import_row_boundaries <- function(probes, input = NULL) {
  if (!length(probes)) {
    return(data.frame(
      header_row = integer(),
      first_data_row = integer()
    ))
  }
  header_rows <- vapply(seq_along(probes), function(i) {
    input_id <- paste0("header_row_", i)
    selected <- if (!is.null(input)) input[[input_id]] else NULL
    selected <- suppressWarnings(as.integer(selected %||% probes[[i]]$suggested_header_row))
    if (is.na(selected) || selected < 1L) {
      selected <- probes[[i]]$suggested_header_row
    }
    max(1L, selected)
  }, integer(1))
  first_rows <- vapply(seq_along(probes), function(i) {
    input_id <- paste0("first_data_row_", i)
    selected <- if (!is.null(input)) input[[input_id]] else NULL
    selected <- suppressWarnings(as.integer(selected %||% probes[[i]]$suggested_first_data_row))
    normalize_first_data_row(header_rows[[i]], selected)
  }, integer(1))
  data.frame(
    header_row = header_rows,
    first_data_row = first_rows
  )
}

selected_import_header_rows <- function(probes, input = NULL) {
  selected_import_row_boundaries(probes, input)$header_row
}

selected_import_first_data_rows <- function(probes, input = NULL) {
  selected_import_row_boundaries(probes, input)$first_data_row
}

format_import_preview <- function(probe, rows = import_preview_row_limit()) {
  preview <- utils::head(probe$preview, rows)
  preview <- cbind(`File row` = seq_len(nrow(preview)), preview, stringsAsFactors = FALSE)
  preview
}

render_import_preview_table <- function(probe) {
  preview <- format_import_preview(probe)
  header <- shiny::tags$tr(lapply(names(preview), shiny::tags$th))
  body <- lapply(seq_len(nrow(preview)), function(i) {
    style <- if (identical(i, probe$suggested_header_row)) {
      "background:#e8f4ff;"
    } else if (identical(i, probe$suggested_first_data_row)) {
      "background:#eaf7ea;"
    } else {
      NULL
    }
    shiny::tags$tr(
      style = style,
      lapply(preview[i, , drop = TRUE], function(value) {
        shiny::tags$td(as.character(value %||% ""))
      })
    )
  })
  shiny::tags$div(
    style = "overflow-x:auto; max-height:220px; overflow-y:auto;",
    shiny::tags$table(
      class = "table table-sm table-bordered",
      shiny::tags$thead(header),
      shiny::tags$tbody(body)
    )
  )
}

import_setup_panel_ui <- function(probes, ns) {
  if (!import_setup_needed(probes)) {
    return(NULL)
  }
  shiny::div(
    class = "card",
    style = "margin-bottom:16px;",
    shiny::div(
      class = "card-body",
      shiny::h3("Import setup"),
      shiny::p(
        "Review the detected row boundaries before mapping columns. Header row defines column names; First reading row skips metadata/settings rows before CGM readings. You will still select Timestamp and Glucose manually."
      ),
      lapply(seq_along(probes), function(i) {
        probe <- probes[[i]]
        shiny::div(
          class = "border rounded p-2 mb-3",
          shiny::tags$strong(probe$file),
          shiny::tags$p(
            style = "margin:6px 0;",
            paste0(
              "Detected profile: ", probe$profile,
              " | Suggested header row: ", probe$suggested_header_row,
              " | Suggested first reading row: ", probe$suggested_first_data_row,
              " | Status: ", probe$detection_status
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                ns(paste0("header_row_", i)),
                "Header row",
                value = probe$suggested_header_row,
                min = 1,
                step = 1,
                width = "180px"
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                ns(paste0("first_data_row_", i)),
                "First reading row",
                value = probe$suggested_first_data_row,
                min = 2,
                step = 1,
                width = "180px"
              )
            )
          ),
          render_import_preview_table(probe)
        )
      })
    )
  )
}

compatible_uploaded_schemas <- function(data_list) {
  if (length(data_list) <= 1L) {
    return(TRUE)
  }
  schemas <- lapply(data_list, function(data) {
    user_mapping_columns(names(data))
  })
  first <- schemas[[1L]]
  all(vapply(schemas[-1L], function(schema) identical(schema, first), logical(1)))
}

required_preview_mappings_selected <- function(mapping) {
  is.list(mapping) &&
    nzchar(mapping$timestamp %||% "") &&
    nzchar(mapping$glucose %||% "")
}
