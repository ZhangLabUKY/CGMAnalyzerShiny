required_cgm_columns <- function() {
  c("id", "timestamp", "glucose")
}

optional_cgm_columns <- function() {
  c("device")
}

clean_mapping_value <- function(x) {
  if (
    is.null(x) || length(x) == 0L || is.na(x[[1L]]) || identical(x[[1L]], "")
  ) {
    return(NA_character_)
  }
  as.character(x[[1L]])
}

is_multi_file_upload <- function(uploaded) {
  identical(uploaded$upload_mode %||% "single_file", "multi_file")
}

derive_source_id <- function(filename) {
  tools::file_path_sans_ext(basename(filename))
}

standardize_upload_mapping <- function(mapping, upload_mode = "single_file") {
  subject_metadata <- mapping$subject_metadata
  mapping <- mapping[setdiff(names(mapping), "subject_metadata")]
  mapping <- lapply(mapping, clean_mapping_value)
  mapping$subject_metadata <- subject_metadata
  if (identical(upload_mode, "multi_file")) {
    mapping$id <- ".source_id"
  }
  mapping
}

validate_mapping <- function(data, mapping, upload_mode = "single_file") {
  mapping <- standardize_upload_mapping(mapping, upload_mode = upload_mode)
  if (is.na(clean_mapping_value(mapping$id)) && ".source_id" %in% names(data)) {
    mapping$id <- ".source_id"
  }
  column_mapping_names <- c(required_cgm_columns(), optional_cgm_columns())
  column_mapping <- stats::setNames(
    vapply(
      column_mapping_names,
      function(name) clean_mapping_value(mapping[[name]]),
      character(1)
    ),
    column_mapping_names
  )
  missing_required <- required_cgm_columns()[is.na(column_mapping[required_cgm_columns()])]
  if (length(missing_required) > 0L) {
    stop(
      "Missing required mapping: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  unknown <- setdiff(column_mapping[!is.na(column_mapping)], names(data))
  if (length(unknown) > 0L) {
    stop(
      "Mapped columns not found in data: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  mapping
}

blank_metadata_value <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

clean_subject_metadata <- function(metadata) {
  if (is.null(metadata) || !is.data.frame(metadata) || !nrow(metadata)) {
    return(data.frame(id = character(), stringsAsFactors = FALSE))
  }
  out <- as.data.frame(metadata, stringsAsFactors = FALSE)
  if (!"id" %in% names(out)) {
    first_name <- names(out)[[1L]]
    names(out)[names(out) == first_name] <- "id"
  }
  out$id <- as.character(out$id)
  out <- out[!is.na(out$id) & nzchar(trimws(out$id)), , drop = FALSE]
  out$id <- trimws(out$id)
  metadata_cols <- setdiff(names(out), "id")
  if (!length(metadata_cols)) {
    return(out[, "id", drop = FALSE])
  }
  for (col in metadata_cols) {
    out[[col]] <- trimws(as.character(out[[col]]))
    out[[col]][blank_metadata_value(out[[col]])] <- NA_character_
  }
  keep_cols <- metadata_cols[vapply(metadata_cols, function(col) any(!is.na(out[[col]])), logical(1))]
  out <- out[, c("id", keep_cols), drop = FALSE]
  out <- out[!duplicated(out$id), , drop = FALSE]
  row.names(out) <- NULL
  out
}

apply_subject_metadata <- function(data, metadata) {
  metadata <- clean_subject_metadata(metadata)
  metadata_cols <- setdiff(names(metadata), "id")
  if (!length(metadata_cols) || !nrow(data) || !"id" %in% names(data)) {
    return(data)
  }
  matched <- match(as.character(data$id), metadata$id)
  for (col in metadata_cols) {
    data[[col]] <- metadata[[col]][matched]
  }
  data
}

#' Parse CGM timestamps
#'
#' @param x Timestamp vector.
#' @param tz Time zone used when timestamps do not include one.
#' @param date_order Date order for non-year-first dates. Defaults to
#'   day-first for CGM device exports.
#'
#' @return POSIXct vector.
#' @noRd
parse_cgm_timestamp <- function(x, tz = "UTC", date_order = "dmy") {
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  if (inherits(x, "POSIXlt")) {
    return(as.POSIXct(x, tz = tz))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = tz))
  }

  if (is.numeric(x)) {
    return(as.POSIXct((x - 25569) * 86400, origin = "1970-01-01", tz = tz))
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  date_order <- clean_timestamp_date_order(date_order)

  parsed <- rep(
    as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz),
    length(x_chr)
  )
  numeric_idx <- !is.na(x_chr) & grepl("^\\d+(\\.\\d+)?$", x_chr)
  numeric_values <- suppressWarnings(as.numeric(x_chr[numeric_idx]))
  excel_idx <- numeric_idx
  excel_idx[numeric_idx] <- !is.na(numeric_values) &
    numeric_values > 20000 &
    numeric_values < 80000
  if (any(excel_idx)) {
    parsed[excel_idx] <- as.POSIXct(
      (as.numeric(x_chr[excel_idx]) - 25569) * 86400,
      origin = "1970-01-01",
      tz = tz
    )
  }

  year_first_formats <- c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y:%m:%d:%H:%M:%S",
    "%Y:%m:%d:%H:%M",
    "%Y-%m-%d %I:%M:%S %p",
    "%Y-%m-%d %I:%M %p",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%OS%z"
  )

  parsed <- parse_with_formats(x_chr, parsed, year_first_formats, tz = tz)

  day_month_formats <- timestamp_date_order_formats(date_order)
  parseable_non_year_first <- !is.na(x_chr) & is.na(parsed)
  parsed <- parse_with_formats(
    x_chr,
    parsed,
    day_month_formats,
    tz = tz,
    idx = parseable_non_year_first
  )

  parsed
}

clean_timestamp_date_order <- function(date_order) {
  date_order <- clean_mapping_value(date_order)
  if (is.na(date_order)) {
    return(NULL)
  }
  date_order <- tolower(date_order)
  if (date_order %in% c("month_first", "mdy", "month-day-year")) {
    return("mdy")
  }
  if (date_order %in% c("day_first", "dmy", "day-month-year")) {
    return("dmy")
  }
  NULL
}

timestamp_date_order_formats <- function(date_order = NULL) {
  mdy <- c(
    "%m/%d/%Y %I:%M:%S %p",
    "%m/%d/%Y %I:%M %p",
    "%m-%d-%Y %I:%M:%S %p",
    "%m-%d-%Y %I:%M %p",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%m/%d/%y %H:%M:%S",
    "%m/%d/%y %H:%M",
    "%m-%d-%Y %H:%M:%S",
    "%m-%d-%Y %H:%M",
    "%m-%d-%y %H:%M:%S",
    "%m-%d-%y %H:%M"
  )
  dmy <- c(
    "%d/%m/%Y %I:%M:%S %p",
    "%d/%m/%Y %I:%M %p",
    "%d-%m-%Y %I:%M:%S %p",
    "%d-%m-%Y %I:%M %p",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d/%m/%y %H:%M:%S",
    "%d/%m/%y %H:%M",
    "%d-%m-%Y %H:%M:%S",
    "%d-%m-%Y %H:%M",
    "%d-%m-%y %H:%M:%S",
    "%d-%m-%y %H:%M"
  )
  if (identical(date_order, "dmy")) {
    c(dmy, mdy)
  } else {
    c(mdy, dmy)
  }
}

parse_with_formats <- function(x_chr, parsed, formats, tz = "UTC", idx = NULL) {
  remaining <- if (is.null(idx)) {
    !is.na(x_chr) & is.na(parsed)
  } else {
    idx & !is.na(x_chr) & is.na(parsed)
  }
  for (fmt in formats) {
    if (!any(remaining)) {
      break
    }
    attempt <- as.POSIXct(strptime(x_chr[remaining], format = fmt, tz = tz))
    fill <- !is.na(attempt)
    parsed[which(remaining)[fill]] <- attempt[fill]
    remaining[which(remaining)[fill]] <- FALSE
  }
  parsed
}

detect_ambiguous_timestamps <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  out <- rep(FALSE, length(x_chr))
  idx <- !is.na(x_chr) &
    grepl("^\\d{1,2}[-/]\\d{1,2}[-/]\\d{2,4}(\\D|$)", x_chr) &
    !grepl("^\\d{4}[-/]", x_chr)
  parts <- regmatches(
    x_chr[idx],
    regexec("^(\\d{1,2})[-/](\\d{1,2})[-/](\\d{2,4})", x_chr[idx])
  )
  out[which(idx)] <- vapply(
    parts,
    function(part) {
      if (length(part) < 4L) {
        return(FALSE)
      }
      first <- suppressWarnings(as.integer(part[[2L]]))
      second <- suppressWarnings(as.integer(part[[3L]]))
      !is.na(first) &&
        !is.na(second) &&
        first <= 12L &&
        second <= 12L &&
        first != second
    },
    logical(1)
  )
  out
}

timestamp_parse_summary <- function(x, tz = "UTC", date_order = "dmy") {
  parsed <- parse_cgm_timestamp(x, tz = tz, date_order = date_order)
  non_missing <- !is.na(trimws(as.character(x))) &
    nzchar(trimws(as.character(x)))
  data.frame(
    rows = length(x),
    non_missing_timestamps = sum(non_missing),
    parsed_timestamps = sum(!is.na(parsed)),
    failed_timestamps = sum(non_missing & is.na(parsed)),
    ambiguous_timestamps = sum(detect_ambiguous_timestamps(x)),
    first_timestamp = if (any(!is.na(parsed))) {
      min(parsed, na.rm = TRUE)
    } else {
      as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz)
    },
    last_timestamp = if (any(!is.na(parsed))) {
      max(parsed, na.rm = TRUE)
    } else {
      as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz)
    },
    stringsAsFactors = FALSE
  )
}

has_ambiguous_timestamps <- function(x) {
  any(detect_ambiguous_timestamps(x), na.rm = TRUE)
}

format_cgm_timestamp_iso <- function(timestamp, tz = "UTC") {
  out <- format(as.POSIXct(timestamp, tz = tz), "%Y-%m-%dT%H:%M:%S", tz = tz)
  out[is.na(timestamp)] <- NA_character_
  out
}

format_cgmanalyzer_timestamp <- function(timestamp, tz = "UTC") {
  out <- format(as.POSIXct(timestamp, tz = tz), "%Y:%m:%d:%H:%M", tz = tz)
  out[is.na(timestamp)] <- NA_character_
  out
}

prepare_cgm_data_export <- function(data, tz = "UTC") {
  out <- as.data.frame(data, stringsAsFactors = FALSE)
  if ("timestamp" %in% names(out)) {
    out$timestamp <- format_cgm_timestamp_iso(out$timestamp, tz = tz)
  }
  out
}

validate_parsed_timestamps <- function(raw_timestamp, parsed_timestamp) {
  non_missing <- !is.na(trimws(as.character(raw_timestamp))) &
    nzchar(trimws(as.character(raw_timestamp)))
  failed <- non_missing & is.na(parsed_timestamp)
  if (any(failed)) {
    examples <- unique(as.character(raw_timestamp[failed]))
    examples <- examples[seq_len(min(length(examples), 3L))]
    stop(
      "Unable to parse timestamp values. Check the selected timestamp column. Example value(s): ",
      paste(examples, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

coerce_glucose <- function(x) {
  as.numeric(gsub(",", "", as.character(x), fixed = TRUE))
}

convert_glucose_to_mg_dl <- function(glucose, units) {
  units_norm <- tolower(trimws(units))
  if (units_norm %in% c("mmol/l", "mmol", "mmol/liter", "mmoll")) {
    return(glucose * 18.0182)
  }
  glucose
}

#' Standardize uploaded CGM data
#'
#' @param data Input data frame.
#' @param mapping Named list with id, timestamp, glucose, and optional device
#'   and group entries.
#' @param units Source glucose units, either mg/dL or mmol/L.
#' @param source_file Optional source file label.
#' @param tz Time zone used for timestamp parsing.
#' @param timestamp_date_order Date order for non-year-first dates.
#'
#' @return Standardized CGM data frame.
#' @noRd
standardize_cgm_data <- function(
  data,
  mapping,
  units = "mg/dL",
  source_file = NA_character_,
  tz = "UTC",
  upload_mode = "single_file",
  timestamp_date_order = "dmy"
) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  id_source <- determine_id_source(data, mapping, upload_mode = upload_mode)
  mapping <- validate_mapping(data, mapping, upload_mode = upload_mode)
  timestamp <- parse_cgm_timestamp(
    data[[mapping$timestamp]],
    tz = tz,
    date_order = timestamp_date_order
  )
  validate_parsed_timestamps(data[[mapping$timestamp]], timestamp)

  out <- data.frame(
    id = as.character(data[[mapping$id]]),
    id_source = id_source,
    timestamp = timestamp,
    glucose = convert_glucose_to_mg_dl(
      coerce_glucose(data[[mapping$glucose]]),
      units
    ),
    units = "mg/dL",
    device = NA_character_,
    source_file = source_file,
    imputed_flag = FALSE,
    stringsAsFactors = FALSE
  )

  for (col in optional_cgm_columns()) {
    mapped <- clean_mapping_value(mapping[[col]])
    if (!is.na(mapped)) {
      out[[col]] <- as.character(data[[mapped]])
    }
  }
  out <- apply_subject_metadata(out, mapping$subject_metadata)

  if (".source_file" %in% names(data)) {
    out$source_file <- as.character(data[[".source_file"]])
  }

  out <- out[order(out$id, out$timestamp, na.last = TRUE), , drop = FALSE]
  row.names(out) <- NULL
  out
}

read_cgm_file <- function(datapath, filename, header_row = 1L, first_data_row = NULL) {
  ext <- tolower(tools::file_ext(filename))
  header_row <- suppressWarnings(as.integer(header_row %||% 1L))
  if (is.na(header_row) || header_row < 1L) {
    header_row <- 1L
  }
  first_data_row <- normalize_first_data_row(header_row, first_data_row %||% (header_row + 1L))
  skip_rows <- header_row - 1L
  drop_after_header <- first_data_row - header_row - 1L
  if (ext %in% c("csv", "txt")) {
    data <- data.table::fread(
      datapath,
      sep = cgm_fread_separator(filename),
      skip = skip_rows,
      header = TRUE,
      fill = TRUE,
      data.table = FALSE,
      showProgress = FALSE
    )
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel files.", call. = FALSE)
    }
    data <- readxl::read_excel(
      datapath,
      skip = skip_rows,
      col_names = TRUE,
      .name_repair = "minimal"
    )
  } else {
    stop("Unsupported file type: ", ext, call. = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (drop_after_header > 0L && nrow(data) >= drop_after_header) {
    data <- data[-seq_len(drop_after_header), , drop = FALSE]
  }
  names(data) <- clean_import_column_names(names(data))
  data[[".source_file"]] <- filename
  data[[".source_id"]] <- derive_source_id(filename)
  data[[".import_header_row"]] <- header_row
  data[[".import_first_data_row"]] <- first_data_row
  data
}

combine_uploaded_files <- function(datapaths, filenames, header_rows = NULL, first_data_rows = NULL) {
  if (is.null(header_rows)) {
    header_rows <- rep(1L, length(datapaths))
  }
  if (is.null(first_data_rows)) {
    first_data_rows <- header_rows + 1L
  }
  data_list <- Map(read_cgm_file, datapaths, filenames, header_rows, first_data_rows)
  if (!compatible_uploaded_schemas(data_list)) {
    stop(
      "Uploaded files use different resolved column names. Review header rows or upload one device/schema group at a time.",
      call. = FALSE
    )
  }
  combined <- dplyr::bind_rows(lapply(data_list, function(x) {
    x[] <- lapply(x, as.character)
    x
  }))
  row.names(combined) <- NULL
  combined
}

load_extdata_csv <- function(filename, missing_message) {
  demo_path <- system.file("extdata", filename, package = "CGMAnalyzerShiny")
  if (!nzchar(demo_path)) {
    candidates <- c(
      file.path("inst", "extdata", filename),
      file.path("..", "..", "inst", "extdata", filename),
      file.path("..", "inst", "extdata", filename)
    )
    existing <- candidates[file.exists(candidates)]
    demo_path <- if (length(existing)) existing[[1L]] else candidates[[1L]]
  }
  if (!file.exists(demo_path)) {
    stop(missing_message, call. = FALSE)
  }

  if (requireNamespace("readr", quietly = TRUE)) {
    data <- readr::read_csv(demo_path, show_col_types = FALSE)
  } else {
    data <- utils::read.csv(demo_path, stringsAsFactors = FALSE)
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!".source_file" %in% names(data)) {
    data[[".source_file"]] <- filename
  }
  if (!".source_id" %in% names(data)) {
    data[[".source_id"]] <- derive_source_id(filename)
  }
  data
}

#' Load bundled 5 percent missing example CGM data
#'
#' @return A data frame with 5 percent missing glucose example CGM readings.
#' @noRd
load_example_missing_5pct_cgm_data <- function() {
  load_extdata_csv(
    "CGMExmplDat5Pct.csv",
    "5 percent missing example CGM data file was not found."
  )
}

#' Load bundled 10 percent missing example CGM data
#'
#' @return A data frame with 10 percent missing glucose example CGM readings.
#' @noRd
load_example_missing_10pct_cgm_data <- function() {
  load_extdata_csv(
    "CGMExmplDat10Pct.csv",
    "10 percent missing example CGM data file was not found."
  )
}
