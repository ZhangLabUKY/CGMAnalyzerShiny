preview_row_choices <- function() {
  c("5" = "5", "10" = "10", "25" = "25", "50" = "50", "100" = "100", "All" = "all")
}

preview_data_rows <- function(data, row_limit = "10") {
  row_limit <- row_limit %||% "10"
  if (identical(row_limit, "all")) {
    return(data)
  }
  limit <- suppressWarnings(as.integer(row_limit))
  if (is.na(limit) || limit < 1L) {
    limit <- 10L
  }
  utils::head(data, limit)
}

prepare_upload_preview_data <- function(data, upload_mode = "single_file") {
  internal_source_cols <- intersect(c(".source_file", ".source_id"), names(data))
  preview <- data

  if (identical(upload_mode, "multi_file") && ".source_file" %in% names(preview)) {
    preview[["Source file"]] <- as.character(preview[[".source_file"]])
  }

  if (length(internal_source_cols) > 0L) {
    preview <- preview[, setdiff(names(preview), internal_source_cols), drop = FALSE]
  }

  if ("Source file" %in% names(preview)) {
    preview <- preview[, c("Source file", setdiff(names(preview), "Source file")), drop = FALSE]
  }

  preview
}

preview_dt_options <- function(page_length = 10) {
  list(
    scrollX = TRUE,
    scrollY = "420px",
    pageLength = page_length,
    lengthChange = FALSE
  )
}

uploaded_file_names <- function(uploaded) {
  as.character(uploaded$files %||% character())
}

internal_upload_source_columns <- function() {
  c(".source_file", ".source_id")
}

user_mapping_columns <- function(columns) {
  setdiff(columns, internal_upload_source_columns())
}

required_mapping_choices <- function(columns) {
  c(stats::setNames("", ""), columns)
}

mapping_choices_for_upload <- function(uploaded) {
  columns <- user_mapping_columns(names(uploaded$data))
  optional_choices <- c(stats::setNames("", ""), columns)
  list(
    columns = columns,
    required_choices = required_mapping_choices(columns),
    optional_choices = optional_choices,
    id = if (is_multi_file_upload(uploaded)) ".source_id" else "",
    timestamp = "",
    glucose = "",
    group = "",
    visit = ""
  )
}
