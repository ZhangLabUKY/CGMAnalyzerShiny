subject_id_source_mapped <- function() {
  "mapped_column"
}

subject_id_source_filename <- function() {
  "filename"
}

determine_id_source <- function(data, mapping, upload_mode = "single_file") {
  id_mapping <- clean_mapping_value(mapping$id)
  if (identical(upload_mode, "multi_file")) {
    return(subject_id_source_filename())
  }
  if (is.na(id_mapping) || identical(id_mapping, ".source_id")) {
    return(subject_id_source_filename())
  }
  subject_id_source_mapped()
}

has_user_subject_id <- function(data) {
  is.data.frame(data) &&
    "id_source" %in% names(data) &&
    any(data$id_source %in% subject_id_source_mapped(), na.rm = TRUE)
}

subject_id_values <- function(data) {
  if (!is.data.frame(data) || !"id" %in% names(data)) {
    return(character())
  }
  clean_filter_values(data$id)
}

subject_id_filter_available <- function(data) {
  values <- subject_id_values(data)
  if (!length(values)) {
    return(FALSE)
  }
  if (length(values) > 1L) {
    return(TRUE)
  }
  if (!"id_source" %in% names(data)) {
    return(TRUE)
  }
  has_user_subject_id(data)
}

show_subject_id_for_display <- function(data = NULL, show_subject_id = NULL) {
  if (!is.null(show_subject_id)) {
    return(isTRUE(show_subject_id))
  }
  subject_id_filter_available(data)
}
