all_filter_value <- function() {
  "__all__"
}

clean_filter_values <- function(values) {
  values <- as.character(values)
  values <- trimws(values)
  values <- values[!is.na(values) & nzchar(values)]
  unique(values)
}

filter_select_choices <- function(values, all_label = "All") {
  values <- clean_filter_values(values)
  c(stats::setNames(all_filter_value(), all_label), stats::setNames(values, values))
}

subject_filter_choices <- function(values, all_label = "All") {
  values <- sort(clean_filter_values(values))
  c(stats::setNames(values, values), stats::setNames(all_filter_value(), all_label))
}

default_subject_selection <- function(values) {
  values <- sort(clean_filter_values(values))
  if (!length(values)) {
    return(all_filter_value())
  }
  values[[1L]]
}

preserve_subject_filter_selection <- function(selected, choices, values) {
  selected <- selected %||% default_subject_selection(values)
  if (!length(selected)) {
    selected <- default_subject_selection(values)
  }
  selected <- selected[[1L]]
  if (selected %in% unname(choices)) {
    selected
  } else {
    default_subject_selection(values)
  }
}

filter_data_by_subject_selection <- function(
  data,
  value,
  id_col = "id"
) {
  if (!is.data.frame(data) || !nrow(data) || !id_col %in% names(data)) {
    return(data)
  }
  value <- normalize_filter_value(value)
  if (!nzchar(value)) {
    return(data)
  }
  data[as.character(data[[id_col]]) == value, , drop = FALSE]
}

normalize_filter_value <- function(value) {
  value <- value %||% ""
  if (!length(value)) {
    return("")
  }
  value <- value[[1L]]
  if (identical(value, all_filter_value())) {
    ""
  } else {
    value
  }
}

specific_filter_selected <- function(value) {
  nzchar(normalize_filter_value(value))
}

preserve_filter_selection <- function(selected, choices) {
  selected <- selected %||% all_filter_value()
  if (!length(selected)) {
    selected <- all_filter_value()
  }
  selected <- selected[[1L]]
  if (selected %in% unname(choices)) {
    selected
  } else {
    all_filter_value()
  }
}

update_filter_select <- function(session, input_id, values, selected = NULL, all_label = "All") {
  choices <- filter_select_choices(values, all_label = all_label)
  shiny::updateSelectInput(
    session,
    input_id,
    choices = choices,
    selected = preserve_filter_selection(selected, choices)
  )
  invisible(choices)
}
