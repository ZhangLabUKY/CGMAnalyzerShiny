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
