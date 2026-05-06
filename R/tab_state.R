default_active_tab <- function() {
  "data"
}

active_tab_value <- function(active_tab) {
  if (is.null(active_tab)) {
    return(NA_character_)
  }
  value <- active_tab()
  value %||% default_active_tab()
}

is_active_tab <- function(active_tab, tabs) {
  if (is.null(active_tab)) {
    return(TRUE)
  }
  active_tab_value(active_tab) %in% tabs
}

req_active_tab <- function(active_tab, tabs) {
  shiny::req(is_active_tab(active_tab, tabs))
}
