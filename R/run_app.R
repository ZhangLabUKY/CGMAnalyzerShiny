#' Run the CGManalyzer2 app
#'
#' @param ... Additional arguments passed to `shiny::shinyApp()`.
#'
#' @return A Shiny application object.
#' @export
run_app <- function(...) {
  if (exists("cgm_bootstrap_native_symbols", mode = "function")) {
    cgm_bootstrap_native_symbols(quiet = TRUE)
  }
  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
