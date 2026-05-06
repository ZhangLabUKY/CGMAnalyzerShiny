#' Run the CGMAnalyzerShiny app
#'
#' @param ... Additional arguments passed to `shiny::shinyApp()`.
#'
#' @return A Shiny application object.
#' @export
run_app <- function(...) {
  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
