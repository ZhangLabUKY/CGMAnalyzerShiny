cgm_with_progress <- function(message, expr, detail = NULL, value = 0, session = shiny::getDefaultReactiveDomain()) {
  expr <- substitute(expr)
  env <- parent.frame()
  if (is.null(session)) {
    return(eval(expr, envir = env))
  }
  shiny::withProgress(
    expr = eval(expr, envir = env),
    message = message,
    detail = detail,
    value = value,
    session = session
  )
}
