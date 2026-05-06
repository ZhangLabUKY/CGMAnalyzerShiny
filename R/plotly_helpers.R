clean_plotly_trace_name <- function(name) {
  if (is.null(name) || !length(name) || is.na(name)) {
    return(name)
  }
  name <- as.character(name)
  name <- sub("^\\((.*)\\)$", "\\1", name)
  sub(",\\s*1$", "", name)
}

clean_plotly_legend_names <- function(plotly_obj) {
  if (is.null(plotly_obj$x$data)) {
    return(plotly_obj)
  }
  plotly_obj$x$data <- lapply(plotly_obj$x$data, function(trace) {
    trace$name <- clean_plotly_trace_name(trace$name)
    trace
  })
  plotly_obj
}

layout_agp_plotly <- function(plotly_obj) {
  plotly_obj <- clean_plotly_legend_names(plotly_obj)
  plotly_obj$x$layout$legend <- utils::modifyList(
    plotly_obj$x$layout$legend %||% list(),
    list(
      orientation = "h",
      x = 0,
      xanchor = "left",
      y = 1.12,
      yanchor = "bottom"
    )
  )
  plotly_obj$x$layout$margin <- utils::modifyList(
    plotly_obj$x$layout$margin %||% list(),
    list(t = 86, r = 24, b = 72, l = 64)
  )
  plotly_obj
}
