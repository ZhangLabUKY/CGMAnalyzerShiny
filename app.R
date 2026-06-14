library(shiny)

local({
  r_files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
  invisible(lapply(r_files, source))
  cgm_bootstrap_native_symbols(quiet = TRUE)
  run_app()
})
