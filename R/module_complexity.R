complexity_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Complexity Analytics"),
    shiny::p("Entropy, Hurst exponent, DFA, and fractal metrics will be added after preprocessing and missingness handling are validated."),
    shinycssloaders::withSpinner(DT::DTOutput(ns("data_requirements")), type = 4)
  )
}

complexity_module_server <- function(id, standardized, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$data_requirements <- DT::renderDT({
      req_active_tab(active_tab, "complexity")
      data <- standardized()
      summary <- compute_qc_summary(data)
      DT::datatable(summary[, c("id", "readings", "median_interval_minutes", "gap_count", "max_gap_minutes")],
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })
  })
}
