complexity_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Complexity analytics"),
    shiny::p("Complexity metrics describe regularity and long-range structure in glucose patterns after the current preprocessing choices are applied."),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("subject_filter"))),
      shiny::column(2, shiny::numericInput(ns("min_points"), "Minimum usable points", value = 100, min = 20, step = 10)),
      shiny::column(2, shiny::numericInput(ns("entropy_bin_width"), "Entropy bin width", value = 10, min = 1, step = 1)),
      shiny::column(2, shiny::numericInput(ns("embedding_dimension"), "Embedding dimension", value = 2, min = 2, step = 1)),
      shiny::column(2, shiny::numericInput(ns("tolerance_multiplier"), "Tolerance multiplier", value = 0.2, min = 0.05, step = 0.05)),
      shiny::column(1, shiny::numericInput(ns("mse_scale_max"), "MSE max scale", value = 5, min = 1, step = 1))
    ),
    shiny::fluidRow(
      shiny::column(2, shiny::numericInput(ns("higuchi_kmax"), "Higuchi kmax", value = 8, min = 2, step = 1))
    ),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("summary_cards")), type = 4),
    shiny::div(
      style = "display:flex; justify-content:space-between; align-items:center; gap:12px;",
      shiny::h4("Complexity metrics"),
      shiny::downloadButton(ns("download_complexity"), "Download CSV")
    ),
    shinycssloaders::withSpinner(DT::DTOutput(ns("metrics_table")), type = 4)
  )
}

complexity_module_server <- function(id, standardized, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$subject_filter <- shiny::renderUI({
      req_active_tab(active_tab, "complexity")
      data <- standardized()
      if (!subject_id_filter_available(data)) {
        return(NULL)
      }
      choices <- filter_select_choices(sort(subject_id_values(data)), all_label = "All")
      shiny::selectInput(
        session$ns("subject"),
        "Subject ID",
        choices = choices,
        selected = preserve_filter_selection(input$subject, choices)
      )
    })

    parameters <- shiny::reactive({
      complexity_default_parameters(
        min_points = input$min_points %||% 100,
        entropy_bin_width = input$entropy_bin_width %||% 10,
        embedding_dimension = input$embedding_dimension %||% 2,
        tolerance_multiplier = input$tolerance_multiplier %||% 0.2,
        mse_scale_max = input$mse_scale_max %||% 5,
        higuchi_kmax = input$higuchi_kmax %||% 8,
        max_gap_intervals = 4
      )
    })

    filtered_data <- shiny::reactive({
      req_active_tab(active_tab, "complexity")
      data <- standardized()
      subject <- normalize_filter_value(input$subject)
      if (nzchar(subject)) {
        data <- data[data$id == subject, , drop = FALSE]
      }
      data
    })

    complexity_results <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "complexity")
      params <- parameters()
      compute_complexity_metrics(
        filtered_data(),
        min_points = params$min_points,
        entropy_bin_width = params$entropy_bin_width,
        embedding_dimension = params$embedding_dimension,
        tolerance_multiplier = params$tolerance_multiplier,
        mse_scale_max = params$mse_scale_max,
        higuchi_kmax = params$higuchi_kmax,
        max_gap_intervals = params$max_gap_intervals
      )
    }),
    cgm_data_signature(filtered_data()),
    parameters()
    )

    output$summary_cards <- shiny::renderUI({
      summary_card_ui(complexity_summary_cards(complexity_results(), parameters()), compact = TRUE)
    })

    output$metrics_table <- DT::renderDT({
      DT::datatable(
        prepare_complexity_metrics_display(complexity_results(), filtered_data()),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    output$download_complexity <- shiny::downloadHandler(
      filename = function() "cgm_complexity_metrics.csv",
      content = function(file) {
        utils::write.csv(prepare_complexity_metrics_display(complexity_results(), filtered_data()), file, row.names = FALSE)
      }
    )

    complexity_results
  })
}
