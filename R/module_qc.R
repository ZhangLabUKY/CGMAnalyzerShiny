qc_module_ui <- function(id) {
  ns <- shiny::NS(id)
    shiny::tagList(
      shiny::h3("Quality Control"),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("qc_summary_cards")), type = 4),
    shiny::h4("Analysis Data QC"),
    shinycssloaders::withSpinner(DT::DTOutput(ns("qc_table")), type = 4),
    shiny::h4("Imputation Status"),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("imputation_status")), type = 4),
    shiny::h4("Missingness Summary"),
    shinycssloaders::withSpinner(DT::DTOutput(ns("missingness_comparison_table")), type = 4),
    shiny::h4("Daily Data Coverage"),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::uiOutput(ns("missingness_subject_filter"))
      )
    ),
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("missingness_heatmap"), height = "420px"), type = 4)
  )
}

qc_module_server <- function(id, standardized, analysis_data, settings, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    qc_summary <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, c("quality", "export"))
      compute_qc_summary(
        analysis_data(),
        valid_day_hours = settings()$valid_day_hours
      )
    }),
    cgm_data_signature(analysis_data()),
    settings()$valid_day_hours
    )

    missingness_comparison <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      compare_missingness_summaries(
        standardized(),
        analysis_data(),
        valid_day_hours = settings()$valid_day_hours
      )
    }),
    cgm_data_signature(standardized()),
    cgm_data_signature(analysis_data()),
    settings()$valid_day_hours
    )

    imputation_status <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      summarize_imputation_status(standardized(), analysis_data(), settings())
    }),
    cgm_data_signature(standardized()),
    cgm_data_signature(analysis_data()),
    settings()$imputation_method,
    settings()$imputation_seed
    )

    gap_periods <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      detect_gap_periods(analysis_data())
    }),
    cgm_data_signature(analysis_data())
    )

    output$missingness_subject_filter <- shiny::renderUI({
      req_active_tab(active_tab, "quality")
      data <- analysis_data()
      if (!subject_id_filter_available(data)) {
        return(NULL)
      }
      choices <- filter_select_choices(sort(subject_id_values(data)), all_label = "All")
      shiny::selectInput(
        session$ns("missingness_participant"),
        "Subject ID",
        choices = choices,
        selected = preserve_filter_selection(input$missingness_participant, choices)
      )
    })

    output$qc_table <- DT::renderDT({
      DT::datatable(prepare_qc_display(qc_summary(), analysis_data()), options = list(scrollX = TRUE, pageLength = 10))
    })

    output$qc_summary_cards <- shiny::renderUI({
      summary_card_ui(quality_summary_cards(analysis_data(), qc_summary(), missingness_comparison()), compact = TRUE)
    })

    output$imputation_status <- shiny::renderUI({
      status <- imputation_status()
      alert_class <- switch(
        status$Status[[1L]],
        "Applied" = "alert alert-success",
        "Unavailable" = "alert alert-warning",
        "No rows filled" = "alert alert-warning",
        "alert alert-info"
      )

      shiny::div(
        class = alert_class,
        shiny::strong(status$Status[[1L]]),
        shiny::tags$p(status$Message[[1L]]),
        shiny::tags$dl(
          class = "row",
          shiny::tags$dt(class = "col-sm-3", "Method"),
          shiny::tags$dd(class = "col-sm-9", status$Method[[1L]]),
          shiny::tags$dt(class = "col-sm-3", "Seed"),
          shiny::tags$dd(class = "col-sm-9", status$Seed[[1L]]),
          shiny::tags$dt(class = "col-sm-3", "Rows filled"),
          shiny::tags$dd(class = "col-sm-9", status[["Filled glucose rows"]][[1L]])
        )
      )
    })

    output$missingness_comparison_table <- DT::renderDT({
      DT::datatable(missingness_comparison(), rownames = FALSE, options = list(scrollX = FALSE, pageLength = 10))
    })

    output$missingness_heatmap <- plotly::renderPlotly({
      create_missingness_heatmap_plot(
        analysis_data(),
        gaps = gap_periods(),
        participant = input$missingness_participant,
        date_range = settings()$analysis_date_range
      )
    })

    qc_summary
  })
}
