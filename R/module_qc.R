qc_module_ui <- function(id) {
  ns <- shiny::NS(id)
      shiny::tagList(
      shiny::h3("Quality control"),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("qc_summary_cards")), type = 4),
    shiny::uiOutput(ns("duplicate_timestamp_note")),
    shiny::h4("Analysis data QC"),
    shinycssloaders::withSpinner(DT::DTOutput(ns("qc_table")), type = 4),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("imputation_status")), type = 4),
    shiny::h4("Missingness Summary"),
    shinycssloaders::withSpinner(DT::DTOutput(ns("missingness_comparison_table")), type = 4),
    shiny::h4("Daily data coverage"),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::uiOutput(ns("missingness_subject_filter"))
      )
    ),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("missingness_heatmap_ui")), type = 4)
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
        valid_day_hours = settings()$valid_day_hours,
        include_preprocessing = should_show_analysis_missingness(settings())
      )
    }),
    cgm_data_signature(standardized()),
    cgm_data_signature(analysis_data()),
    settings()$valid_day_hours,
    should_show_analysis_missingness(settings())
    )

    imputation_status <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      summarize_imputation_status(standardized(), analysis_data(), settings())
    }),
    cgm_data_signature(standardized()),
    cgm_data_signature(analysis_data()),
    imputation_settings_signature(settings())
    )

    gap_periods <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      detect_gap_periods(analysis_data())
    }),
    cgm_data_signature(analysis_data())
    )

    missingness_calendar_data <- shiny::reactive({
      req_active_tab(active_tab, "quality")
      data <- analysis_data()
      gaps <- gap_periods()
      participant <- normalize_filter_value(input$missingness_participant)
      if (nzchar(participant)) {
        data <- data[data$id == participant, , drop = FALSE]
        if (is.data.frame(gaps) && nrow(gaps) && "id" %in% names(gaps)) {
          gaps <- gaps[gaps$id == participant, , drop = FALSE]
        }
      }
      compute_missingness_calendar_data(
        data,
        gaps = gaps,
        date_range = settings()$analysis_date_range
      )
    })

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

    output$duplicate_timestamp_note <- shiny::renderUI({
      note <- duplicate_timestamp_note(qc_summary(), analysis_data())
      if (is.null(note)) {
        return(NULL)
      }
      shiny::div(
        class = "alert alert-warning",
        shiny::strong("Duplicate timestamps need review"),
        shiny::tags$p(note$message)
      )
    })

    output$imputation_status <- shiny::renderUI({
      if (!should_show_analysis_missingness(settings())) {
        return(NULL)
      }
      status <- imputation_status()
      alert_class <- switch(
        status$Status[[1L]],
        "Applied" = "alert alert-success",
        "Unavailable" = "alert alert-warning",
        "Could not apply" = "alert alert-warning",
        "No rows filled" = "alert alert-warning",
        "alert alert-info"
      )

      shiny::div(
        shiny::h4("Imputation status"),
        class = alert_class,
        shiny::strong(status$Status[[1L]]),
        shiny::tags$p(status$Message[[1L]]),
        shiny::tags$dl(
          class = "row",
          shiny::tags$dt(class = "col-sm-3", "Method"),
          shiny::tags$dd(class = "col-sm-9", status$Method[[1L]]),
          shiny::tags$dt(class = "col-sm-3", "Backend"),
          shiny::tags$dd(class = "col-sm-9", status$Backend[[1L]]),
          shiny::tags$dt(class = "col-sm-3", "Seed"),
          shiny::tags$dd(class = "col-sm-9", status$Seed[[1L]]),
          shiny::tags$dt(class = "col-sm-3", "Rows filled"),
          shiny::tags$dd(class = "col-sm-9", status[["Filled glucose rows"]][[1L]])
        ),
        if (nrow(preprocessing_comparison_summary(status))) {
          shiny::tagList(
            shiny::tags$hr(),
            shiny::h5("Before vs after preprocessing"),
            summary_card_ui(preprocessing_comparison_summary(status), compact = TRUE),
            shiny::tags$p(
              class = "help-block",
              "Estimated missing readings from timestamp gaps are QC context only. ",
              "This imputation workflow fills existing missing glucose rows and does not insert new timestamp rows."
            )
          )
        }
      )
    })

    output$missingness_comparison_table <- DT::renderDT({
      DT::datatable(missingness_comparison(), rownames = FALSE, options = list(scrollX = FALSE, pageLength = 10))
    })

    output$missingness_heatmap_ui <- shiny::renderUI({
      req_active_tab(active_tab, "quality")
      dimensions <- missingness_calendar_dimensions(
        missingness_calendar_data(),
        show_subject_id = subject_id_filter_available(analysis_data())
      )
      plotly::plotlyOutput(
        session$ns("missingness_heatmap"),
        height = paste0(dimensions$height, "px")
      )
    })

    output$missingness_heatmap <- plotly::renderPlotly({
      create_missingness_heatmap_plot(
        analysis_data(),
        calendar_data = missingness_calendar_data()
      )
    })

    qc_summary
  })
}
