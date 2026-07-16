functional_profiles_visual_choices <- function() {
  c(
    "Smoothed profile" = "profile",
    "Rate of change" = "rate",
    "FPCA scores" = "scores",
    "Phenotype profiles" = "phenotypes"
  )
}

functional_profiles_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Glycemic Patterns"),
    shiny::tags$style(
      "
      .functional-controls-panel {
        display: grid;
        grid-template-columns: minmax(220px, 0.85fr) minmax(360px, 1.35fr) minmax(240px, 0.9fr);
        gap: 16px;
        align-items: start;
        margin: 12px 0 18px;
      }
      .functional-control-group {
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        padding: 12px 14px 6px;
        background: #fff;
      }
      .functional-control-group h4 {
        margin: 0 0 10px;
        font-size: 15px;
        font-weight: 600;
      }
      .functional-control-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(140px, 1fr));
        gap: 10px 14px;
      }
      .functional-control-grid .form-group {
        margin-bottom: 8px;
      }
      .functional-status-note {
        margin: 10px 0 14px;
        padding: 8px 12px;
        border-radius: 6px;
        font-size: 13px;
      }
      .functional-status-note.alert-warning {
        color: #7c2d12;
        background-color: #fff7ed;
        border-color: #fed7aa;
      }
      .functional-plot-guide {
        display: flex;
        flex-wrap: wrap;
        gap: 10px 16px;
        align-items: center;
        margin: 0 0 10px;
        color: #374151;
        font-size: 13px;
      }
      .functional-plot-guide-item {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        min-height: 20px;
      }
      .functional-plot-swatch {
        width: 22px;
        height: 3px;
        border-radius: 2px;
        background: #9ca3af;
        display: inline-block;
      }
      .functional-plot-swatch.band {
        height: 12px;
        background: rgba(96, 165, 250, 0.35);
        border: 1px solid rgba(37, 99, 235, 0.65);
      }
      .functional-plot-swatch.mean {
        background: #d95f59;
      }
      .functional-plot-swatch.day {
        background: #3b82a0;
      }
      .functional-plot-swatch.point {
        width: 9px;
        height: 9px;
        border-radius: 50%;
        background: #d95f59;
      }
      .functional-plot-swatch.zero {
        background: repeating-linear-gradient(
          90deg,
          #666,
          #666 4px,
          transparent 4px,
          transparent 8px
        );
      }
      .functional-download-row {
        display: flex;
        justify-content: flex-end;
        gap: 8px;
        margin-top: 10px;
      }
      @media (max-width: 1100px) {
        .functional-controls-panel {
          grid-template-columns: 1fr;
        }
      }
      @media (max-width: 650px) {
        .functional-control-grid {
          grid-template-columns: 1fr;
        }
      }
      "
    ),
    shiny::div(
      class = "functional-controls-panel",
      shiny::div(
        class = "functional-control-group",
        shiny::h4("Selection"),
        shiny::div(
          class = "functional-control-grid",
          shiny::uiOutput(ns("subject_filter")),
          shiny::selectInput(ns("visual_mode"), "Visual type", choices = functional_profiles_visual_choices(), selected = "profile")
        )
      ),
      shiny::div(
        class = "functional-control-group",
        shiny::h4("Smoothing"),
        shiny::div(
          class = "functional-control-grid",
          shiny::numericInput(ns("grid_minutes"), "Grid minutes", value = 5, min = 1, step = 1),
          shiny::selectInput(ns("basis_type"), "Basis", choices = functional_basis_choices(), selected = "bspline"),
          shiny::numericInput(ns("basis_count"), "Basis functions", value = 15, min = 4, step = 1),
          shiny::numericInput(ns("smoothing_lambda"), "Smoothing lambda", value = 0.01, min = 0, step = 0.01)
        )
      ),
      shiny::div(
        class = "functional-control-group",
        shiny::h4("Eligibility"),
        shiny::div(
          class = "functional-control-grid",
          shiny::numericInput(ns("min_observed_points"), "Minimum points", value = 24, min = 4, step = 1),
          shiny::numericInput(ns("cluster_count"), "Phenotype groups", value = 3, min = 1, max = 8, step = 1)
        )
      )
    ),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("summary_cards")), type = 4),
    shiny::uiOutput(ns("engine_status")),
    shiny::uiOutput(ns("smoothing_warning")),
    shiny::uiOutput(ns("plot_guide")),
    shiny::uiOutput(ns("functional_visual_ui")),
    shiny::h4("Glycemic pattern features"),
    shinycssloaders::withSpinner(DT::DTOutput(ns("features_table")), type = 4),
    shiny::div(
      class = "functional-download-row",
      shiny::downloadButton(ns("download_features"), "Download full features"),
      shiny::downloadButton(ns("download_curves"), "Download curves")
    )
  )
}

functional_engine_status_ui <- function(bundle) {
  status <- bundle$engine_status
  if (!is.data.frame(status) || !nrow(status)) {
    return(NULL)
  }
  unavailable <- status[!status$Available, , drop = FALSE]
  if (!nrow(unavailable)) {
    return(NULL)
  }
  shiny::div(
    class = "alert alert-info",
    shiny::strong("FDA engine status"),
    shiny::tags$p(
      paste(
        paste0(unavailable$Engine, " unavailable for ", unavailable$Role),
        collapse = "; "
      ),
      ". Available fallback outputs remain exploratory."
    )
  )
}

functional_status_note_ui <- function(message, level = "info") {
  if (!nzchar(message %||% "")) {
    return(NULL)
  }
  shiny::div(
    class = paste("alert", paste0("alert-", level), "functional-status-note"),
    message
  )
}

functional_plot_guide_ui <- function(mode = "profile") {
  item <- function(class, label) {
    shiny::span(
      class = "functional-plot-guide-item",
      shiny::span(class = paste("functional-plot-swatch", class)),
      label
    )
  }
  items <- switch(
    mode %||% "profile",
    rate = list(
      item("mean", "Mean rate-of-change curve"),
      item("zero", "Zero-change reference line")
    ),
    scores = list(
      item("point", "Point color = Subject ID"),
      item("day", "Hover shows FPCA scores and phenotype group")
    ),
    phenotypes = list(
      item("mean", "Line color = phenotype group"),
      item("band", "Band = profile variability within group")
    ),
    list(
      item("mean", "Mean smoothed glucose curve"),
      item("band", "Band = 25th-75th percentile day range"),
      item("day", "Individual day profiles")
    )
  )
  shiny::div(class = "functional-plot-guide", items)
}

functional_profiles_module_server <- function(id, analysis_data, settings, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$subject_filter <- shiny::renderUI({
      req_active_tab(active_tab, "functional_profiles")
      data <- analysis_data()
      ids <- subject_id_values(data)
      mode <- input$visual_mode %||% "profile"
      if (mode %in% c("scores", "phenotypes")) {
        selected <- functional_comparison_subject_selection(ids, input$comparison_subjects, default_count = 2L)
        return(shiny::selectizeInput(
          session$ns("comparison_subjects"),
          "Comparison subjects",
          choices = ids,
          selected = selected,
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = "Choose at least two subjects"
          )
        ))
      }
      selected <- functional_profile_subject_selection(ids, input$subject)
      shiny::selectInput(
        session$ns("subject"),
        "Subject ID",
        choices = ids,
        selected = selected
      )
    })

    parameters <- shiny::reactive({
      functional_default_parameters(
        grid_minutes = input$grid_minutes %||% 5L,
        basis_type = input$basis_type %||% "bspline",
        basis_count = input$basis_count %||% 15L,
        smoothing_lambda = input$smoothing_lambda %||% 0.01,
        min_observed_points = input$min_observed_points %||% 24L,
        cluster_count = input$cluster_count %||% 3L
      )
    })

    selected_subjects <- shiny::reactive({
      req_active_tab(active_tab, "functional_profiles")
      ids <- subject_id_values(analysis_data())
      mode <- input$visual_mode %||% "profile"
      if (mode %in% c("scores", "phenotypes")) {
        functional_comparison_subject_selection(ids, input$comparison_subjects, default_count = 2L)
      } else {
        functional_profile_subject_selection(ids, input$subject)
      }
    })

    filtered_data <- shiny::reactive({
      req_active_tab(active_tab, "functional_profiles")
      filter_functional_data_by_subjects(analysis_data(), selected_subjects())
    })

    functional_bundle <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "functional_profiles")
      data <- filtered_data()
      cgm_with_progress(
        "Preparing glycemic patterns",
        detail = "Smoothing glucose curves...",
        value = 0.1,
        {
          bundle <- cgm_timed(
            "functional_profiles_bundle",
            compute_functional_profile_bundle(
              data,
              parameters = parameters(),
              thresholds = settings()$thresholds_mg_dl
            ),
            context = list(subjects = paste(selected_subjects(), collapse = ","))
          )
          shiny::incProgress(0.7, detail = "Preparing visual summaries...")
          bundle
        }
      )
    }),
    cgm_data_signature(filtered_data()),
    threshold_signature(settings()$thresholds_mg_dl),
    parameters(),
    cache = "session"
    )

    output$summary_cards <- shiny::renderUI({
      summary_card_ui(functional_summary_cards(functional_bundle()), compact = TRUE)
    })

    output$engine_status <- shiny::renderUI({
      functional_engine_status_ui(functional_bundle())
    })

    output$smoothing_warning <- shiny::renderUI({
      warning <- functional_smoothing_warning_text(functional_bundle())
      functional_status_note_ui(warning, level = "warning")
    })

    output$plot_guide <- shiny::renderUI({
      mode <- input$visual_mode %||% "profile"
      bundle <- functional_bundle()
      if (!functional_visual_available(bundle, mode)) {
        return(NULL)
      }
      functional_plot_guide_ui(mode)
    })

    output$functional_visual_ui <- shiny::renderUI({
      bundle <- functional_bundle()
      mode <- input$visual_mode %||% "profile"
      if (!functional_visual_available(bundle, mode)) {
        return(functional_status_note_ui(functional_unavailable_visual_message(bundle, mode), level = "info"))
      }
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(session$ns("functional_plot"), height = "480px"),
        type = 4
      )
    })

    output$functional_plot <- plotly::renderPlotly({
      bundle <- functional_bundle()
      mode <- input$visual_mode %||% "profile"
      shiny::req(functional_visual_available(bundle, mode))
      plot <- switch(
        mode,
        rate = create_functional_rate_plot(bundle$subject_curves, subject = selected_subjects()[[1L]] %||% ""),
        scores = create_functional_score_plot(bundle$features),
        phenotypes = create_functional_phenotype_plot(bundle$subject_curves),
        create_functional_profile_plot(bundle$subject_curves, bundle$day_curves, subject = selected_subjects()[[1L]] %||% "")
      )
      clean_functional_plotly_legend(plotly::ggplotly(plot, tooltip = "text"))
    })

    output$features_table <- DT::renderDT({
      display <- prepare_functional_key_features_display(
        functional_bundle()$features,
        data = filtered_data(),
        show_subject_id = TRUE
      )
      DT::datatable(display, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    })

    output$download_features <- shiny::downloadHandler(
      filename = glycemic_pattern_features_filename,
      content = function(file) {
        out <- prepare_functional_features_export(functional_bundle(), data = filtered_data())
        data.table::fwrite(out, file)
      }
    )

    output$download_curves <- shiny::downloadHandler(
      filename = glycemic_pattern_curves_filename,
      content = function(file) {
        out <- prepare_functional_curves_export(functional_bundle())
        data.table::fwrite(out, file)
      }
    )

    functional_bundle
  })
}
