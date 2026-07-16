predictive_risk_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Predictive Risk"),
    shiny::tags$style(
      "
      .predictive-controls-panel {
        display: grid;
        grid-template-columns: minmax(260px, 0.9fr) minmax(420px, 1.4fr) minmax(240px, 0.8fr);
        gap: 16px;
        align-items: start;
        margin: 12px 0 18px;
      }
      .predictive-control-group {
        border: 1px solid #e5e7eb;
        border-radius: 8px;
        padding: 12px 14px 6px;
        background: #fff;
      }
      .predictive-control-group h4 {
        margin: 0 0 10px;
        font-size: 15px;
        font-weight: 600;
      }
      .predictive-control-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(140px, 1fr));
        gap: 10px 14px;
      }
      .predictive-control-grid .form-group {
        margin-bottom: 8px;
      }
      .predictive-control-help {
        margin: -4px 0 8px;
        color: #5f6b7a;
        font-size: 12px;
        line-height: 1.35;
      }
      .predictive-status-note {
        margin: 10px 0 14px;
        padding: 8px 12px;
        border-radius: 6px;
        font-size: 13px;
      }
      .predictive-plot-guide {
        display: flex;
        flex-wrap: wrap;
        gap: 10px 16px;
        align-items: center;
        margin: 0 0 8px;
        color: #374151;
        font-size: 13px;
      }
      .predictive-plot-guide-item {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        min-height: 20px;
      }
      .predictive-plot-swatch {
        width: 18px;
        height: 10px;
        border-radius: 2px;
        border: 1px solid transparent;
        display: inline-block;
      }
      .predictive-plot-swatch.low {
        background: rgba(59, 130, 160, 0.18);
        border-color: #3B82A0;
      }
      .predictive-plot-swatch.elevated {
        background: rgba(217, 119, 6, 0.18);
        border-color: #D97706;
      }
      .predictive-plot-swatch.high {
        background: rgba(220, 38, 38, 0.18);
        border-color: #DC2626;
      }
      .predictive-plot-swatch.threshold {
        height: 3px;
        background: repeating-linear-gradient(
          90deg,
          #374151,
          #374151 6px,
          transparent 6px,
          transparent 10px
        );
        border: 0;
      }
      .predictive-plot-swatch.event {
        width: 0;
        height: 0;
        border-left: 7px solid transparent;
        border-right: 7px solid transparent;
        border-bottom: 12px solid #B91C1C;
        border-radius: 0;
        background: transparent;
      }
      .predictive-output-grid {
        display: grid;
        grid-template-columns: minmax(0, 1.35fr) minmax(300px, 0.75fr);
        gap: 16px;
        align-items: start;
      }
      @media (max-width: 1100px) {
        .predictive-controls-panel,
        .predictive-output-grid {
          grid-template-columns: 1fr;
        }
      }
      @media (max-width: 650px) {
        .predictive-control-grid {
          grid-template-columns: 1fr;
        }
      }
      "
    ),
    shiny::div(
      class = "predictive-controls-panel",
      shiny::div(
        class = "predictive-control-group",
        shiny::h4("Selection"),
        shiny::div(
          class = "predictive-control-grid",
          shiny::uiOutput(ns("subject_filter")),
          shiny::selectInput(ns("target"), "Risk target", choices = predictive_target_choices(), selected = "low")
        )
      ),
      shiny::div(
        class = "predictive-control-group",
        shiny::h4("Model"),
        shiny::div(
          class = "predictive-control-grid",
          shiny::selectInput(ns("horizon_minutes"), "Future window", choices = predictive_horizon_choices(), selected = 30),
          shiny::selectInput(ns("model"), "Model", choices = predictive_model_choices(), selected = "glm"),
          shiny::div(
            shiny::numericInput(ns("min_examples"), "Minimum examples", value = 80, min = 20, step = 10),
            shiny::div(
              class = "predictive-control-help",
              "Minimum usable timestamp rows after feature engineering and future-window labeling."
            )
          ),
          shiny::div(
            shiny::numericInput(ns("min_events"), "Minimum events", value = 5, min = 1, step = 1),
            shiny::div(
              class = "predictive-control-help",
              "Minimum actual target events and non-events required in training data."
            )
          )
        )
      ),
      shiny::div(
        class = "predictive-control-group",
        shiny::h4("Display"),
        shiny::div(
          class = "predictive-control-grid",
          shiny::div(
            shiny::numericInput(ns("threshold"), "Risk threshold", value = 0.5, min = 0.05, max = 0.95, step = 0.05),
            shiny::div(
              class = "predictive-control-help",
              "Probability cutoff used for sensitivity and specificity, not the glucose range threshold."
            )
          )
        )
      )
    ),
    shinycssloaders::withSpinner(shiny::uiOutput(ns("summary_cards")), type = 4),
    shiny::uiOutput(ns("status_note")),
    shiny::div(
      class = "predictive-output-grid",
      shiny::div(
        shiny::uiOutput(ns("risk_plot_guide")),
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("risk_plot"), height = "430px"), type = 4)
      ),
      shiny::div(
        shiny::h4("Feature importance"),
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("importance_plot"), height = "360px"), type = 4)
      )
    ),
    shiny::h4("Model performance"),
    shinycssloaders::withSpinner(DT::DTOutput(ns("performance_table")), type = 4)
  )
}

predictive_status_note_ui <- function(message, level = "info") {
  if (!nzchar(message %||% "")) {
    return(NULL)
  }
  shiny::div(
    class = paste("alert", paste0("alert-", level), "predictive-status-note"),
    message
  )
}

predictive_risk_plot_guide_ui <- function(event_label = "Observed future target event") {
  item <- function(class, label) {
    shiny::span(
      class = "predictive-plot-guide-item",
      shiny::span(class = paste("predictive-plot-swatch", class)),
      label
    )
  }
  shiny::div(
    class = "predictive-plot-guide",
    item("low", "Low risk"),
    item("elevated", "Elevated risk"),
    item("high", "High risk"),
    item("threshold", "Risk threshold"),
    item("event", event_label)
  )
}

predictive_risk_module_server <- function(id, analysis_data, settings, active_tab = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    output$subject_filter <- shiny::renderUI({
      req_active_tab(active_tab, "predictive_risk")
      data <- analysis_data()
      ids <- subject_id_values(data)
      choices <- predictive_subject_choices(ids)
      shiny::selectInput(
        session$ns("subject"),
        "Subject ID",
        choices = choices,
        selected = preserve_predictive_subject_selection(input$subject, ids)
      )
    })

    shiny::observeEvent(settings()$thresholds_mg_dl, {
      selected <- input$target %||% "low"
      shiny::updateSelectInput(
        session,
        "target",
        choices = predictive_target_choices(settings()$thresholds_mg_dl),
        selected = selected
      )
    }, ignoreInit = FALSE)

    selected_subject <- shiny::reactive({
      ids <- subject_id_values(analysis_data())
      default <- predictive_default_subject_selection(ids)
      preserve_predictive_subject_selection(input$subject %||% default, ids)
    })

    selected_data <- shiny::reactive({
      req_active_tab(active_tab, "predictive_risk")
      filter_predictive_data_by_subject(analysis_data(), selected_subject())
    })

    parameters <- shiny::reactive({
      predictive_default_parameters(
        horizon_minutes = as.integer(input$horizon_minutes %||% 30L),
        target = input$target %||% "low",
        model = input$model %||% "glm",
        min_examples = input$min_examples %||% 80L,
        min_events = input$min_events %||% 5L,
        threshold = input$threshold %||% 0.5
      )
    })

    predictive_bundle <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "predictive_risk")
      data <- selected_data()
      cgm_with_progress(
        "Preparing Predictive Risk",
        detail = "Building leakage-safe examples...",
        value = 0.1,
        {
          bundle <- cgm_timed(
            "predictive_risk_bundle",
            compute_predictive_risk_bundle(
              data,
              parameters = parameters(),
              thresholds = settings()$thresholds_mg_dl
            ),
            context = list(
              subject = selected_subject(),
              target = parameters()$target,
              horizon = parameters()$horizon_minutes,
              model = parameters()$model
            )
          )
          shiny::incProgress(0.7, detail = "Preparing risk outputs...")
          bundle
        }
      )
    }),
    cgm_data_signature(selected_data()),
    selected_subject(),
    threshold_signature(settings()$thresholds_mg_dl),
    parameters(),
    cache = "session"
    )

    output$summary_cards <- shiny::renderUI({
      summary_card_ui(predictive_summary_cards(predictive_bundle()), compact = TRUE)
    })

    output$status_note <- shiny::renderUI({
      message <- predictive_status_ui_message(predictive_bundle())
      predictive_status_note_ui(message, level = "info")
    })

    output$risk_plot_guide <- shiny::renderUI({
      predictive_risk_plot_guide_ui(predictive_event_label(parameters()))
    })

    output$risk_plot <- plotly::renderPlotly({
      bundle <- predictive_bundle()
      plot <- create_predictive_risk_plot(
        bundle$scores,
        subject = selected_subject(),
        target_label = bundle$target_label,
        threshold = bundle$parameters$threshold,
        event_label = predictive_event_label(bundle$parameters),
        show_legend = FALSE
      )
      plotly::ggplotly(plot, tooltip = "text") |>
        plotly::layout(
          showlegend = FALSE,
          margin = list(l = 70, r = 20, b = 75, t = 70)
        )
    })

    output$importance_plot <- plotly::renderPlotly({
      plot <- create_predictive_importance_plot(predictive_bundle()$importance)
      plotly::ggplotly(plot, tooltip = "text")
    })

    output$performance_table <- DT::renderDT({
      display <- prepare_predictive_performance_display(predictive_bundle())
      DT::datatable(display, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    })

    predictive_bundle
  })
}
