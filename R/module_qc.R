quality_section_header_ui <- function(title, controls = NULL) {
  shiny::div(
    class = "cgm-quality-section-header",
    shiny::h4(title),
    if (!is.null(controls)) {
      shiny::div(class = "cgm-quality-section-controls", controls)
    }
  )
}

quality_section_ui <- function(title, body, controls = NULL, class = NULL) {
  shiny::tags$section(
    class = paste(
      "cgm-quality-section",
      class %||% ""
    ),
    quality_section_header_ui(title, controls = controls),
    shiny::div(class = "cgm-quality-section-body", body)
  )
}

quality_imputation_warning_list <- function(message) {
  items <- quality_imputation_warning_items(message)
  if (!nrow(items)) {
    return(character())
  }
  items$Text
}

quality_raw_imputation_warnings <- function(message) {
  message <- as.character(message %||% "")
  message <- message[nzchar(trimws(message))]
  if (!length(message)) {
    return(character())
  }
  message <- paste(message, collapse = " | ")
  if (grepl("Warnings:", message, fixed = TRUE)) {
    message <- sub("^.*Warnings:\\s*", "", message)
  }
  items <- unlist(strsplit(message, "\\s*\\|\\s*"))
  items <- unlist(lapply(items, function(item) {
    if (grepl("Subject [^[:space:]]+ has a contiguous missing block", item)) {
      item <- sub("^.*?(Subject [^[:space:]]+ has a contiguous missing block)", "\\1", item)
      item <- gsub(
        "\\.\\s+(Subject [^[:space:]]+ has a contiguous missing block)",
        ".|||\\1",
        item,
        perl = TRUE
      )
    }
    strsplit(item, "|||", fixed = TRUE)[[1L]]
  }), use.names = FALSE)
  items <- trimws(items)
  items <- items[!grepl("^Long contiguous missing glucose blocks were detected", items, ignore.case = TRUE)]
  items <- items[!grepl("^Number of logged events:", items, ignore.case = TRUE)]
  items <- items[!grepl("^Logged events:", items, ignore.case = TRUE)]
  unique(items[nzchar(items)])
}

quality_warning_subject_key <- function(subject) {
  subject <- trimws(as.character(subject %||% NA_character_))
  if (is.na(subject) || !nzchar(subject)) {
    return(NA_character_)
  }
  subject <- sub("^Subject\\s+", "", subject, ignore.case = TRUE)
  subject <- sub("\\s*[:;,.]+$", "", subject)
  subject <- gsub("\\s+", " ", subject)
  if (!nzchar(subject)) {
    NA_character_
  } else {
    subject
  }
}

quality_warning_group_label <- function(subject_key) {
  if (is.na(subject_key) || !nzchar(subject_key)) {
    "Dataset warnings"
  } else {
    paste("Subject", subject_key)
  }
}

quality_warning_type_order <- function(type) {
  match(type, c("missingness", "block", "other"), nomatch = 99L)
}

quality_imputation_warning_items <- function(message) {
  warnings <- quality_raw_imputation_warnings(message)
  empty <- data.frame(
    GroupKey = character(),
    Group = character(),
    Subject = character(),
    Type = character(),
    Title = character(),
    Detail = character(),
    Text = character(),
    stringsAsFactors = FALSE
  )
  if (!length(warnings)) {
    return(empty)
  }

  rows <- lapply(warnings, function(warning) {
    text <- trimws(warning)
    subject <- NA_character_
    body <- text
    if (grepl("^Subject .+? has ", text)) {
      subject <- sub("^Subject (.*?) has .*$", "\\1", text, perl = TRUE)
      body <- trimws(sub("^Subject .*? has\\s*", "", text, perl = TRUE))
    } else if (grepl("^Subject .+?:", text)) {
      subject <- sub("^Subject ([^:]+):.*$", "\\1", text)
      body <- trimws(sub("^Subject .+?:\\s*", "", text))
    }
    subject_key <- quality_warning_subject_key(subject)

    is_missingness <- grepl("^High missingness", body, ignore.case = TRUE)
    is_block <- grepl("contiguous missing block", body, ignore.case = TRUE)
    title <- if (is_missingness) {
      "High missingness"
    } else if (is_block && grepl("full day", body, ignore.case = TRUE)) {
      "Contiguous full-day block"
    } else if (is_block) {
      "Contiguous missing block"
    } else {
      "Review warning"
    }

    detail <- body
    if (is_block) {
      detail <- sub("^a contiguous missing block\\s*", "", detail, ignore.case = TRUE)
      detail <- sub("^of\\s*", "", detail, ignore.case = TRUE)
    }
    data.frame(
      GroupKey = if (is.na(subject_key)) "dataset" else paste0("subject:", subject_key),
      Group = quality_warning_group_label(subject_key),
      Subject = subject_key,
      Type = if (is_missingness) "missingness" else if (is_block) "block" else "other",
      Title = title,
      Detail = detail,
      Text = text,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(match(out$GroupKey, unique(out$GroupKey)), quality_warning_type_order(out$Type)), , drop = FALSE]
  row.names(out) <- NULL
  out
}

quality_imputation_warning_groups_ui <- function(
  warnings,
  show_all = FALSE,
  toggle_id = NULL,
  max_groups = 4L,
  max_items_per_group = 2L
) {
  if (!is.data.frame(warnings) || !nrow(warnings)) {
    return(NULL)
  }
  if (!"GroupKey" %in% names(warnings)) {
    warnings$GroupKey <- warnings$Group
  }
  group_keys <- unique(warnings$GroupKey)
  visible_group_keys <- if (isTRUE(show_all)) {
    group_keys
  } else {
    utils::head(group_keys, max_groups)
  }
  hidden_count <- 0L

  group_ui <- lapply(visible_group_keys, function(group_key) {
    group_rows <- warnings[warnings$GroupKey == group_key, , drop = FALSE]
    group_name <- group_rows$Group[[1L]]
    visible_rows <- if (isTRUE(show_all)) {
      group_rows
    } else {
      utils::head(group_rows, max_items_per_group)
    }
    hidden_count <<- hidden_count + max(nrow(group_rows) - nrow(visible_rows), 0L)
    type_names <- unique(visible_rows$Type)
    shiny::div(
      class = "cgm-quality-warning-group",
      shiny::div(
        class = "cgm-quality-warning-group-header",
        shiny::strong(group_name),
        shiny::span(paste(format_count(nrow(group_rows)), "warning(s)"))
      ),
      lapply(type_names, function(type_name) {
        type_rows <- visible_rows[visible_rows$Type == type_name, , drop = FALSE]
        shiny::div(
          class = paste("cgm-quality-warning-type", paste0("cgm-quality-warning-type-", type_name)),
          shiny::tags$ul(lapply(seq_len(nrow(type_rows)), function(i) {
            shiny::tags$li(
              shiny::strong(type_rows$Title[[i]]),
              shiny::span(type_rows$Detail[[i]])
            )
          }))
        )
      })
    )
  })

  if (!isTRUE(show_all) && length(group_keys) > length(visible_group_keys)) {
    hidden_groups <- setdiff(group_keys, visible_group_keys)
    hidden_count <- hidden_count + sum(warnings$GroupKey %in% hidden_groups)
  }

  shiny::div(
    class = "cgm-quality-warning-list",
    shiny::strong("Warnings"),
    group_ui,
    if (!is.null(toggle_id) && (hidden_count > 0L || isTRUE(show_all))) {
      shiny::actionLink(
        toggle_id,
        if (isTRUE(show_all)) {
          "Show fewer warnings"
        } else {
          paste("Show all warnings", paste0("(", format_count(hidden_count), " more)"))
        },
        class = "cgm-quality-warning-toggle"
      )
    }
  )
}

quality_imputation_message <- function(status) {
  message <- if (is.data.frame(status) && "Message" %in% names(status)) {
    as.character(status$Message[[1L]] %||% "")
  } else {
    ""
  }
  if (!nzchar(message)) {
    return("")
  }
  trimws(sub("\\s+Warnings:.*$", "", message))
}

quality_imputation_panel_class <- function(status) {
  status <- status %||% ""
  severity <- switch(
    status,
    "Applied" = "success",
    "Not needed" = "success",
    "Unavailable" = "warning",
    "Could not apply" = "warning",
    "No rows filled" = "warning",
    "Stale" = "warning",
    "info"
  )
  paste("cgm-quality-imputation-panel", paste0("cgm-quality-imputation-", severity))
}

quality_percent_value <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1L]])) {
    return("Not available")
  }
  paste0(format(round(value[[1L]], 2), trim = TRUE), "%")
}

quality_preprocessing_summary_cards <- function(status) {
  if (!is.data.frame(status) || !nrow(status)) {
    return(data.frame(Label = character(), Value = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    Label = c(
      "Original missing",
      "Original (%)",
      "After preprocessing",
      "After (%)",
      "Filled rows",
      "Gap readings"
    ),
    Value = c(
      format_count(status[["Original missing glucose"]][[1L]]),
      quality_percent_value(status[["Original missing glucose (%)"]]),
      format_count(status[["Analysis missing glucose"]][[1L]]),
      quality_percent_value(status[["Analysis missing glucose (%)"]]),
      format_count(status[["Filled glucose rows"]][[1L]]),
      format_count(status[["Estimated missing readings from gaps"]][[1L]])
    ),
    stringsAsFactors = FALSE
  )
}

quality_imputation_status_ui <- function(status) {
  quality_imputation_status_ui_impl(status)
}

quality_imputation_status_ui_impl <- function(status, show_all_warnings = FALSE, warning_toggle_id = NULL) {
  if (!is.data.frame(status) || !nrow(status)) {
    return(NULL)
  }
  warning_text <- if ("Warnings" %in% names(status)) status$Warnings[[1L]] else ""
  message_text <- if ("Message" %in% names(status)) status$Message[[1L]] else ""
  warnings <- quality_imputation_warning_items(warning_text %||% "")
  if (!nrow(warnings)) {
    warnings <- quality_imputation_warning_items(message_text %||% "")
  }

  shiny::div(
    class = quality_imputation_panel_class(status$Status[[1L]]),
    shiny::div(
      class = "cgm-quality-imputation-header",
      shiny::div(
        shiny::h4("Imputation status"),
        shiny::p(class = "cgm-quality-imputation-message", quality_imputation_message(status))
      ),
      shiny::span(class = "cgm-quality-status-badge", status$Status[[1L]])
    ),
    shiny::div(
      class = "cgm-quality-kv-grid",
      shiny::div(shiny::span("Method"), shiny::strong(status$Method[[1L]])),
      shiny::div(shiny::span("Model"), shiny::strong(status$Model[[1L]] %||% "Not set")),
      shiny::div(shiny::span("Rows filled"), shiny::strong(format_count(status[["Filled glucose rows"]][[1L]]))),
      if ("Method details" %in% names(status) && nzchar(status[["Method details"]][[1L]] %||% "")) {
        shiny::div(
          class = "cgm-quality-kv-wide",
          shiny::span("Subject-level methods"),
          shiny::strong(status[["Method details"]][[1L]])
        )
      }
    ),
    if (nrow(warnings)) {
      quality_imputation_warning_groups_ui(
        warnings,
        show_all = show_all_warnings,
        toggle_id = warning_toggle_id
      )
    },
    if (nrow(quality_preprocessing_summary_cards(status))) {
      shiny::tagList(
        shiny::tags$hr(),
        shiny::h5("Before vs after preprocessing"),
        summary_card_ui(quality_preprocessing_summary_cards(status), compact = TRUE),
        shiny::tags$p(
          class = "help-block cgm-quality-footnote",
          "Inferred timestamp-gap readings are included in the imputation missing-rate review. ",
          "When imputation is applied, generated gap rows are included in analysis data and exports."
        )
      )
    }
  )
}

quality_qc_review_summary <- function(qc_summary, analysis_data) {
  display <- prepare_qc_display(qc_summary, analysis_data, show_subject_id = TRUE)
  status <- if ("QC status" %in% names(display)) display[["QC status"]] else character()
  review_rows <- if (length(status)) {
    display[status %in% "Review", intersect(c("Subject ID", "QC status", "Review notes"), names(display)), drop = FALSE]
  } else {
    data.frame()
  }
  raw_sum <- function(column) {
    if (is.data.frame(qc_summary) && column %in% names(qc_summary)) {
      sum(qc_summary[[column]], na.rm = TRUE)
    } else {
      0L
    }
  }
  list(
    cards = data.frame(
      Label = c(
        "Subjects OK",
        "Needs review",
        "Duplicate timestamps",
        "Implausible values",
        "Timestamp gaps"
      ),
      Value = c(
        format_count(sum(status == "OK", na.rm = TRUE)),
        format_count(sum(status == "Review", na.rm = TRUE)),
        format_count(raw_sum("duplicate_timestamps")),
        format_count(raw_sum("implausible_values")),
        format_count(raw_sum("gap_count"))
      ),
      stringsAsFactors = FALSE
    ),
    review_rows = review_rows
  )
}

quality_qc_review_ui <- function(summary) {
  if (!is.list(summary)) {
    return(NULL)
  }
  rows <- summary$review_rows
  visible_rows <- if (is.data.frame(rows) && nrow(rows)) {
    utils::head(rows, 8L)
  } else {
    data.frame()
  }
  hidden_count <- if (is.data.frame(rows)) max(nrow(rows) - nrow(visible_rows), 0L) else 0L

  shiny::div(
    class = "cgm-quality-qc-review",
    summary_card_ui(summary$cards, compact = TRUE),
    if (!is.data.frame(rows) || !nrow(rows)) {
      shiny::div(
        class = "cgm-quality-compact-note cgm-quality-note-success",
        shiny::strong("No QC review flags"),
        shiny::span("All subjects meet the current QC review checks.")
      )
    } else {
      shiny::tagList(
        shiny::div(
          class = "table-responsive cgm-quality-review-table-wrap",
          shiny::tags$table(
            class = "table table-condensed cgm-quality-review-table",
            shiny::tags$thead(
              shiny::tags$tr(lapply(names(visible_rows), shiny::tags$th))
            ),
            shiny::tags$tbody(
              lapply(seq_len(nrow(visible_rows)), function(i) {
                shiny::tags$tr(lapply(visible_rows[i, , drop = FALSE], function(value) {
                  shiny::tags$td(as.character(value[[1L]]))
                }))
              })
            )
          )
        ),
        if (hidden_count > 0L) {
          shiny::div(
            class = "cgm-quality-review-more",
            paste(format_count(hidden_count), "additional subject(s) need review.")
          )
        }
      )
    }
  )
}

qc_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "cgm-quality-dashboard",
    shiny::div(
      class = "cgm-quality-overview",
      shiny::h3("Quality control"),
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("qc_summary_cards")),
        type = 4
      )
    ),
    shinycssloaders::withSpinner(
      shiny::uiOutput(ns("imputation_status")),
      type = 4
    ),
    quality_section_ui(
      "Analysis data QC",
      shiny::tagList(
        shiny::uiOutput(ns("duplicate_timestamp_note")),
        shinycssloaders::withSpinner(
          shiny::uiOutput(ns("qc_review_ui")),
          type = 4
        )
      ),
      class = "cgm-quality-section-wide cgm-quality-qc-section"
    ),
    quality_section_ui(
      "Daily data coverage",
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("missingness_heatmap_ui")),
        type = 4
      ),
      controls = shiny::uiOutput(ns("missingness_subject_filter")),
      class = "cgm-quality-section-wide cgm-quality-calendar-section"
    )
  )
}

qc_module_server <- function(
  id,
  standardized,
  analysis_data,
  settings,
  active_tab = NULL,
  standardized_missingness_precompute = NULL,
  analysis_missingness_precompute = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    show_all_imputation_warnings <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$toggle_imputation_warnings, {
      show_all_imputation_warnings(!isTRUE(show_all_imputation_warnings()))
    }, ignoreInit = TRUE)

    qc_summary <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, c("quality", "export"))
      cgm_with_progress(
        "Preparing Quality summary",
        detail = "Calculating QC checks...",
        value = 0.2,
        cgm_timed(
          "quality_qc_summary",
          compute_qc_summary(
            analysis_data(),
            valid_day_hours = settings()$valid_day_hours
          )
        )
      )
    }),
    cgm_data_signature(analysis_data()),
    settings()$valid_day_hours,
    cache = "session"
    )

    standardized_missingness <- if (is.function(standardized_missingness_precompute)) {
      standardized_missingness_precompute
    } else {
      shiny::bindCache(shiny::reactive({
        req_active_tab(active_tab, "quality")
        data <- standardized()
        result <- cgm_with_progress(
          "Preparing Quality baseline review",
          detail = "Scanning original standardized data...",
          value = 0.2,
          {
            cgm_timed(
              "quality_standardized_missingness_precompute",
              cgm_suppress_non_cgma_messages(
                missingness_precompute(
                  data,
                  interval_minutes = settings()$imputation_interval_minutes %||% 5L
                )
              )
            )
          }
        )
        cgm_maybe_gc(nrow(data))
        result
      }),
      cgm_data_signature(standardized()),
      settings()$imputation_interval_minutes,
      cache = "session"
      )
    }

    analysis_missingness <- if (is.function(analysis_missingness_precompute)) {
      analysis_missingness_precompute
    } else {
      shiny::bindCache(shiny::reactive({
        req_active_tab(active_tab, "quality")
        data <- analysis_data()
        result <- cgm_with_progress(
          "Preparing Quality missingness review",
          detail = "Scanning analysis data for timestamp gaps...",
          value = 0.2,
          {
            cgm_timed(
              "quality_analysis_missingness_precompute",
              cgm_suppress_non_cgma_messages(
                missingness_precompute(
                  data,
                  interval_minutes = settings()$imputation_interval_minutes %||% 5L
                )
              )
            )
          }
        )
        cgm_maybe_gc(nrow(data))
        result
      }),
      cgm_data_signature(analysis_data()),
      settings()$imputation_interval_minutes,
      cache = "session"
      )
    }

    missingness_comparison <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      cgm_with_progress(
        "Preparing Missingness Summary",
        detail = "Comparing original and analysis data...",
        value = 0.2,
        cgm_timed(
          "quality_missingness_comparison",
          compare_missingness_summaries(
            standardized(),
            analysis_data(),
            valid_day_hours = settings()$valid_day_hours,
            include_preprocessing = should_show_analysis_missingness(settings()),
            interval_minutes = settings()$imputation_interval_minutes %||% 5L,
            original_precomputed = standardized_missingness(),
            analysis_precomputed = analysis_missingness()
          )
        )
      )
    }),
    cgm_data_signature(standardized()),
    cgm_data_signature(analysis_data()),
    settings()$valid_day_hours,
    settings()$imputation_interval_minutes,
    should_show_analysis_missingness(settings()),
    cache = "session"
    )

    study_window <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      cgm_with_progress(
        "Preparing study window",
        detail = "Summarizing observed coverage...",
        value = 0.2,
        cgm_timed(
          "quality_study_window",
          study_window_summary(
            analysis_data(),
            expected_duration_days = settings()$expected_study_duration_days
          )
        )
      )
    }),
    cgm_data_signature(analysis_data()),
    expected_study_duration_signature(settings()),
    cache = "session"
    )

    imputation_status <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      cgm_with_progress(
        "Preparing imputation status",
        detail = "Summarizing preprocessing effects...",
        value = 0.2,
        cgm_timed(
          "quality_imputation_status",
          summarize_imputation_status(
            standardized(),
            analysis_data(),
            settings(),
            original_precomputed = standardized_missingness(),
            analysis_precomputed = analysis_missingness()
          )
        )
      )
    }),
    cgm_data_signature(standardized()),
    cgm_data_signature(analysis_data()),
    imputation_settings_signature(settings()),
    cache = "session"
    )

    gap_periods <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      cgm_with_progress(
        "Preparing gap periods",
        detail = "Finding timestamp-gap intervals...",
        value = 0.2,
        cgm_timed(
          "quality_gap_periods",
          detect_gap_periods(
            analysis_data(),
            interval_minutes = settings()$imputation_interval_minutes %||% 5L,
            precomputed = analysis_missingness()
          )
        )
      )
    }),
    cgm_data_signature(analysis_data()),
    settings()$imputation_interval_minutes,
    cache = "session"
    )

    missingness_calendar_all <- shiny::bindCache(shiny::reactive({
      req_active_tab(active_tab, "quality")
      cgm_with_progress(
        "Preparing daily coverage",
        detail = "Building participant-day coverage calendar...",
        value = 0.2,
        cgm_timed(
          "quality_daily_coverage_calendar_data",
          compute_missingness_calendar_data(
            analysis_data(),
            gaps = gap_periods(),
            date_range = settings()$analysis_date_range,
            interval_minutes = settings()$imputation_interval_minutes %||% 5L,
            precomputed = analysis_missingness()
          )
        )
      )
    }),
    cgm_data_signature(analysis_data()),
    analysis_date_range_signature(settings()),
    settings()$imputation_interval_minutes,
    cache = "session"
    )

    missingness_calendar_data <- shiny::reactive({
      req_active_tab(active_tab, "quality")
      cgm_timed(
        "quality_daily_coverage_filter",
        filter_missingness_calendar_participant(
          missingness_calendar_all(),
          participant = input$missingness_participant %||% ""
        )
      )
    })

    force_missingness_subject_id_display <- shiny::reactive({
      specific_filter_selected(input$missingness_participant)
    })

    day_coverage_summary <- shiny::reactive({
      day_coverage_warning_summary(
        missingness_calendar_data(),
        data = analysis_data(),
        show_subject_id = force_missingness_subject_id_display()
      )
    })

    missingness_comparison_display <- shiny::reactive({
      append_day_coverage_warnings(
        missingness_comparison(),
        missingness_calendar_data(),
        data = analysis_data()
      )
    })

    output$missingness_subject_filter <- shiny::renderUI({
      req_active_tab(active_tab, "quality")
      data <- analysis_data()
      if (!subject_id_filter_available(data)) {
        return(NULL)
      }
      ids <- sort(subject_id_values(data))
      choices <- subject_filter_choices(ids, all_label = "All")
      shiny::selectInput(
        session$ns("missingness_participant"),
        "Subject ID",
        choices = choices,
        selected = preserve_subject_filter_selection(
          input$missingness_participant,
          choices,
          ids
        )
      )
    })

    output$qc_table <- DT::renderDT({
      display <- cgm_timed("quality_qc_table_prepare", prepare_qc_display(qc_summary(), analysis_data()))
      cgm_timed(
        "quality_qc_table_dt_render",
        DT::datatable(display, options = list(scrollX = TRUE, pageLength = 10)),
        rows = nrow(display)
      )
    }, server = TRUE)

    output$qc_review_ui <- shiny::renderUI({
      quality_qc_review_ui(
        quality_qc_review_summary(qc_summary(), analysis_data())
      )
    })

    output$qc_summary_cards <- shiny::renderUI({
      summary_card_ui(
        quality_summary_cards(
          analysis_data(),
          qc_summary(),
          missingness_comparison()
        ),
        compact = TRUE
      )
    })

    output$study_window_table <- DT::renderDT({
      table <- study_window()
      cgm_timed(
        "quality_study_window_dt_render",
        DT::datatable(table, rownames = FALSE, options = list(scrollX = FALSE, pageLength = 10)),
        rows = nrow(table)
      )
    }, server = TRUE)

    output$duplicate_timestamp_note <- shiny::renderUI({
      note <- duplicate_timestamp_note(qc_summary(), analysis_data())
      if (is.null(note)) {
        return(NULL)
      }
      shiny::div(
        class = "cgm-quality-compact-note cgm-quality-note-warning",
        shiny::strong("Duplicate timestamps need review"),
        shiny::span(note$message)
      )
    })

    output$imputation_status <- shiny::renderUI({
      if (!should_show_analysis_missingness(settings())) {
        return(NULL)
      }
      quality_imputation_status_ui_impl(
        imputation_status(),
        show_all_warnings = show_all_imputation_warnings(),
        warning_toggle_id = session$ns("toggle_imputation_warnings")
      )
    })

    output$missingness_comparison_table <- DT::renderDT({
      display <- missingness_comparison_display()
      cgm_timed(
        "quality_missingness_table_dt_render",
        DT::datatable(display, rownames = FALSE, options = list(scrollX = FALSE, pageLength = 10)),
        rows = nrow(display)
      )
    }, server = TRUE)

    output$day_coverage_warning_note <- shiny::renderUI({
      note <- day_coverage_warning_note(day_coverage_summary())
      if (!nzchar(note)) {
        return(NULL)
      }
      shiny::div(
        class = "alert alert-warning",
        note
      )
    })

    output$missingness_heatmap_ui <- shiny::renderUI({
      req_active_tab(active_tab, "quality")
      dimensions <- missingness_calendar_dimensions(
        missingness_calendar_data(),
        show_subject_id = show_subject_id_for_display(
          analysis_data(),
          subject_id_filter_available(analysis_data()) || force_missingness_subject_id_display()
        )
      )
      plotly::plotlyOutput(
        session$ns("missingness_heatmap"),
        height = paste0(dimensions$height, "px")
      )
    })

    output$missingness_heatmap <- plotly::renderPlotly({
      cgm_timed(
        "quality_daily_coverage_plotly_render",
        create_missingness_heatmap_plot(
          analysis_data(),
          calendar_data = missingness_calendar_data(),
          show_subject_id = subject_id_filter_available(analysis_data()) || force_missingness_subject_id_display()
        )
      )
    })

    qc_summary
  })
}
