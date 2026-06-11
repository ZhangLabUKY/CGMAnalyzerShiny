column_mapping_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Column Mapping"),
    shiny::uiOutput(ns("mapping_note")),
    shiny::div(
      class = "cgm-column-mapping-grid",
      shiny::div(
        class = "cgm-column-mapping-field cgm-column-mapping-field-subject",
        shiny::uiOutput(ns("id_mapping_ui"))
      ),
      shiny::div(
        class = "cgm-column-mapping-field cgm-column-mapping-field-timestamp",
        shiny::uiOutput(ns("timestamp_mapping_ui"))
      ),
      shiny::div(
        class = "cgm-column-mapping-field cgm-column-mapping-field-glucose",
        shiny::uiOutput(ns("glucose_mapping_ui"))
      )
    ),
    shiny::div(
      class = "cgm-column-mapping-units",
      shiny::radioButtons(
        ns("source_units"),
        "Source units",
        choices = c("mg/dL", "mmol/L"),
        selected = "mg/dL",
        inline = TRUE
      )
    ),
    shiny::div(
      class = "card mb-3",
      style = "padding: 12px 14px;",
      shiny::h4("Subject Metadata"),
      shiny::p(
        class = "text-muted",
        "Optional subject-level metadata can be used for grouping and imputation features."
      ),
      shiny::actionButton(ns("edit_metadata"), "Edit subject metadata", icon = shiny::icon("table")),
      shiny::uiOutput(ns("metadata_status"))
    ),
    shiny::uiOutput(ns("mapping_status"))
  )
}

guess_column <- function(columns, patterns) {
  hit <- columns[Reduce(`|`, lapply(patterns, function(pattern) grepl(pattern, columns, ignore.case = TRUE)))]
  if (length(hit)) hit[[1L]] else ""
}

subject_id_mapping_label <- function(upload) {
  if (is_multi_file_upload(upload)) "Subject ID" else "Subject ID (optional)"
}

subject_id_status_value <- function(upload, id_mapping = "") {
  if (is_multi_file_upload(upload) || !nzchar(id_mapping %||% "")) "File name" else id_mapping
}

metadata_default_columns <- function() {
  c("group", "sex", "age", "hba1c")
}

normalize_metadata_column_name <- function(name) {
  name <- tolower(trimws(as.character(name[[1L]] %||% "")))
  name <- gsub("[^a-z0-9]+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  if (!nzchar(name) || identical(name, "id")) {
    return(NA_character_)
  }
  name
}

subject_ids_from_upload <- function(upload, id_mapping = "") {
  data <- upload$data
  if (is.null(data) || !is.data.frame(data) || !nrow(data)) {
    return(character())
  }
  upload_mode <- upload$upload_mode %||% "single_file"
  if (identical(upload_mode, "multi_file") && ".source_id" %in% names(data)) {
    ids <- data$.source_id
  } else if (nzchar(id_mapping %||% "") && id_mapping %in% names(data)) {
    ids <- data[[id_mapping]]
  } else if (".source_id" %in% names(data)) {
    ids <- data$.source_id
  } else {
    ids <- "Subject 1"
  }
  ids <- unique(as.character(ids))
  ids[!is.na(ids) & nzchar(trimws(ids))]
}

guess_metadata_column <- function(columns, target) {
  patterns <- switch(
    target,
    group = c("^group$", "^arm$", "treatment", "cohort", "condition"),
    sex = c("^sex$", "gender"),
    age = c("^age$", "age_year", "years"),
    hba1c = c("^hba1c$", "hba1c", "a1c"),
    character()
  )
  guess_column(columns, patterns)
}

prefill_subject_metadata <- function(upload, id_mapping = "") {
  ids <- subject_ids_from_upload(upload, id_mapping)
  out <- data.frame(id = ids, stringsAsFactors = FALSE)
  for (col in metadata_default_columns()) {
    out[[col]] <- ""
  }
  data <- upload$data
  if (is.null(data) || !is.data.frame(data) || !nrow(data) || !length(ids)) {
    return(out)
  }
  id_col <- if (identical(upload$upload_mode %||% "single_file", "multi_file") && ".source_id" %in% names(data)) {
    ".source_id"
  } else if (nzchar(id_mapping %||% "") && id_mapping %in% names(data)) {
    id_mapping
  } else if (".source_id" %in% names(data)) {
    ".source_id"
  } else {
    NA_character_
  }
  if (is.na(id_col)) {
    return(out)
  }
  columns <- setdiff(names(data), c(".source_file", ".source_id"))
  for (target in metadata_default_columns()) {
    source_col <- guess_metadata_column(columns, target)
    if (!nzchar(source_col) || !source_col %in% names(data)) {
      next
    }
    values <- vapply(ids, function(id) {
      raw_values <- data[[source_col]][as.character(data[[id_col]]) == id]
      raw_values <- trimws(as.character(raw_values))
      raw_values <- unique(raw_values[!is.na(raw_values) & nzchar(raw_values)])
      if (length(raw_values) == 1L) raw_values[[1L]] else ""
    }, character(1))
    out[[target]] <- values
  }
  out
}

metadata_display_table <- function(metadata, upload, id_mapping = "") {
  ids <- subject_ids_from_upload(upload, id_mapping)
  if (!is.data.frame(metadata) || !nrow(metadata) || !identical(sort(metadata$id), sort(ids))) {
    return(prefill_subject_metadata(upload, id_mapping))
  }
  out <- as.data.frame(metadata, stringsAsFactors = FALSE)
  for (col in metadata_default_columns()) {
    if (!col %in% names(out)) {
      out[[col]] <- ""
    }
  }
  out <- out[match(ids, out$id), , drop = FALSE]
  row.names(out) <- NULL
  out
}

metadata_status_text <- function(metadata) {
  cleaned <- clean_subject_metadata(metadata)
  metadata_cols <- setdiff(names(cleaned), "id")
  if (!length(metadata_cols)) {
    return("No subject metadata has been added.")
  }
  paste("Metadata columns:", paste(metadata_cols, collapse = ", "))
}

column_mapping_module_server <- function(id, uploaded) {
  shiny::moduleServer(id, function(input, output, session) {
    subject_metadata <- shiny::reactiveVal(data.frame(id = character(), stringsAsFactors = FALSE))

    output$mapping_note <- shiny::renderUI({
      upload <- uploaded()
      if (is_multi_file_upload(upload)) {
        shiny::div(
          class = "alert alert-info",
          "Multiple files detected. Subject IDs will be taken from file names."
        )
      }
    })

    output$id_mapping_ui <- shiny::renderUI({
      upload <- uploaded()
      if (is_multi_file_upload(upload)) {
        shiny::tagList(
          shiny::tags$label(class = "control-label", subject_id_mapping_label(upload)),
          shiny::div(class = "form-control cgm-static-mapping-value", "File name")
        )
      } else {
        choices <- mapping_choices_for_upload(upload)
        shiny::selectInput(session$ns("id_col"), subject_id_mapping_label(upload), choices = choices$required_choices, selected = "")
      }
    })

    output$timestamp_mapping_ui <- shiny::renderUI({
      choices <- mapping_choices_for_upload(uploaded())
      shiny::selectInput(session$ns("timestamp_col"), "Timestamp", choices = choices$required_choices, selected = "")
    })

    output$glucose_mapping_ui <- shiny::renderUI({
      choices <- mapping_choices_for_upload(uploaded())
      shiny::selectInput(session$ns("glucose_col"), "Glucose", choices = choices$required_choices, selected = "")
    })

    shiny::observeEvent(list(uploaded(), input$id_col), {
      subject_metadata(metadata_display_table(subject_metadata(), uploaded(), input$id_col %||% ""))
    }, ignoreInit = FALSE)

    output$metadata_status <- shiny::renderUI({
      shiny::tags$p(
        style = "margin-top: 8px; margin-bottom: 0; color: #555;",
        metadata_status_text(subject_metadata())
      )
    })

    shiny::observeEvent(input$edit_metadata, {
      subject_metadata(metadata_display_table(subject_metadata(), uploaded(), input$id_col %||% ""))
      shiny::showModal(shiny::modalDialog(
        title = "Subject Metadata",
        size = "l",
        shiny::p("Edit optional metadata for each Subject ID. Blank columns are ignored."),
        DT::DTOutput(session$ns("metadata_table")),
        shiny::fluidRow(
          shiny::column(8, shiny::textInput(session$ns("metadata_new_column"), "Add metadata column", value = "")),
          shiny::column(4, shiny::br(), shiny::actionButton(session$ns("metadata_add_column"), "Add column"))
        ),
        footer = shiny::tagList(
          shiny::modalButton("Close"),
          shiny::actionButton(session$ns("metadata_done"), "Save", class = "btn-primary")
        )
      ))
    })

    output$metadata_table <- DT::renderDT({
      DT::datatable(
        subject_metadata(),
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = 0)),
        options = list(scrollX = TRUE, paging = FALSE, searching = FALSE, info = FALSE)
      )
    })

    shiny::observeEvent(input$metadata_table_cell_edit, {
      info <- input$metadata_table_cell_edit
      table <- subject_metadata()
      row <- info$row
      col <- info$col + 1L
      if (!nrow(table) || row < 1L || row > nrow(table) || col < 1L || col > ncol(table)) {
        return(invisible(NULL))
      }
      col_name <- names(table)[[col]]
      if (identical(col_name, "id")) {
        return(invisible(NULL))
      }
      table[row, col_name] <- as.character(info$value)
      subject_metadata(table)
    })

    shiny::observeEvent(input$metadata_add_column, {
      col <- normalize_metadata_column_name(input$metadata_new_column)
      if (is.na(col)) {
        return(invisible(NULL))
      }
      table <- subject_metadata()
      if (!col %in% names(table)) {
        table[[col]] <- ""
      }
      subject_metadata(table)
      shiny::updateTextInput(session, "metadata_new_column", value = "")
    })

    shiny::observeEvent(input$metadata_done, {
      shiny::removeModal()
    })

    mapping <- shiny::reactive({
      upload <- uploaded()
      upload_mode <- upload$upload_mode %||% "single_file"
      list(
        id = if (identical(upload_mode, "multi_file")) ".source_id" else input$id_col,
        timestamp = input$timestamp_col,
        glucose = input$glucose_col,
        device = "",
        subject_metadata = clean_subject_metadata(subject_metadata()),
        source_units = input$source_units,
        timestamp_parser = "compatibility",
        upload_mode = upload_mode
      )
    })

    output$mapping_status <- shiny::renderUI({
      map <- mapping()
      upload <- uploaded()
      id_label <- subject_id_status_value(upload, map$id)
      metadata_cols <- setdiff(names(map$subject_metadata), "id")
      metadata_status <- if (length(metadata_cols)) paste("| Metadata =", paste(metadata_cols, collapse = ", ")) else ""
      shiny::tags$p(
        style = "margin-top: 8px; color: #555;",
        paste(
          "Current mapping:",
          "Subject ID =", id_label,
          "| Timestamp =", map$timestamp %||% "",
          "| Glucose =", map$glucose %||% "",
          metadata_status,
          "| Units =", map$source_units %||% "mg/dL"
        )
      )
    })

    mapping
  })
}
