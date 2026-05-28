column_mapping_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Column Mapping"),
    shiny::uiOutput(ns("mapping_note")),
    shiny::fluidRow(
      shiny::column(4, shiny::uiOutput(ns("id_mapping_ui"))),
      shiny::column(4, shiny::uiOutput(ns("timestamp_mapping_ui"))),
      shiny::column(4, shiny::uiOutput(ns("glucose_mapping_ui")))
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::radioButtons(
          ns("source_units"),
          "Source units",
          choices = c("mg/dL", "mmol/L"),
          selected = "mg/dL",
          inline = TRUE
        )
      ),
      shiny::column(
        4,
        shiny::uiOutput(ns("group_mapping_ui"))
      )
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

column_mapping_module_server <- function(id, uploaded) {
  shiny::moduleServer(id, function(input, output, session) {
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
          shiny::tags$label(subject_id_mapping_label(upload)),
          shiny::div(class = "form-control", "File name")
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

    output$group_mapping_ui <- shiny::renderUI({
      choices <- mapping_choices_for_upload(uploaded())
      shiny::selectInput(
        session$ns("group_col"),
        "Group / metadata (optional)",
        choices = choices$optional_choices,
        selected = ""
      )
    })

    mapping <- shiny::reactive({
      upload <- uploaded()
      upload_mode <- upload$upload_mode %||% "single_file"
      list(
        id = if (identical(upload_mode, "multi_file")) ".source_id" else input$id_col,
        timestamp = input$timestamp_col,
        glucose = input$glucose_col,
        group = input$group_col,
        device = "",
        source_units = input$source_units,
        upload_mode = upload_mode
      )
    })

    output$mapping_status <- shiny::renderUI({
      map <- mapping()
      upload <- uploaded()
      id_label <- subject_id_status_value(upload, map$id)
      group_label <- clean_mapping_value(map$group)
      group_status <- if (!is.na(group_label)) {
        paste("| Group =", group_label)
      } else {
        ""
      }
      shiny::tags$p(
        style = "margin-top: 8px; color: #555;",
        paste(
          "Current mapping:",
          "Subject ID =", id_label,
          "| Timestamp =", map$timestamp %||% "",
          "| Glucose =", map$glucose %||% "",
          group_status,
          "| Units =", map$source_units %||% "mg/dL"
        )
      )
    })

    mapping
  })
}
