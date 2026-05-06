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
      )
    ),
    shiny::uiOutput(ns("mapping_status"))
  )
}

guess_column <- function(columns, patterns) {
  hit <- columns[Reduce(`|`, lapply(patterns, function(pattern) grepl(pattern, columns, ignore.case = TRUE)))]
  if (length(hit)) hit[[1L]] else ""
}

column_mapping_module_server <- function(id, uploaded) {
  shiny::moduleServer(id, function(input, output, session) {
    output$mapping_note <- shiny::renderUI({
      upload <- uploaded()
      if (is_multi_file_upload(upload)) {
        shiny::div(
          class = "alert alert-info",
          "Multiple files detected. Participant IDs will be taken from file names."
        )
      }
    })

    output$id_mapping_ui <- shiny::renderUI({
      upload <- uploaded()
      if (is_multi_file_upload(upload)) {
        shiny::tagList(
          shiny::tags$label("Participant ID"),
          shiny::div(class = "form-control", "File name")
        )
      } else {
        choices <- mapping_choices_for_upload(upload)
        shiny::selectInput(session$ns("id_col"), "Participant ID", choices = choices$required_choices, selected = "")
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

    mapping <- shiny::reactive({
      upload <- uploaded()
      upload_mode <- upload$upload_mode %||% "single_file"
      list(
        id = if (identical(upload_mode, "multi_file")) ".source_id" else input$id_col,
        timestamp = input$timestamp_col,
        glucose = input$glucose_col,
        group = "",
        visit = "",
        device = "",
        source_units = input$source_units,
        upload_mode = upload_mode
      )
    })

    output$mapping_status <- shiny::renderUI({
      map <- mapping()
      upload <- uploaded()
      id_label <- if (is_multi_file_upload(upload)) "File name" else map$id %||% ""
      shiny::tags$p(
        style = "margin-top: 8px; color: #555;",
        paste(
          "Current mapping:",
          "Participant ID =", id_label,
          "| Timestamp =", map$timestamp %||% "",
          "| Glucose =", map$glucose %||% "",
          "| Units =", map$source_units %||% "mg/dL"
        )
      )
    })

    mapping
  })
}
