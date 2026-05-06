upload_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Upload"),
    shiny::fileInput(
      ns("cgm_files"),
      "CGM files",
      multiple = TRUE,
      accept = c(".csv", ".txt", ".xlsx", ".xls")
    ),
    shiny::actionButton(ns("load_demo"), "Load demo data"),
    shiny::actionButton(ns("load_missingness_demo"), "Load missingness demo"),
    shiny::uiOutput(ns("upload_status")),
    shiny::uiOutput(ns("file_list"))
  )
}

upload_preview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Data Preview"),
    shiny::selectInput(ns("preview_rows"), "Preview rows", choices = preview_row_choices(), selected = "10", width = "160px"),
    DT::DTOutput(ns("preview"))
  )
}

upload_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    uploaded <- shiny::reactive({
      files <- input$cgm_files
      if (!is.null(files) && nrow(files) > 0L) {
        combined <- combine_uploaded_files(files$datapath, files$name)
        upload_mode <- if (nrow(files) > 1L) "multi_file" else "single_file"
        return(list(data = combined, files = files$name, demo = FALSE, upload_mode = upload_mode))
      }

      demo_clicks <- input$load_demo %||% 0L
      missing_demo_clicks <- input$load_missingness_demo %||% 0L
      if (demo_clicks < 1L && missing_demo_clicks < 1L) {
        shiny::req(FALSE)
      }

      if (missing_demo_clicks > demo_clicks) {
        list(data = load_missingness_demo_cgm_data(), files = "demo_cgm_missingness.csv", demo = TRUE, upload_mode = "single_file")
      } else {
        list(data = load_demo_cgm_data(), files = "demo_cgm.csv", demo = TRUE, upload_mode = "single_file")
      }
    })

    output$upload_status <- shiny::renderUI({
      data <- uploaded()
      source_label <- if (isTRUE(data$demo)) "demo data" else "uploaded data"
      mode_label <- if (identical(data$upload_mode, "multi_file")) "one file per participant" else "combined file"
      shiny::tags$p(
        paste(source_label, "-", mode_label, "-", length(data$files), "file(s),", nrow(data$data), "row(s),", ncol(data$data), "column(s).")
      )
    })

    output$file_list <- shiny::renderUI({
      files <- uploaded_file_names(uploaded())
      shiny::div(
        style = "display:flex; flex-wrap:wrap; gap:6px; margin: 0 0 12px 0;",
        lapply(files, function(file) {
          shiny::span(
            class = "badge bg-secondary",
            style = "font-size: 0.85rem;",
            file
          )
        })
      )
    })

    output$preview <- DT::renderDT({
      data <- uploaded()$data
      preview <- preview_data_rows(data, input$preview_rows %||% "10")
      page_length <- if (identical(input$preview_rows, "all")) 100L else nrow(preview)
      DT::datatable(
        preview,
        rownames = FALSE,
        options = preview_dt_options(page_length = page_length)
      )
    })

    uploaded
  })
}
