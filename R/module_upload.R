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
    shiny::actionButton(ns("load_example_complete"), "Load complete example"),
    shiny::actionButton(
      ns("load_example_missing_5pct"),
      "Load 5% missing example"
    ),
    shiny::actionButton(
      ns("load_example_missing_10pct"),
      "Load 10% missing example"
    ),
    shiny::uiOutput(ns("upload_status")),
    shiny::uiOutput(ns("file_list"))
  )
}

upload_preview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Data Preview"),
    shiny::selectInput(
      ns("preview_rows"),
      "Preview rows",
      choices = preview_row_choices(),
      selected = "10",
      width = "160px"
    ),
    DT::DTOutput(ns("preview"))
  )
}

upload_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    selected_example <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      input$load_example_complete,
      {
        selected_example("complete")
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$load_example_missing_5pct,
      {
        selected_example("missing_5pct")
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$load_example_missing_10pct,
      {
        selected_example("missing_10pct")
      },
      ignoreInit = TRUE
    )

    file_probes <- shiny::reactive({
      files <- input$cgm_files
      if (is.null(files) || !nrow(files)) {
        return(list())
      }
      cgm_timed(
        "upload_probe_files",
        Map(probe_cgm_file_import, files$datapath, files$name),
        rows = nrow(files)
      )
    })

    uploaded <- shiny::reactive({
      files <- input$cgm_files
      if (!is.null(files) && nrow(files) > 0L) {
        probes <- file_probes()
        row_boundaries <- selected_import_row_boundaries(probes, input)
        upload_mode <- if (nrow(files) > 1L) "multi_file" else "single_file"
        combined <- tryCatch(
          cgm_timed(
            "upload_read_combine",
            combine_uploaded_files(
              files$datapath,
              files$name,
              header_rows = row_boundaries$header_row,
              first_data_rows = row_boundaries$first_data_row
            ),
            context = list(files = nrow(files), upload_mode = upload_mode)
          ),
          error = function(error) {
            return(structure(
              data.frame(),
              import_error = conditionMessage(error)
            ))
          }
        )
        import_error <- attr(combined, "import_error", exact = TRUE)
        if (!is.null(import_error)) {
          return(list(
            data = data.frame(),
            files = files$name,
            demo = FALSE,
            upload_mode = upload_mode,
            import_error = import_error,
            import_probes = probes,
            row_boundaries = row_boundaries
          ))
        }
        return(list(
          data = combined,
          files = files$name,
          demo = FALSE,
          upload_mode = upload_mode,
          import_probes = probes,
          row_boundaries = row_boundaries
        ))
      }

      example <- selected_example()
      if (is.null(example)) {
        shiny::req(FALSE)
      }

      switch(
        example,
        complete = list(
          data = cgm_timed("upload_load_example_complete", load_example_complete_cgm_data()),
          files = "cgm_example_complete.csv",
          demo = TRUE,
          upload_mode = "single_file"
        ),
        missing_5pct = list(
          data = cgm_timed("upload_load_example_5pct", load_example_missing_5pct_cgm_data()),
          files = "CGMExmplDat5Pct",
          demo = TRUE,
          upload_mode = "single_file"
        ),
        missing_10pct = list(
          data = cgm_timed("upload_load_example_10pct", load_example_missing_10pct_cgm_data()),
          files = "CGMExmplDat10Pct",
          demo = TRUE,
          upload_mode = "single_file"
        ),
        shiny::req(FALSE)
      )
    })

    output$import_setup <- shiny::renderUI({
      probes <- file_probes()
      import_setup_panel_ui(probes, session$ns)
    })

    output$upload_status <- shiny::renderUI({
      data <- uploaded()
      if (!is.null(data$import_error)) {
        return(shiny::div(
          class = "alert alert-warning",
          data$import_error
        ))
      }
      source_label <- if (isTRUE(data$demo)) "example data" else "uploaded data"
      mode_label <- if (identical(data$upload_mode, "multi_file")) {
        "one file per participant"
      } else {
        "combined file"
      }
      shiny::tags$p(
        paste(
          source_label,
          "-",
          mode_label,
          "-",
          length(data$files),
          "file(s),",
          nrow(data$data),
          "row(s),",
          ncol(data$data),
          "column(s)."
        )
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
      upload <- uploaded()
      preview <- cgm_timed("upload_preview_prepare", {
        data <- prepare_upload_preview_data(
          upload$data,
          upload$upload_mode %||% "single_file"
        )
        preview_data_rows(data, input$preview_rows %||% "10")
      })
      page_length <- if (identical(input$preview_rows, "all")) {
        100L
      } else {
        nrow(preview)
      }
      cgm_timed(
        "upload_preview_dt_render",
        DT::datatable(
          preview,
          rownames = FALSE,
          options = preview_dt_options(page_length = page_length)
        ),
        rows = nrow(preview)
      )
    })

    uploaded
  })
}
