test_that("preview_data_rows respects default, numeric, and All row limits", {
  data <- data.frame(x = seq_len(150))

  expect_equal(nrow(preview_data_rows(data, NULL)), 10)
  expect_equal(nrow(preview_data_rows(data, "100")), 100)
  expect_equal(nrow(preview_data_rows(data, "all")), 150)
  expect_equal(nrow(preview_data_rows(data, "not-a-number")), 10)
})

test_that("upload preview hides internal source columns for single-file uploads", {
  data <- data.frame(
    time = c("2026-05-05 08:00:00", "2026-05-05 08:05:00"),
    glucose = c(100, 105),
    .source_file = "Study.csv",
    .source_id = "Study",
    .import_header_row = 1L,
    .import_first_data_row = 2L,
    stringsAsFactors = FALSE
  )

  preview <- prepare_upload_preview_data(data, upload_mode = "single_file")

  expect_equal(nrow(preview), nrow(data))
  expect_false(".source_file" %in% names(preview))
  expect_false(".source_id" %in% names(preview))
  expect_false(".import_header_row" %in% names(preview))
  expect_false(".import_first_data_row" %in% names(preview))
  expect_false("Source file" %in% names(preview))
  expect_equal(names(preview), c("time", "glucose"))
})

test_that("upload preview shows friendly source file for multi-file uploads", {
  data <- data.frame(
    time = c("2026-05-05 08:00:00", "2026-05-06 08:00:00"),
    glucose = c(100, 120),
    .source_file = c("SubjectA.csv", "SubjectB.csv"),
    .source_id = c("SubjectA", "SubjectB"),
    .import_header_row = c(1L, 1L),
    .import_first_data_row = c(2L, 2L),
    stringsAsFactors = FALSE
  )

  preview <- prepare_upload_preview_data(data, upload_mode = "multi_file")

  expect_equal(nrow(preview), nrow(data))
  expect_false(".source_file" %in% names(preview))
  expect_false(".source_id" %in% names(preview))
  expect_false(".import_header_row" %in% names(preview))
  expect_false(".import_first_data_row" %in% names(preview))
  expect_equal(names(preview)[1], "Source file")
  expect_equal(preview[["Source file"]], c("SubjectA.csv", "SubjectB.csv"))
})

test_that("preview DT options enable horizontal and vertical scrolling", {
  options <- preview_dt_options(page_length = 25)

  expect_true(options$scrollX)
  expect_equal(options$scrollY, "420px")
  expect_equal(options$pageLength, 25)
  expect_false(options$lengthChange)
})

test_that("uploaded_file_names returns single and multiple filenames", {
  expect_equal(uploaded_file_names(list(files = "one.csv")), "one.csv")
  expect_equal(uploaded_file_names(list(files = c("one.csv", "two.csv"))), c("one.csv", "two.csv"))
})

test_that("mapping choices support multi-file filename ids and first-render columns", {
  uploaded <- list(
    upload_mode = "multi_file",
    data = data.frame(
      time = "2026-05-05 08:00:00",
      glucose = 100,
      group = "Control",
      .source_file = "A.csv",
      .source_id = "A",
      stringsAsFactors = FALSE
    )
  )

  choices <- mapping_choices_for_upload(uploaded)

  expect_equal(choices$id, ".source_id")
  expect_equal(choices$timestamp, "")
  expect_equal(choices$glucose, "")
  expect_true("time" %in% choices$columns)
  expect_true("glucose" %in% choices$columns)
  expect_false(".source_file" %in% choices$columns)
  expect_false(".source_id" %in% choices$columns)
  expect_equal(unname(choices$required_choices), c("", "time", "glucose", "group"))
  expect_equal(unname(choices$optional_choices), c("", "time", "glucose", "group"))
})

test_that("mapping choices expose optional group metadata without internal source columns", {
  uploaded <- list(
    upload_mode = "single_file",
    data = data.frame(
      USUBJID = "11",
      SEX = "F",
      Time = "2020:01:16:00:00",
      LBORRES = 150,
      .source_file = "Example.csv",
      .source_id = "Example",
      stringsAsFactors = FALSE
    )
  )

  choices <- mapping_choices_for_upload(uploaded)

  expect_true("SEX" %in% choices$columns)
  expect_true("SEX" %in% unname(choices$optional_choices))
  expect_false(".source_file" %in% unname(choices$optional_choices))
  expect_false(".source_id" %in% unname(choices$optional_choices))
  expect_equal(choices$group, "")
})

test_that("mapping choices keep single-file subject id optional", {
  uploaded <- list(
    upload_mode = "single_file",
    data = data.frame(
      subject_id = "A",
      timestamp = "2026-05-05 08:00:00",
      glucose = 100,
      .source_file = "Combined.csv",
      .source_id = "Combined",
      stringsAsFactors = FALSE
    )
  )

  choices <- mapping_choices_for_upload(uploaded)

  expect_equal(choices$id, "")
  expect_equal(choices$timestamp, "")
  expect_equal(choices$glucose, "")
  expect_false(".source_file" %in% choices$columns)
  expect_false(".source_id" %in% choices$columns)
  expect_equal(unname(choices$required_choices), c("", "subject_id", "timestamp", "glucose"))
})

test_that("subject id mapping labels and status use filename fallback", {
  single <- list(upload_mode = "single_file")
  multi <- list(upload_mode = "multi_file")

  expect_equal(subject_id_mapping_label(single), "Subject ID (optional)")
  expect_equal(subject_id_mapping_label(multi), "Subject ID")
  expect_equal(subject_id_status_value(single, ""), "File name")
  expect_equal(subject_id_status_value(single, "subject_id"), "subject_id")
  expect_equal(subject_id_status_value(multi, "ignored_column"), "File name")
})

test_that("column mapping UI exposes group metadata and hides removed optional controls", {
  html <- paste(as.character(column_mapping_module_ui("column_mapping")), collapse = "\n")

  expect_true(grepl("column_mapping-group_mapping_ui", html, fixed = TRUE))
  expect_false(grepl("column_mapping-visit_col", html, fixed = TRUE))
  expect_false(grepl("column_mapping-device_mapping_ui", html, fixed = TRUE))
  expect_false(grepl("column_mapping-timestamp_order_ui", html, fixed = TRUE))
  expect_false(grepl("Date order", html, fixed = TRUE))
})

collect_tag_ids <- function(ui) {
  ids <- character()
  walk <- function(node) {
    if (inherits(node, "shiny.tag")) {
      id <- node$attribs$id
      if (!is.null(id)) {
        ids <<- c(ids, id)
      }
      lapply(node$children, walk)
    } else if (inherits(node, "shiny.tag.list") || is.list(node)) {
      lapply(node, walk)
    }
    invisible(NULL)
  }
  walk(ui)
  ids
}

collect_tag_classes <- function(ui) {
  classes <- character()
  walk <- function(node) {
    if (inherits(node, "shiny.tag")) {
      class <- node$attribs$class
      if (!is.null(class)) {
        classes <<- c(classes, class)
      }
      lapply(node$children, walk)
    } else if (inherits(node, "shiny.tag.list") || is.list(node)) {
      lapply(node, walk)
    }
    invisible(NULL)
  }
  walk(ui)
  classes
}

test_that("data tab hides downstream workflow before upload", {
  ids <- collect_tag_ids(app_ui())

  expect_true("upload-cgm_files" %in% ids)
  expect_true("upload-load_example_complete" %in% ids)
  expect_true("upload-load_example_missing_5pct" %in% ids)
  expect_true("upload-load_example_missing_10pct" %in% ids)
  expect_false("upload-load_demo" %in% ids)
  expect_false("upload-load_missingness_demo" %in% ids)
  expect_true("data_upload_hint" %in% ids)
  expect_true("upload-import_setup" %in% ids)
  expect_true("data_mapping_ui" %in% ids)
  expect_true("data_workflow_ui" %in% ids)
  expect_false("column_mapping-mapping_note" %in% ids)
  expect_false("preprocessing-tir_lower" %in% ids)
  expect_false("upload-preview_rows" %in% ids)
})

test_that("navbar exposes stable selected tab id and spinner-wrapped heavy outputs", {
  ui <- app_ui()
  ids <- collect_tag_ids(ui)
  classes <- collect_tag_classes(ui)

  expect_true("active_tab" %in% ids)
  expect_true(any(grepl("load-container", classes, fixed = TRUE)))
  expect_true("metrics-metrics_table" %in% ids)
  expect_true("qc-qc_table" %in% ids)
  expect_true("qc-missingness_subject_filter" %in% ids)
  expect_false("qc-missingness_participant" %in% ids)
  expect_true("qc-missingness_heatmap_ui" %in% ids)
  expect_false("qc-missingness_timeline" %in% ids)
  expect_true("plots-filter_layout" %in% ids)
  expect_false("plots-participant" %in% ids)
  expect_true("plots-active_plot" %in% ids)
  expect_false("plots-plot_detail" %in% ids)
  expect_true("stats-test_result" %in% ids)
  expect_false("complexity-data_requirements" %in% ids)
  expect_true("complexity-metrics_table_ui" %in% ids)
})

test_that("navbar places complexity after metrics and before plots/statistics", {
  expected_labels <- c("Data", "Quality", "Metrics", "Complexity", "Plots", "Statistics", "Export")
  labels <- character()
  walk_links <- function(node) {
    if (inherits(node, "shiny.tag")) {
      if (identical(node[["name"]], "a")) {
        labels <<- c(labels, paste(unlist(node[["children"]]), collapse = ""))
      }
      lapply(node[["children"]], walk_links)
    } else if (inherits(node, "shiny.tag.list") || is.list(node)) {
      lapply(node, walk_links)
    }
    invisible(NULL)
  }
  walk_links(app_ui())
  labels <- labels[labels %in% expected_labels]
  positions <- stats::setNames(match(expected_labels, labels), expected_labels)

  expect_false(any(is.na(positions)))
  expect_true(positions[["Metrics"]] < positions[["Complexity"]])
  expect_true(positions[["Complexity"]] < positions[["Plots"]])
  expect_true(positions[["Complexity"]] < positions[["Statistics"]])
  expect_true(positions[["Statistics"]] < positions[["Export"]])
})

test_that("active tab helper gates tabs predictably", {
  active <- function() "metrics"

  expect_true(is_active_tab(active, "metrics"))
  expect_true(is_active_tab(active, c("statistics", "metrics")))
  expect_false(is_active_tab(active, "quality"))
})

test_that("upload module loads bundled examples through server actions", {
  shiny::testServer(upload_module_server, {
    session$setInputs(load_example_complete = 0)
    session$flushReact()
    session$setInputs(load_example_complete = 1)
    session$flushReact()
    complete <- uploaded()
    expect_true(isTRUE(complete$demo))
    expect_equal(complete$files, "cgm_example_complete.csv")
    expect_equal(complete$upload_mode, "single_file")
    expect_true(nrow(complete$data) > 0L)

    session$setInputs(load_example_missing_5pct = 0)
    session$flushReact()
    session$setInputs(load_example_missing_5pct = 1)
    session$flushReact()
    missing_5pct <- uploaded()
    expect_true(isTRUE(missing_5pct$demo))
    expect_equal(missing_5pct$files, "CGMExmplDat5Pct")
    expect_true(any(is.na(missing_5pct$data$LBORRES)))
  })
})
