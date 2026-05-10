test_that("preview_data_rows respects default, numeric, and All row limits", {
  data <- data.frame(x = seq_len(150))

  expect_equal(nrow(preview_data_rows(data, NULL)), 10)
  expect_equal(nrow(preview_data_rows(data, "100")), 100)
  expect_equal(nrow(preview_data_rows(data, "all")), 150)
  expect_equal(nrow(preview_data_rows(data, "not-a-number")), 10)
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
  expect_equal(unname(choices$required_choices), c("", "time", "glucose", "group", ".source_id"))
  expect_equal(unname(choices$optional_choices), c("", "time", "glucose", "group", ".source_id"))
})

test_that("mapping choices keep single-file subject id optional", {
  uploaded <- list(
    upload_mode = "single_file",
    data = data.frame(
      subject_id = "A",
      timestamp = "2026-05-05 08:00:00",
      glucose = 100,
      stringsAsFactors = FALSE
    )
  )

  choices <- mapping_choices_for_upload(uploaded)

  expect_equal(choices$id, "")
  expect_equal(choices$timestamp, "")
  expect_equal(choices$glucose, "")
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

test_that("column mapping UI hides group, visit, device, and date order controls", {
  html <- paste(as.character(column_mapping_module_ui("column_mapping")), collapse = "\n")

  expect_false(grepl("column_mapping-group_col", html, fixed = TRUE))
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
  expect_true("upload-load_demo" %in% ids)
  expect_true("data_upload_hint" %in% ids)
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
  expect_true("qc-missingness_heatmap" %in% ids)
  expect_false("qc-missingness_timeline" %in% ids)
  expect_true("plots-subject_filter" %in% ids)
  expect_false("plots-participant" %in% ids)
  expect_true("plots-active_plot" %in% ids)
  expect_false("plots-plot_detail" %in% ids)
  expect_true("stats-test_result" %in% ids)
  expect_true("complexity-data_requirements" %in% ids)
})

test_that("active tab helper gates tabs predictably", {
  active <- function() "metrics"

  expect_true(is_active_tab(active, "metrics"))
  expect_true(is_active_tab(active, c("statistics", "metrics")))
  expect_false(is_active_tab(active, "quality"))
})
