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

test_that("mapping choices expose metadata source columns without internal source columns", {
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

test_that("column mapping UI exposes subject metadata action and hides removed optional controls", {
  html <- paste(as.character(column_mapping_module_ui("column_mapping")), collapse = "\n")

  expect_true(grepl("cgm-column-mapping-grid", html, fixed = TRUE))
  expect_true(grepl("cgm-column-mapping-field-subject", html, fixed = TRUE))
  expect_true(grepl("cgm-column-mapping-field-timestamp", html, fixed = TRUE))
  expect_true(grepl("cgm-column-mapping-field-glucose", html, fixed = TRUE))
  expect_true(grepl("cgm-column-mapping-units", html, fixed = TRUE))
  expect_false(grepl("column_mapping-timestamp_parser", html, fixed = TRUE))
  expect_false(grepl("Fast year-first", html, fixed = TRUE))
  expect_false(grepl("Compatibility", html, fixed = TRUE))
  expect_false(grepl("Base strptime", html, fixed = TRUE))
  expect_true(grepl("column_mapping-edit_metadata", html, fixed = TRUE))
  expect_true(grepl("Subject Metadata", html, fixed = TRUE))
  expect_false(grepl("column_mapping-group_mapping_ui", html, fixed = TRUE))
  expect_false(grepl("column_mapping-visit_col", html, fixed = TRUE))
  expect_false(grepl("column_mapping-device_mapping_ui", html, fixed = TRUE))
  expect_false(grepl("column_mapping-timestamp_order_ui", html, fixed = TRUE))
  expect_false(grepl("Date order", html, fixed = TRUE))
})

test_that("subject metadata initializes, normalizes, and drops blank columns", {
  uploaded <- list(
    upload_mode = "multi_file",
    data = data.frame(
      SEX = c("F", "F", "M"),
      AGE = c("42", "42", ""),
      HBA1C = c("", "", ""),
      .source_id = c("A", "A", "B"),
      stringsAsFactors = FALSE
    )
  )

  metadata <- prefill_subject_metadata(uploaded)
  cleaned <- clean_subject_metadata(metadata)
  custom_name <- normalize_metadata_column_name("Baseline BMI (%)")

  expect_equal(metadata$id, c("A", "B"))
  expect_equal(metadata$sex, c("F", "M"))
  expect_equal(metadata$age, c("42", ""))
  expect_equal(metadata$hba1c, c("", ""))
  expect_true("sex" %in% names(cleaned))
  expect_true("age" %in% names(cleaned))
  expect_false("hba1c" %in% names(cleaned))
  expect_equal(custom_name, "baseline_bmi")
})

test_that("active tab helper gates tabs predictably", {
  active <- function() "metrics"

  expect_true(is_active_tab(active, "metrics"))
  expect_true(is_active_tab(active, c("statistics", "metrics")))
  expect_false(is_active_tab(active, "quality"))
})

test_that("upload module offers only retained bundled examples", {
  html <- as.character(upload_module_ui("upload"))

  expect_false(grepl("load_example_complete", html, fixed = TRUE))
  expect_false(grepl("Load complete example", html, fixed = TRUE))
  expect_true(grepl("load_example_missing_5pct", html, fixed = TRUE))
  expect_true(grepl("load_example_missing_10pct", html, fixed = TRUE))
})

test_that("upload module loads bundled examples through server actions", {
  shiny::testServer(upload_module_server, {
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
