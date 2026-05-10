collect_polish_tag_ids <- function(ui) {
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

test_that("data setup status reports readiness steps", {
  upload <- list(
    data = data.frame(time = "2026-05-05 08:00:00", glucose = 100),
    files = "sample.csv"
  )
  mapping <- list(timestamp = "time", glucose = "glucose")
  standardized <- data.frame(
    id = "sample",
    timestamp = parse_cgm_timestamp("2026-05-05 08:00:00"),
    glucose = 100,
    stringsAsFactors = FALSE
  )
  settings <- list(analysis_date_range = c(start = "2026-05-05", end = "2026-05-05"))

  status <- data_setup_status(upload, mapping, standardized, settings = settings)

  expect_equal(status$Step, c("Files loaded", "Required mappings", "Timestamps parsed", "Analysis date range"))
  expect_true(all(status$Status == "Ready"))
  expect_true(any(grepl("2026-05-05", status$Detail, fixed = TRUE)))
})

test_that("uploaded data helper detects usable uploads", {
  expect_false(has_uploaded_data(NULL))
  expect_false(has_uploaded_data(list(data = data.frame(), files = character())))
  expect_false(has_uploaded_data(list(data = NULL, files = "sample.csv")))
  expect_true(has_uploaded_data(list(data = data.frame(glucose = 100), files = "sample.csv")))
})

test_that("data setup status identifies missing required mappings", {
  upload <- list(data = data.frame(time = "x", glucose = 100), files = "sample.csv")
  mapping <- list(timestamp = "", glucose = "glucose")

  status <- data_setup_status(upload, mapping)
  mappings <- status[status$Step == "Required mappings", , drop = FALSE]

  expect_equal(mappings$Status, "Needs attention")
  expect_true(grepl("Select timestamp", mappings$Detail, fixed = TRUE))
})

test_that("data upload summary reports compact counts", {
  upload <- list(
    data = data.frame(time = c("2026-05-05 08:00:00", "2026-05-06 08:00:00"), glucose = c(100, NA)),
    files = c("a.csv", "b.csv")
  )
  standardized <- data.frame(
    id = c("A", "B"),
    timestamp = parse_cgm_timestamp(c("2026-05-05 08:00:00", "2026-05-06 08:00:00")),
    glucose = c(100, NA),
    stringsAsFactors = FALSE
  )

  summary <- data_upload_summary(upload, standardized)

  expect_equal(summary$Value[summary$Label == "Rows"], "2")
  expect_equal(summary$Value[summary$Label == "Files"], "2")
  expect_equal(summary$Value[summary$Label == "Subject IDs"], "2")
  expect_equal(summary$Value[summary$Label == "Missing glucose"], "1")
})

test_that("quality summary cards use QC and missingness values", {
  data <- data.frame(
    id = c("A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-05 08:05:00",
      "2026-05-06 08:00:00"
    )),
    glucose = c(100, NA, 120),
    imputed_flag = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  qc <- data.frame(id = c("A", "B"), valid_days = c(1, 2))
  missingness <- data.frame(
    `Timestamp gaps` = c(2, 3),
    `Missing glucose after preprocessing` = c(1, 0),
    `Filled glucose rows` = c(1, 0),
    check.names = FALSE
  )

  cards <- quality_summary_cards(data, qc, missingness)

  expect_equal(cards$Value[cards$Label == "Subject IDs"], "2")
  expect_equal(cards$Value[cards$Label == "Valid days"], "3")
  expect_equal(cards$Value[cards$Label == "Timestamp gaps"], "5")
  expect_equal(cards$Value[cards$Label == "Filled rows"], "1")
})

test_that("plot selection summary respects daily overlay day filtering", {
  data <- data.frame(
    id = c("A", "A", "B"),
    timestamp = parse_cgm_timestamp(c(
      "2026-05-05 08:00:00",
      "2026-05-06 08:00:00",
      "2026-05-06 09:00:00"
    )),
    glucose = c(100, 110, 120),
    group = NA_character_,
    visit = NA_character_,
    stringsAsFactors = FALSE
  )

  summary <- plot_selection_summary(data, plot_type = "daily_overlay", day = "2026-05-06")

  expect_equal(summary$Value[summary$Label == "Rows plotted"], "2")
  expect_equal(summary$Value[summary$Label == "Subject IDs"], "2")
  expect_equal(summary$Value[summary$Label == "Days"], "1")
})

test_that("main tab UI exposes polish summary outputs", {
  app_code <- paste(readLines(testthat::test_path("../../R/app_ui.R"), warn = FALSE), collapse = "\n")
  server_code <- paste(readLines(testthat::test_path("../../R/app_server.R"), warn = FALSE), collapse = "\n")
  app_ids <- collect_polish_tag_ids(app_ui())
  qc_html <- paste(as.character(qc_module_ui("qc")), collapse = "\n")
  plots_html <- paste(as.character(plots_module_ui("plots")), collapse = "\n")
  metrics_html <- paste(as.character(metrics_module_ui("metrics")), collapse = "\n")

  expect_true("data_workflow_ui" %in% app_ids)
  expect_true(grepl("data_setup_status", server_code, fixed = TRUE))
  expect_true(grepl("data_summary", server_code, fixed = TRUE))
  expect_true(grepl("column_mapping_module_ui", server_code, fixed = TRUE))
  expect_true(grepl("preprocessing_module_ui", server_code, fixed = TRUE))
  expect_true(grepl("upload_preview_ui", server_code, fixed = TRUE))
  expect_false("column_mapping-mapping_note" %in% app_ids)
  expect_false("preprocessing-tir_lower" %in% app_ids)
  expect_false("upload-preview_rows" %in% app_ids)
  expect_true(grepl("qc-qc_summary_cards", qc_html, fixed = TRUE))
  expect_true(grepl("plots-plot_summary", plots_html, fixed = TRUE))
  expect_true(grepl("Metric Overview", metrics_html, fixed = TRUE))
  expect_true(grepl("Detailed Metrics", metrics_html, fixed = TRUE))
})
