test_that("parse_cgm_timestamp handles common timestamp formats", {
  parsed <- parse_cgm_timestamp(c("2026-05-05 08:00:00", "05/05/2026 08:05"))

  expect_s3_class(parsed, "POSIXct")
  expect_false(any(is.na(parsed)))
})

test_that("parse_cgm_timestamp handles ISO, AM/PM, dash dates, and Excel timestamps", {
  excel_value <- as.numeric(as.POSIXct("2026-05-06 11:30:00", tz = "UTC")) / 86400 + 25569
  parsed <- parse_cgm_timestamp(c(
    "2026-05-06T11:30:00",
    "2026-05-06 11:30",
    "05/13/2026 11:30 PM",
    "13-05-2026 23:30",
    as.character(excel_value)
  ))

  expect_equal(format_cgm_timestamp_iso(parsed), c(
    "2026-05-06T11:30:00",
    "2026-05-06T11:30:00",
    "2026-05-13T23:30:00",
    "2026-05-13T23:30:00",
    "2026-05-06T11:30:00"
  ))
})

test_that("ambiguous day and month timestamps default to day first", {
  raw <- data.frame(
    id = "A",
    timestamp = "01-02-2019 02:49",
    glucose = 100,
    stringsAsFactors = FALSE
  )

  expect_true(has_ambiguous_timestamps(raw$timestamp))
  expect_equal(format_cgm_timestamp_iso(parse_cgm_timestamp(raw$timestamp)), "2019-02-01T02:49:00")
  out <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "timestamp", glucose = "glucose")
  )
  mdy <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "timestamp", glucose = "glucose"),
    timestamp_date_order = "mdy"
  )

  expect_equal(format_cgm_timestamp_iso(out$timestamp), "2019-02-01T02:49:00")
  expect_equal(format_cgm_timestamp_iso(mdy$timestamp), "2019-01-02T02:49:00")
})

test_that("canonical timestamp formatter returns ISO-like app timestamps", {
  parsed <- parse_cgm_timestamp("2026-05-06 11:30:00")

  expect_equal(format_cgm_timestamp_iso(parsed), "2026-05-06T11:30:00")
  expect_equal(format_cgmanalyzer_timestamp(parsed), "2026:05:06:11:30")
})

test_that("standardize_cgm_data maps required and optional columns", {
  raw <- data.frame(
    subject = c("A", "A", "B"),
    time = c("2026-05-05 08:00:00", "2026-05-05 08:05:00", "2026-05-05 08:00:00"),
    value = c("100", "110", "6.0"),
    arm = c("Control", "Control", "Treatment"),
    stringsAsFactors = FALSE
  )

  out <- standardize_cgm_data(
    raw,
    mapping = list(id = "subject", timestamp = "time", glucose = "value", group = "arm", source_units = "mg/dL"),
    units = "mg/dL"
  )

  expect_named(out, c("id", "timestamp", "glucose", "units", "device", "group", "visit", "source_file", "imputed_flag"))
  expect_equal(out$id, c("A", "A", "B"))
  expect_equal(out$group, c("Control", "Control", "Treatment"))
  expect_true(all(out$units == "mg/dL"))
  expect_true(all(out$imputed_flag == FALSE))
})

test_that("standardize_cgm_data converts mmol/L to mg/dL", {
  raw <- data.frame(
    id = "A",
    timestamp = "2026-05-05 08:00:00",
    glucose = 6,
    stringsAsFactors = FALSE
  )

  out <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "timestamp", glucose = "glucose"),
    units = "mmol/L"
  )

  expect_equal(out$glucose, 6 * 18.0182, tolerance = 0.001)
})

test_that("standardize_cgm_data keeps optional columns empty when mappings are omitted", {
  raw <- data.frame(
    id = "A",
    timestamp = "2026-05-05 08:00:00",
    glucose = 100,
    device = "FreeStyle Libre",
    stringsAsFactors = FALSE
  )

  out <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "timestamp", glucose = "glucose")
  )

  expect_true(all(is.na(out$device)))
  expect_true(all(is.na(out$group)))
  expect_true(all(is.na(out$visit)))
})

test_that("standardize_cgm_data still supports direct backend device mapping", {
  raw <- data.frame(
    id = "A",
    timestamp = "2026-05-05 08:00:00",
    glucose = 100,
    device = "FreeStyle Libre",
    stringsAsFactors = FALSE
  )

  out <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "timestamp", glucose = "glucose", device = "device")
  )

  expect_equal(out$device, "FreeStyle Libre")
})

test_that("prepared CGM export uses canonical timestamp strings", {
  raw <- data.frame(
    id = "A",
    timestamp = "2026-05-06 11:30:00",
    glucose = 100,
    stringsAsFactors = FALSE
  )
  standardized <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "timestamp", glucose = "glucose")
  )
  exported <- prepare_cgm_data_export(standardized)

  expect_type(exported$timestamp, "character")
  expect_equal(exported$timestamp, "2026-05-06T11:30:00")
})

test_that("read_cgm_file adds source file and filename-derived source id", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("time,glucose", "2026-05-05 08:00:00,100"), path)

  out <- read_cgm_file(path, "Patient-001.csv")

  expect_equal(out$.source_file, "Patient-001.csv")
  expect_equal(out$.source_id, "Patient-001")
  expect_equal(derive_source_id("Patient-002.csv"), "Patient-002")
})

test_that("single-file standardization requires participant id mapping", {
  raw <- data.frame(
    time = "2026-05-05 08:00:00",
    glucose = 100,
    stringsAsFactors = FALSE
  )

  expect_error(
    standardize_cgm_data(raw, mapping = list(timestamp = "time", glucose = "glucose")),
    "Missing required mapping: id"
  )
})

test_that("multi-file standardization uses filename-derived participant ids", {
  raw <- data.frame(
    id = c("Wrong", "Wrong"),
    time = c("2026-05-05 08:00:00", "2026-05-05 08:05:00"),
    glucose = c(100, 110),
    group = c("Control", "Control"),
    .source_file = c("PatientA.csv", "PatientA.csv"),
    .source_id = c("PatientA", "PatientA"),
    stringsAsFactors = FALSE
  )

  out <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "time", glucose = "glucose", group = "group"),
    upload_mode = "multi_file"
  )

  expect_equal(out$id, c("PatientA", "PatientA"))
  expect_equal(out$group, c("Control", "Control"))
  expect_equal(out$source_file, c("PatientA.csv", "PatientA.csv"))
})

test_that("multiple uploaded files combine while preserving source fields", {
  path_a <- tempfile(fileext = ".csv")
  path_b <- tempfile(fileext = ".csv")
  writeLines(c("time,glucose,visit", "2026-05-05 08:00:00,100,Baseline"), path_a)
  writeLines(c("time,glucose,visit", "2026-05-05 08:00:00,120,Baseline"), path_b)

  combined <- combine_uploaded_files(c(path_a, path_b), c("A.csv", "B.csv"))

  expect_equal(nrow(combined), 2)
  expect_equal(combined$.source_file, c("A.csv", "B.csv"))
  expect_equal(combined$.source_id, c("A", "B"))

  out <- standardize_cgm_data(
    combined,
    mapping = list(timestamp = "time", glucose = "glucose", visit = "visit"),
    upload_mode = "multi_file"
  )

  expect_equal(out$id, c("A", "B"))
  expect_equal(out$visit, c("Baseline", "Baseline"))
})
