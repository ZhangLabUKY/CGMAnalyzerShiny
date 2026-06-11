test_that("parse_cgm_timestamp handles common timestamp formats", {
  parsed <- parse_cgm_timestamp(c("2026-05-05 08:00:00", "05/05/2026 08:05"), timestamp_parser = "compatibility")

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
  ), timestamp_parser = "compatibility")

  expect_equal(format_cgm_timestamp_iso(parsed), c(
    "2026-05-06T11:30:00",
    "2026-05-06T11:30:00",
    "2026-05-13T23:30:00",
    "2026-05-13T23:30:00",
    "2026-05-06T11:30:00"
  ))
})

test_that("parse_cgm_timestamp handles year-first colon CGM timestamps", {
  parsed <- parse_cgm_timestamp(c(
    "2020:01:16:00:00",
    "2020:01:16:00:00:30",
    "2020-01-16T00:00:00"
  ), timestamp_parser = "compatibility")

  expect_equal(format_cgm_timestamp_iso(parsed), c(
    "2020-01-16T00:00:00",
    "2020-01-16T00:00:30",
    "2020-01-16T00:00:00"
  ))
})

test_that("parse_cgm_timestamp handles date-only timestamps at midnight", {
  parsed <- parse_cgm_timestamp(c("2020-12-25", "2019-12-29", "2019-11-18"))

  expect_equal(format_cgm_timestamp_iso(parsed), c(
    "2020-12-25T00:00:00",
    "2019-12-29T00:00:00",
    "2019-11-18T00:00:00"
  ))
  expect_true(all(detect_date_only_timestamps(c("2020-12-25", "2019-12-29", "2019-11-18"))))
})

test_that("parse_cgm_timestamp keeps ISO UTC timestamp parsing intact", {
  parsed <- parse_cgm_timestamp(c(
    "2020-05-11T00:06:17Z",
    "2020-05-11T00:06:17.123Z",
    "2020-05-11T00:06:17+0000"
  ))

  expect_equal(format_cgm_timestamp_iso(parsed), c(
    "2020-05-11T00:06:17",
    "2020-05-11T00:06:17",
    "2020-05-11T00:06:17"
  ))
  expect_false(any(detect_date_only_timestamps(c(
    "2020-05-11T00:06:17Z",
    "2020-05-11T00:06:17.123Z",
    "2020-05-11T00:06:17+0000"
  ))))
})

test_that("fasttime timestamp path preserves strict ISO timestamps", {
  testthat::skip_if_not_installed("fasttime")
  raw <- c("2026-05-05T08:00:00Z", "2026-05-05 08:05:00")

  parsed <- fasttime_parse_cgm_timestamp(raw, tz = "UTC")

  expect_s3_class(parsed, "POSIXct")
  expect_equal(format_cgm_timestamp_iso(parsed), c(
    "2026-05-05T08:00:00",
    "2026-05-05T08:05:00"
  ))
})

test_that("sample-detected fast timestamp parsing falls back only for failed rows", {
  raw <- c(rep("2026-05-05 08:00:00", 350), "not a timestamp")

  parsed <- parse_cgm_timestamp(raw, tz = "UTC", timestamp_parser = "compatibility")

  expect_equal(sum(!is.na(parsed)), 350L)
  expect_true(is.na(parsed[[351L]]))
  expect_equal(format_cgm_timestamp_iso(parsed[[1L]]), "2026-05-05T08:00:00")
})

test_that("default timestamp parsing prefers fasttime without compatibility fallback", {
  parsed <- parse_cgm_timestamp(c("2026-05-05 08:00:00", "2020:01:16:00:00", "05/05/2026 08:05"))

  expect_false(is.na(parsed[[1L]]))
  expect_equal(format_cgm_timestamp_iso(parsed[[2L]]), "2020-01-16T00:00:00")
  expect_true(is.na(parsed[[3L]]))
})

test_that("timestamp parser option can enable lubridate compatibility", {
  old <- options(CGMA.timestamp_parser = "compatibility")
  on.exit(options(old), add = TRUE)

  parsed <- parse_cgm_timestamp("05/05/2026 08:05")

  expect_false(is.na(parsed))
  expect_equal(format_cgm_timestamp_iso(parsed), "2026-05-05T08:05:00")
})

test_that("ambiguous day and month timestamps default to day first", {
  raw <- data.frame(
    id = "A",
    timestamp = "01-02-2019 02:49",
    glucose = 100,
    stringsAsFactors = FALSE
  )

  expect_true(has_ambiguous_timestamps(raw$timestamp))
  expect_equal(format_cgm_timestamp_iso(parse_cgm_timestamp(raw$timestamp, timestamp_parser = "compatibility")), "2019-02-01T02:49:00")
  out <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "timestamp", glucose = "glucose"),
    timestamp_parser = "compatibility"
  )
  mdy <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "timestamp", glucose = "glucose"),
    timestamp_date_order = "mdy",
    timestamp_parser = "compatibility"
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
    stringsAsFactors = FALSE
  )
  subject_metadata <- data.frame(
    id = c("A", "B"),
    group = c("Control", "Treatment"),
    age = c("42", "55"),
    sex = c("F", "M"),
    hba1c = c("6.2", "7.1"),
    empty_feature = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  out <- standardize_cgm_data(
    raw,
    mapping = list(
      id = "subject",
      timestamp = "time",
      glucose = "value",
      subject_metadata = subject_metadata,
      source_units = "mg/dL"
    ),
    units = "mg/dL"
  )

  expect_named(out, c("id", "id_source", "timestamp", "glucose", "units", "device", "source_file", "imputed_flag", "group", "age", "sex", "hba1c"))
  expect_s3_class(out, "data.frame")
  expect_false(data.table::is.data.table(out))
  expect_equal(out$id, c("A", "A", "B"))
  expect_equal(unique(out$id_source), subject_id_source_mapped())
  expect_equal(out$group, c("Control", "Control", "Treatment"))
  expect_equal(out$age, c("42", "42", "55"))
  expect_equal(out$sex, c("F", "F", "M"))
  expect_equal(out$hba1c, c("6.2", "6.2", "7.1"))
  expect_false("empty_feature" %in% names(out))
  expect_true(all(out$units == "mg/dL"))
  expect_true(all(out$imputed_flag == FALSE))
})

test_that("standardize_cgm_data sorts by reference-compatible order and preserves metadata", {
  raw <- data.frame(
    subject = c("B", "A", "A"),
    time = c("2026-05-05 08:10:00", "2026-05-05 08:05:00", "2026-05-05 08:00:00"),
    value = c("1,200", "100", "110"),
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    id = c("A", "B"),
    cohort = c("Control", "Treatment"),
    stringsAsFactors = FALSE
  )

  out <- standardize_cgm_data(
    raw,
    mapping = list(
      id = "subject",
      timestamp = "time",
      glucose = "value",
      subject_metadata = metadata
    )
  )

  expect_false(data.table::is.data.table(out))
  expect_equal(out$id, c("A", "A", "B"))
  expect_equal(format_cgm_timestamp_iso(out$timestamp), c(
    "2026-05-05T08:00:00",
    "2026-05-05T08:05:00",
    "2026-05-05T08:10:00"
  ))
  expect_equal(out$glucose, c(110, 100, 1200))
  expect_equal(out$cohort, c("Control", "Control", "Treatment"))
})

test_that("coerce_glucose handles numeric and comma-formatted character values", {
  expect_equal(coerce_glucose(c(100, 110.5)), c(100, 110.5))
  expect_equal(coerce_glucose(c("1,000", "120", "")), c(1000, 120, NA_real_))
})

test_that("example CGMissingDataR-style upload shape parses colon time and missing glucose", {
  raw <- data.frame(
    USUBJID = rep(c("11", "18"), each = 4),
    SEX = rep(c("F", "M"), each = 4),
    LBORRES = c("150.0", "", "125.0", "132.0", "140.0", NA, "138.0", "137.0"),
    Time = c(
      "2020:01:16:00:00",
      "2020:01:16:00:05",
      "2020:01:16:00:10",
      "2020:01:16:00:15",
      "2020:02:20:07:45",
      "2020:02:20:07:50",
      "2020:02:20:07:55",
      "2020:02:20:08:00"
    ),
    stringsAsFactors = FALSE
  )

  out <- standardize_cgm_data(
    raw,
    mapping = list(
      id = "USUBJID",
      timestamp = "Time",
      glucose = "LBORRES",
      subject_metadata = data.frame(id = c("11", "18"), group = c("F", "M"), stringsAsFactors = FALSE)
    ),
    timestamp_parser = "compatibility"
  )

  expect_equal(subject_id_values(out), c("11", "18"))
  expect_equal(unique(out$group), c("F", "M"))
  expect_equal(sum(is.na(out$timestamp)), 0)
  expect_equal(format_cgm_timestamp_iso(out$timestamp[[1L]]), "2020-01-16T00:00:00")
  expect_equal(sum(is.na(out$glucose)), 2)
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
  expect_false("group" %in% names(out))
  expect_false("age" %in% names(out))
  expect_false("sex" %in% names(out))
  expect_false("hba1c" %in% names(out))
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

test_that("standardize_cgm_data accepts date-only timestamp columns", {
  raw <- data.frame(
    id = c("A", "A", "A"),
    timestamp = c("2020-12-25", "2019-12-29", "2019-11-18"),
    glucose = c(100, 110, 120),
    stringsAsFactors = FALSE
  )

  standardized <- standardize_cgm_data(
    raw,
    mapping = list(id = "id", timestamp = "timestamp", glucose = "glucose")
  )
  exported <- prepare_cgm_data_export(standardized)

  expect_false(any(is.na(standardized$timestamp)))
  expect_equal(exported$timestamp, c(
    "2019-11-18T00:00:00",
    "2019-12-29T00:00:00",
    "2020-12-25T00:00:00"
  ))
})

test_that("read_cgm_file adds source file and filename-derived source id", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("time,glucose", "2026-05-05 08:00:00,100"), path)

  out <- read_cgm_file(path, "Patient-001.csv")

  expect_equal(out$.source_file, "Patient-001.csv")
  expect_equal(out$.source_id, "Patient-001")
  expect_equal(out$.import_header_row, 1L)
  expect_equal(out$.import_first_data_row, 2L)
  expect_equal(derive_source_id("Patient-002.csv"), "Patient-002")
})

test_that("read_cgm_file can read a selected setup sample", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "subject,time,glucose,extra",
    "A,2026-05-05 08:00:00,100,discard",
    "A,2026-05-05 08:05:00,101,discard",
    "A,2026-05-05 08:10:00,102,discard"
  ), path)

  out <- read_cgm_file(
    path,
    "Patient-001.csv",
    select_columns = c("subject", "time", "glucose"),
    nrows = 2
  )

  expect_equal(nrow(out), 2)
  expect_true(all(c("subject", "time", "glucose", ".source_file", ".source_id") %in% names(out)))
  expect_false("extra" %in% names(out))
})

test_that("read_cgm_file can do a selected full read when nrows is NULL", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "subject,time,glucose,extra",
    "A,2026-05-05 08:00:00,100,discard",
    "A,2026-05-05 08:05:00,101,discard",
    "A,2026-05-05 08:10:00,102,discard"
  ), path)

  out <- read_cgm_file(
    path,
    "Patient-001.csv",
    select_columns = c("subject", "time", "glucose"),
    nrows = NULL
  )

  expect_equal(nrow(out), 3)
  expect_true(all(c("subject", "time", "glucose") %in% names(out)))
  expect_false("extra" %in% names(out))
})

test_that("single-file standardization uses selected or filename-derived subject ids", {
  raw <- data.frame(
    subject = "SelectedA",
    time = "2026-05-05 08:00:00",
    glucose = 100,
    .source_file = "FallbackA.csv",
    .source_id = "FallbackA",
    stringsAsFactors = FALSE
  )

  selected <- standardize_cgm_data(
    raw,
    mapping = list(id = "subject", timestamp = "time", glucose = "glucose")
  )
  fallback <- standardize_cgm_data(
    raw,
    mapping = list(id = "", timestamp = "time", glucose = "glucose")
  )

  expect_equal(selected$id, "SelectedA")
  expect_equal(selected$id_source, subject_id_source_mapped())
  expect_equal(fallback$id, "FallbackA")
  expect_equal(fallback$id_source, subject_id_source_filename())
  expect_equal(fallback$source_file, "FallbackA.csv")
})

test_that("single-file standardization errors when subject id and filename fallback are absent", {
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
    mapping = list(
      id = "id",
      timestamp = "time",
      glucose = "glucose",
      subject_metadata = data.frame(id = "PatientA", group = "Control", stringsAsFactors = FALSE)
    ),
    upload_mode = "multi_file"
  )

  expect_equal(out$id, c("PatientA", "PatientA"))
  expect_equal(unique(out$id_source), subject_id_source_filename())
  expect_equal(out$group, c("Control", "Control"))
  expect_equal(out$source_file, c("PatientA.csv", "PatientA.csv"))
})

test_that("multiple uploaded files combine while preserving source fields", {
  path_a <- tempfile(fileext = ".csv")
  path_b <- tempfile(fileext = ".csv")
  writeLines(c("time,glucose,group", "2026-05-05 08:00:00,100,Control"), path_a)
  writeLines(c("time,glucose,group", "2026-05-05 08:00:00,120,Treatment"), path_b)

  combined <- combine_uploaded_files(c(path_a, path_b), c("A.csv", "B.csv"))

  expect_equal(nrow(combined), 2)
  expect_equal(combined$.source_file, c("A.csv", "B.csv"))
  expect_equal(combined$.source_id, c("A", "B"))

  out <- standardize_cgm_data(
    combined,
    mapping = list(
      timestamp = "time",
      glucose = "glucose",
      subject_metadata = data.frame(id = c("A", "B"), group = c("Control", "Treatment"), stringsAsFactors = FALSE)
    ),
    upload_mode = "multi_file"
  )

  expect_equal(out$id, c("A", "B"))
  expect_equal(out$id_source, c(subject_id_source_filename(), subject_id_source_filename()))
  expect_equal(out$group, c("Control", "Treatment"))
})

test_that("subject id visibility helpers distinguish mapped and filename-derived ids", {
  mapped <- data.frame(id = "A", id_source = subject_id_source_mapped(), stringsAsFactors = FALSE)
  single_filename <- data.frame(id = "A", id_source = subject_id_source_filename(), stringsAsFactors = FALSE)
  multi_filename <- data.frame(id = c("A", "B"), id_source = subject_id_source_filename(), stringsAsFactors = FALSE)

  expect_true(has_user_subject_id(mapped))
  expect_false(has_user_subject_id(single_filename))
  expect_true(subject_id_filter_available(mapped))
  expect_false(subject_id_filter_available(single_filename))
  expect_true(subject_id_filter_available(multi_filename))
})
