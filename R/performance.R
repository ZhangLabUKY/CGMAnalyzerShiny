cgm_data_signature <- function(data) {
  if (is.null(data) || !is.data.frame(data)) {
    return(list(rows = 0L))
  }

  timestamp <- if ("timestamp" %in% names(data)) data$timestamp else as.POSIXct(character())
  glucose <- if ("glucose" %in% names(data)) data$glucose else numeric()
  id <- if ("id" %in% names(data)) data$id else character()
  id_source <- if ("id_source" %in% names(data)) data$id_source else character()
  source_file <- if ("source_file" %in% names(data)) data$source_file else character()
  imputed_flag <- if ("imputed_flag" %in% names(data)) data$imputed_flag else logical()

  timestamp_numeric <- suppressWarnings(as.numeric(timestamp))
  finite_timestamp <- is.finite(timestamp_numeric)
  list(
    rows = nrow(data),
    participants = length(unique(id[!is.na(id)])),
    id_sources = sort(unique(id_source[!is.na(id_source)])),
    source_files = sort(unique(source_file[!is.na(source_file)])),
    first_timestamp = if (any(finite_timestamp)) min(timestamp_numeric[finite_timestamp]) else NA_real_,
    last_timestamp = if (any(finite_timestamp)) max(timestamp_numeric[finite_timestamp]) else NA_real_,
    missing_glucose = sum(is.na(glucose)),
    glucose_sum = sum(as.numeric(glucose), na.rm = TRUE),
    imputed_rows = sum(imputed_flag %in% TRUE, na.rm = TRUE)
  )
}

threshold_signature <- function(thresholds) {
  unlist(thresholds[c("tbr_level2", "tir_lower", "tir_upper", "tar_level2")], use.names = TRUE)
}

cgm_performance_log_enabled <- function() {
  isTRUE(getOption("CGMA.performance_log", FALSE))
}

cgm_performance_log_file <- function() {
  file <- getOption("CGMA.performance_log_file", FALSE)
  if (isFALSE(file) || is.null(file)) {
    return("")
  }
  if (isTRUE(file)) {
    file.path(
      "dev_notes",
      "logs",
      paste0("performance-", format(Sys.Date(), "%Y%m%d"), ".csv")
    )
  } else {
    as.character(file[[1L]])
  }
}

cgm_performance_result_rows <- function(value) {
  if (is.data.frame(value)) {
    return(nrow(value))
  }
  if (is.list(value) && is.data.frame(value$data)) {
    return(nrow(value$data))
  }
  NA_integer_
}

cgm_performance_context_text <- function(context = NULL) {
  if (is.null(context) || !length(context)) {
    return("")
  }
  values <- unlist(context, recursive = FALSE, use.names = TRUE)
  values <- values[!is.na(names(values)) & nzchar(names(values))]
  if (!length(values)) {
    return("")
  }
  paste(
    sprintf("%s=%s", names(values), vapply(values, as.character, character(1))),
    collapse = ";"
  )
}

cgm_log_performance <- function(label, elapsed_ms, rows = NA_integer_, status = "ok", context = NULL) {
  if (!cgm_performance_log_enabled()) {
    return(invisible(NULL))
  }
  label <- as.character(label[[1L]])
  row_text <- if (is.na(rows)) "" else paste0(" rows=", rows)
  context_text <- cgm_performance_context_text(context)
  context_message <- if (nzchar(context_text)) paste0(" ", context_text) else ""
  message(sprintf("[CGMA perf] %s %.1f ms status=%s%s%s", label, elapsed_ms, status, row_text, context_message))

  file <- cgm_performance_log_file()
  if (nzchar(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    row <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      label = label,
      elapsed_ms = round(elapsed_ms, 3),
      rows = if (is.na(rows)) NA_integer_ else as.integer(rows),
      status = status,
      context = context_text,
      stringsAsFactors = FALSE
    )
    utils::write.table(
      row,
      file = file,
      append = file.exists(file),
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists(file),
      qmethod = "double"
    )
  }
  invisible(NULL)
}

cgm_timed <- function(label, expr, rows = NULL, context = NULL) {
  if (!cgm_performance_log_enabled()) {
    return(force(expr))
  }
  start <- proc.time()[["elapsed"]]
  status <- "ok"
  error <- NULL
  value <- tryCatch(
    force(expr),
    error = function(e) {
      status <<- "error"
      error <<- e
      NULL
    }
  )
  elapsed_ms <- 1000 * (proc.time()[["elapsed"]] - start)
  resolved_rows <- rows
  if (is.null(resolved_rows)) {
    resolved_rows <- cgm_performance_result_rows(value)
  }
  log_context <- context
  if (!is.null(error)) {
    log_context <- c(log_context %||% list(), list(error_message = conditionMessage(error)))
  }
  cgm_log_performance(
    label = label,
    elapsed_ms = elapsed_ms,
    rows = resolved_rows %||% NA_integer_,
    status = status,
    context = log_context
  )
  if (!is.null(error)) {
    stop(error)
  }
  value
}
