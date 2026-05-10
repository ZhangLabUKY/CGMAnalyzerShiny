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
