example_missing_5pct_raw_data <- function(fill_missing = FALSE) {
  data <- load_example_missing_5pct_cgm_data()
  if (isTRUE(fill_missing)) {
    glucose <- suppressWarnings(as.numeric(data$LBORRES))
    fill_value <- stats::median(glucose, na.rm = TRUE)
    data$LBORRES[is.na(glucose)] <- fill_value
  }
  data
}

example_missing_5pct_standardized <- function(fill_missing = FALSE) {
  standardize_cgm_data(
    example_missing_5pct_raw_data(fill_missing = fill_missing),
    mapping = list(id = "USUBJID", timestamp = "Time", glucose = "LBORRES")
  )
}
