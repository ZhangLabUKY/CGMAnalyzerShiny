metric_display_catalog <- function() {
  data.frame(
    raw_name = c(
      "readings",
      "mean_glucose",
      "median_glucose",
      "min_glucose",
      "max_glucose",
      "sd_glucose",
      "cv_percent",
      "gmi_percent",
      "tir_percent",
      "tbr_percent",
      "tar_percent",
      "tbr_level2_percent",
      "tbr_level1_percent",
      "tar_level1_percent",
      "tar_level2_percent",
      "lbgi",
      "hbgi",
      "j_index",
      "conga_2h",
      "modd",
      "mage"
    ),
    metric = c(
      "Readings",
      "Mean glucose",
      "Median glucose",
      "Minimum glucose",
      "Maximum glucose",
      "Standard deviation",
      "Coefficient of variation",
      "Glucose management indicator",
      "Time in range (70-180 mg/dL)",
      "Time below range (<70 mg/dL)",
      "Time above range (>180 mg/dL)",
      "Level 2 below range (<54 mg/dL)",
      "Level 1 below range (54-69 mg/dL)",
      "Level 1 above range (181-250 mg/dL)",
      "Level 2 above range (>250 mg/dL)",
      "Low blood glucose index",
      "High blood glucose index",
      "J-index",
      "CONGA, 2 hour",
      "Mean of daily differences",
      "Mean amplitude of glycemic excursions"
    ),
    category = c(
      "Data Coverage",
      "Central Tendency",
      "Central Tendency",
      "Central Tendency",
      "Central Tendency",
      "Variability",
      "Variability",
      "Risk",
      "Time in Range",
      "Time in Range",
      "Time in Range",
      "Detailed Range Bands",
      "Detailed Range Bands",
      "Detailed Range Bands",
      "Detailed Range Bands",
      "Risk",
      "Risk",
      "Risk",
      "Excursions",
      "Excursions",
      "Excursions"
    ),
    units = c(
      "count",
      "mg/dL",
      "mg/dL",
      "mg/dL",
      "mg/dL",
      "mg/dL",
      "%",
      "%",
      "%",
      "%",
      "%",
      "%",
      "%",
      "%",
      "%",
      "index",
      "index",
      "index",
      "mg/dL",
      "mg/dL",
      "mg/dL"
    ),
    digits = c(0, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
    stringsAsFactors = FALSE
  )
}

metric_identifier_columns <- function(metrics) {
  c("id", "id_source", "group", "visit")[c("id", "id_source", "group", "visit") %in% names(metrics)]
}

metric_category_order <- function() {
  c(
    "Data Coverage",
    "Central Tendency",
    "Variability",
    "Time in Range",
    "Detailed Range Bands",
    "Risk",
    "Excursions"
  )
}

empty_metrics_display <- function() {
  data.frame(
    `Subject ID` = character(),
    Category = character(),
    Metric = character(),
    Value = numeric(),
    Units = character(),
    stringsAsFactors = FALSE
  )
}

metric_participant_filter_choices <- function(display) {
  values <- if ("Subject ID" %in% names(display)) display[["Subject ID"]] else character()
  filter_select_choices(sort(clean_filter_values(values)))
}

metric_category_filter_choices <- function(display) {
  values <- if ("Category" %in% names(display)) clean_filter_values(display$Category) else character()
  ordered <- metric_category_order()
  ordered <- ordered[ordered %in% values]
  filter_select_choices(ordered)
}

format_metric_value <- function(value, digits) {
  ifelse(is.na(value), NA_real_, round(as.numeric(value), digits = digits))
}

#' Prepare metrics for user-facing display
#'
#' @param metrics Wide metric data from `compute_core_metrics()`.
#'
#' @return Long-format user-facing metrics.
#' @export
prepare_metrics_display <- function(metrics) {
  catalog <- metric_display_catalog()
  available <- catalog[catalog$raw_name %in% names(metrics), , drop = FALSE]
  id_columns <- metric_identifier_columns(metrics)

  if (!nrow(metrics) || !nrow(available)) {
    return(empty_metrics_display())
  }

  rows <- lapply(seq_len(nrow(available)), function(i) {
    spec <- available[i, , drop = FALSE]
    out <- data.frame(
      Category = spec$category,
      Metric = spec$metric,
      Value = format_metric_value(metrics[[spec$raw_name]], spec$digits),
      Units = spec$units,
      stringsAsFactors = FALSE
    )
    if (subject_id_filter_available(metrics)) {
      out[["Subject ID"]] <- as.character(metrics$id)
    }
    if ("group" %in% id_columns) {
      out$Group <- as.character(metrics$group)
    }
    if ("visit" %in% id_columns) {
      out$Visit <- as.character(metrics$visit)
    }
    out
  })

  out <- do.call(rbind, rows)
  leading <- c(intersect("Subject ID", names(out)), intersect(c("Group", "Visit"), names(out)))
  out <- out[, c(leading, "Category", "Metric", "Value", "Units"), drop = FALSE]
  row.names(out) <- NULL
  out
}

filter_metrics_display <- function(
  display,
  participant = "",
  group = "",
  visit = "",
  category = "",
  include_category = TRUE
) {
  participant <- normalize_filter_value(participant)
  group <- normalize_filter_value(group)
  visit <- normalize_filter_value(visit)
  category <- normalize_filter_value(category)

  if ("Subject ID" %in% names(display) && nzchar(participant %||% "")) {
    display <- display[display[["Subject ID"]] == participant, , drop = FALSE]
  }
  if ("Group" %in% names(display) && nzchar(group %||% "")) {
    display <- display[display$Group == group, , drop = FALSE]
  }
  if ("Visit" %in% names(display) && nzchar(visit %||% "")) {
    display <- display[display$Visit == visit, , drop = FALSE]
  }
  if (isTRUE(include_category) && nzchar(category %||% "")) {
    display <- display[display$Category == category, , drop = FALSE]
  }
  display
}

metric_test_choices <- function(metrics) {
  catalog <- metric_display_catalog()
  available <- catalog[catalog$raw_name %in% names(metrics), , drop = FALSE]
  stats::setNames(available$raw_name, available$metric)
}
