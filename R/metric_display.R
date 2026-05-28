metric_threshold_label <- function(raw_name, thresholds = default_cgm_thresholds()) {
  lower <- thresholds$tir_lower
  upper <- thresholds$tir_upper
  level2_low <- thresholds$tbr_level2
  level2_high <- thresholds$tar_level2
  labels <- c(
    tir_percent = paste0("Time in range (", lower, "-", upper, " mg/dL)"),
    tbr_percent = paste0("Time below range (<", lower, " mg/dL)"),
    tar_percent = paste0("Time above range (>", upper, " mg/dL)"),
    tbr_level2_percent = paste0("Level 2 below range (<", level2_low, " mg/dL)"),
    tbr_level1_percent = paste0("Level 1 below range (", level2_low, "-", lower - 1, " mg/dL)"),
    tar_level1_percent = paste0("Level 1 above range (", upper + 1, "-", level2_high, " mg/dL)"),
    tar_level2_percent = paste0("Level 2 above range (>", level2_high, " mg/dL)")
  )
  labels[[raw_name]] %||% NA_character_
}

metric_threshold_definition <- function(raw_name, thresholds = default_cgm_thresholds()) {
  lower <- thresholds$tir_lower
  upper <- thresholds$tir_upper
  level2_low <- thresholds$tbr_level2
  level2_high <- thresholds$tar_level2
  definitions <- c(
    tir_percent = paste0("Percent of observed readings between ", lower, " and ", upper, " mg/dL."),
    tbr_percent = paste0("Percent of observed readings below ", lower, " mg/dL."),
    tar_percent = paste0("Percent of observed readings above ", upper, " mg/dL."),
    tbr_level2_percent = paste0("Percent of observed readings below ", level2_low, " mg/dL."),
    tbr_level1_percent = paste0("Percent of observed readings from ", level2_low, " to ", lower - 1, " mg/dL."),
    tar_level1_percent = paste0("Percent of observed readings from ", upper + 1, " to ", level2_high, " mg/dL."),
    tar_level2_percent = paste0("Percent of observed readings above ", level2_high, " mg/dL.")
  )
  definitions[[raw_name]] %||% NA_character_
}

metric_display_catalog <- function(thresholds = default_cgm_thresholds()) {
  raw_name <- c(
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
    "conga_12h",
    "conga_24h",
    "modd",
    "mage"
  )
  metric <- c(
    "Readings",
    "Mean glucose",
    "Median glucose",
    "Minimum glucose",
    "Maximum glucose",
    "Standard deviation",
    "Coefficient of variation",
    "Glucose management indicator",
    metric_threshold_label("tir_percent", thresholds),
    metric_threshold_label("tbr_percent", thresholds),
    metric_threshold_label("tar_percent", thresholds),
    metric_threshold_label("tbr_level2_percent", thresholds),
    metric_threshold_label("tbr_level1_percent", thresholds),
    metric_threshold_label("tar_level1_percent", thresholds),
    metric_threshold_label("tar_level2_percent", thresholds),
    "Low blood glucose index",
    "High blood glucose index",
    "J-index",
    "CONGA, 12 hour",
    "CONGA, 24 hour",
    "Mean of daily differences",
    "Mean amplitude of glycemic excursions"
  )
  definition <- c(
    "Number of observed glucose readings included in the metric calculation.",
    "Average observed glucose concentration.",
    "Middle observed glucose value.",
    "Lowest observed glucose value.",
    "Highest observed glucose value.",
    "Standard deviation of observed glucose values.",
    "Glucose standard deviation divided by mean glucose.",
    "Estimated A1C-like index derived from mean glucose.",
    metric_threshold_definition("tir_percent", thresholds),
    metric_threshold_definition("tbr_percent", thresholds),
    metric_threshold_definition("tar_percent", thresholds),
    metric_threshold_definition("tbr_level2_percent", thresholds),
    metric_threshold_definition("tbr_level1_percent", thresholds),
    metric_threshold_definition("tar_level1_percent", thresholds),
    metric_threshold_definition("tar_level2_percent", thresholds),
    "Risk index summarizing exposure to low glucose values.",
    "Risk index summarizing exposure to high glucose values.",
    "Composite index reflecting glucose level and variability.",
    "Variability metric comparing glucose values separated by 12 hours.",
    "Variability metric comparing glucose values separated by 24 hours.",
    "Mean absolute difference between matched readings on consecutive days.",
    "Average size of major glucose excursions."
  )
  data.frame(
    raw_name = raw_name,
    metric = metric,
    definition = definition,
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
    digits = c(0, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2),
    metric_order = seq_along(raw_name),
    stringsAsFactors = FALSE
  )
}

metric_identifier_columns <- function(metrics) {
  c("id", "id_source", "group")[c("id", "id_source", "group") %in% names(metrics)]
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
    Definition = character(),
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
#' @noRd
prepare_metrics_display <- function(metrics, thresholds = default_cgm_thresholds()) {
  catalog <- metric_display_catalog(thresholds = thresholds)
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
      Definition = spec$definition,
      Value = format_metric_value(metrics[[spec$raw_name]], spec$digits),
      Units = spec$units,
      .metric_order = spec$metric_order,
      stringsAsFactors = FALSE
    )
    if (subject_id_filter_available(metrics)) {
      out[["Subject ID"]] <- as.character(metrics$id)
    }
    if ("group" %in% id_columns) {
      out$Group <- as.character(metrics$group)
    }
    out
  })

  out <- do.call(rbind, rows)
  out$Category <- factor(out$Category, levels = metric_category_order(), ordered = TRUE)
  order_columns <- c(intersect("Category", names(out)), intersect(c("Subject ID", "Group"), names(out)), ".metric_order")
  out <- out[do.call(order, out[order_columns]), , drop = FALSE]
  out$Category <- as.character(out$Category)
  out$.metric_order <- NULL
  leading <- c(intersect("Subject ID", names(out)), intersect("Group", names(out)))
  out <- out[, c(leading, "Category", "Metric", "Definition", "Value", "Units"), drop = FALSE]
  row.names(out) <- NULL
  out
}

filter_metrics_display <- function(
  display,
  participant = "",
  group = "",
  category = "",
  include_category = TRUE
) {
  participant <- normalize_filter_value(participant)
  group <- normalize_filter_value(group)
  category <- normalize_filter_value(category)

  if ("Subject ID" %in% names(display) && nzchar(participant %||% "")) {
    display <- display[display[["Subject ID"]] == participant, , drop = FALSE]
  }
  if ("Group" %in% names(display) && nzchar(group %||% "")) {
    display <- display[display$Group == group, , drop = FALSE]
  }
  if (isTRUE(include_category) && nzchar(category %||% "")) {
    display <- display[display$Category == category, , drop = FALSE]
  }
  display
}

metric_test_choices <- function(metrics, thresholds = default_cgm_thresholds()) {
  catalog <- metric_display_catalog(thresholds = thresholds)
  available <- catalog[catalog$raw_name %in% names(metrics), , drop = FALSE]
  stats::setNames(available$raw_name, available$metric)
}

key_metric_names <- function(thresholds = default_cgm_thresholds()) {
  catalog <- metric_display_catalog(thresholds = thresholds)
  raw_names <- c(
    "mean_glucose",
    "cv_percent",
    "tir_percent",
    "tbr_percent",
    "tar_percent",
    "gmi_percent"
  )
  catalog$metric[match(raw_names, catalog$raw_name)]
}

metric_summary_cards <- function(display, thresholds = default_cgm_thresholds()) {
  key_metrics <- key_metric_names(thresholds)
  rows <- lapply(key_metrics, function(metric_name) {
    values <- if ("Metric" %in% names(display)) {
      display$Value[display$Metric == metric_name]
    } else {
      numeric()
    }
    units <- if ("Metric" %in% names(display) && "Units" %in% names(display)) {
      display$Units[display$Metric == metric_name]
    } else {
      character()
    }
    numeric_values <- suppressWarnings(as.numeric(values))
    value <- if (length(numeric_values) && any(!is.na(numeric_values))) {
      round(mean(numeric_values, na.rm = TRUE), 2)
    } else {
      NA_real_
    }
    unit <- if (length(units)) units[[1L]] else ""
    data.frame(
      Label = metric_name,
      Value = if (is.na(value)) "NA" else trimws(paste(value, unit)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

optional_metric_note_text <- function(status) {
  if (identical(status, "failed")) {
    "Core clinical metrics are shown. Some optional metrics are not available for this dataset."
  } else {
    ""
  }
}

metrics_table_options <- function(display) {
  category_index <- match("Category", names(display)) - 1L
  options <- list(scrollX = FALSE, pageLength = 15)
  if (!is.na(category_index)) {
    options$rowGroup <- list(dataSrc = category_index)
    options$order <- list(list(category_index, "asc"))
    options$columnDefs <- list(list(targets = category_index, visible = FALSE))
  }
  options
}
