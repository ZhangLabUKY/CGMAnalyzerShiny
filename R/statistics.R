grouping_choices <- function(metrics) {
  if (!is.data.frame(metrics) || !nrow(metrics)) {
    return(character())
  }
  choices <- names(metrics)[vapply(names(metrics), function(x) is_stat_grouping_column(metrics, x), logical(1))]
  preferred <- intersect(c("group", "sex"), choices)
  unique(c(preferred, setdiff(choices, preferred)))
}

stat_grouping_excluded_columns <- function() {
  unique(c(
    metric_source_columns(),
    "metric_engine",
    "cgmanalyzer_status",
    "iglu_status",
    metric_display_catalog()$raw_name
  ))
}

is_numeric_like <- function(x) {
  values <- trimws(as.character(x))
  values <- values[!is.na(values) & nzchar(values)]
  length(values) > 0L && all(!is.na(suppressWarnings(as.numeric(values))))
}

clean_stat_group_values <- function(x) {
  values <- trimws(as.character(x))
  values[is.na(values) | !nzchar(values)] <- NA_character_
  values
}

is_stat_grouping_column <- function(metrics, column) {
  if (!column %in% names(metrics) || column %in% stat_grouping_excluded_columns()) {
    return(FALSE)
  }
  values <- metrics[[column]]
  if (inherits(values, c("Date", "POSIXt")) || is.numeric(values) || is.integer(values) || is_numeric_like(values)) {
    return(FALSE)
  }
  values <- clean_stat_group_values(values)
  length(unique(values[!is.na(values)])) == 2L
}

format_grouping_label <- function(grouping) {
  labels <- c(group = "Group", sex = "Sex")
  if (grouping %in% names(labels)) {
    return(labels[[grouping]])
  }
  tools::toTitleCase(gsub("_", " ", grouping))
}

format_test_label <- function(test_type) {
  labels <- c(welch_t = "Welch t-test", wilcoxon = "Wilcoxon rank-sum")
  if (test_type %in% names(labels)) labels[[test_type]] else test_type
}

metric_label <- function(metric) {
  catalog <- metric_display_catalog()
  hit <- catalog$metric[match(metric, catalog$raw_name)]
  if (length(hit) && !is.na(hit)) hit else metric
}

insufficient_stat_result <- function(metric, grouping, test_type, groups = NA_character_, n = NA_character_, note) {
  data.frame(
    Metric = metric_label(metric),
    `Grouping Variable` = format_grouping_label(grouping),
    Test = format_test_label(test_type),
    Groups = groups,
    N = n,
    Statistic = NA_real_,
    `P-value` = NA_real_,
    Note = note,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

stat_analysis_data <- function(metrics, metric, grouping, period = default_time_window()) {
  metrics <- filter_metrics_by_period(metrics, period)
  if (!metric %in% names(metrics) || !grouping %in% names(metrics)) {
    return(data.frame(value = numeric(), group = character(), stringsAsFactors = FALSE))
  }

  analysis <- data.frame(
    value = as.numeric(metrics[[metric]]),
    group = clean_stat_group_values(metrics[[grouping]]),
    stringsAsFactors = FALSE
  )
  analysis[!is.na(analysis$value) & !is.na(analysis$group), , drop = FALSE]
}

summarize_metric_by_group <- function(metrics, metric, grouping = "group", period = default_time_window()) {
  analysis <- stat_analysis_data(metrics, metric, grouping, period)
  if (!nrow(analysis)) {
    return(data.frame(
      Group = character(),
      N = integer(),
      Mean = numeric(),
      SD = numeric(),
      Median = numeric(),
      IQR = numeric(),
      Minimum = numeric(),
      Maximum = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(sort(unique(analysis$group)), function(group_name) {
    values <- analysis$value[analysis$group == group_name]
    data.frame(
      Group = group_name,
      N = length(values),
      Mean = round(mean(values), 2),
      SD = round(stats::sd(values), 2),
      Median = round(stats::median(values), 2),
      IQR = round(stats::IQR(values), 2),
      Minimum = round(min(values), 2),
      Maximum = round(max(values), 2),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

format_stat_number <- function(value, digits = 3L) {
  value <- suppressWarnings(as.numeric(value[[1L]]))
  if (is.na(value) || !is.finite(value)) {
    return("NA")
  }
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_p_value <- function(value) {
  value <- suppressWarnings(as.numeric(value[[1L]]))
  if (is.na(value) || !is.finite(value)) {
    return("NA")
  }
  if (value < 0.001) {
    return("<0.001")
  }
  format(round(value, 3), nsmall = 3, trim = TRUE)
}

stat_method_note <- function(test_type) {
  notes <- c(
    welch_t = "Welch t-test compares group means and allows unequal variances.",
    wilcoxon = "Wilcoxon rank-sum compares group rank distributions."
  )
  notes[[test_type]] %||% ""
}

#' Run a participant-level metric statistical test
#'
#' @param metrics Wide subject-level metric data.
#' @param metric Raw metric column name.
#' @param grouping Grouping column name.
#' @param test_type Either `welch_t` or `wilcoxon`.
#' @param period Metric period to test when period-specific rows are present.
#'
#' @return One-row data frame with test result or an insufficiency note.
#' @noRd
run_metric_stat_test <- function(metrics, metric, grouping = "group", test_type = "welch_t", period = default_time_window()) {
  metrics <- filter_metrics_by_period(metrics, period)
  if (!metric %in% names(metrics)) {
    stop("Selected metric is not available.", call. = FALSE)
  }
  if (!grouping %in% names(metrics)) {
    return(insufficient_stat_result(metric, grouping, test_type, note = "Selected grouping variable is not available."))
  }
  if (!test_type %in% c("welch_t", "wilcoxon")) {
    stop("Unsupported test type.", call. = FALSE)
  }

  analysis <- stat_analysis_data(metrics, metric, grouping, period = period)
  group_names <- sort(unique(analysis$group))

  if (length(group_names) != 2L) {
    return(insufficient_stat_result(
      metric,
      grouping,
      test_type,
      groups = paste(group_names, collapse = " vs "),
      n = paste(table(analysis$group), collapse = " / "),
      note = "Select a grouping variable with exactly two groups for this test."
    ))
  }

  counts <- table(analysis$group)
  groups_label <- paste(group_names, collapse = " vs ")
  n_label <- paste(as.integer(counts[group_names]), collapse = " / ")
  if (any(counts[group_names] < 2L)) {
    return(insufficient_stat_result(
      metric,
      grouping,
      test_type,
      groups = groups_label,
      n = n_label,
      note = "At least two observations per group are required."
    ))
  }

  formula <- stats::as.formula("value ~ group")
  test <- if (identical(test_type, "welch_t")) {
    stats::t.test(formula, data = analysis)
  } else {
    stats::wilcox.test(formula, data = analysis, exact = FALSE)
  }

  data.frame(
    Metric = metric_label(metric),
    `Grouping Variable` = format_grouping_label(grouping),
    Test = format_test_label(test_type),
    Groups = groups_label,
    N = n_label,
    Statistic = unname(test$statistic),
    `P-value` = unname(test$p.value),
    Note = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
