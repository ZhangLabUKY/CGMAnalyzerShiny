grouping_choices <- function(metrics) {
  choices <- intersect(c("group", "visit"), names(metrics))
  choices[vapply(choices, function(x) length(unique(stats::na.omit(metrics[[x]]))) >= 2L, logical(1))]
}

format_grouping_label <- function(grouping) {
  labels <- c(group = "Group", visit = "Visit")
  if (grouping %in% names(labels)) labels[[grouping]] else grouping
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

#' Run a participant-level metric statistical test
#'
#' @param metrics Wide participant/visit-level metric data.
#' @param metric Raw metric column name.
#' @param grouping Grouping column name.
#' @param test_type Either `welch_t` or `wilcoxon`.
#'
#' @return One-row data frame with test result or an insufficiency note.
#' @noRd
run_metric_stat_test <- function(metrics, metric, grouping = "group", test_type = "welch_t") {
  if (!metric %in% names(metrics)) {
    stop("Selected metric is not available.", call. = FALSE)
  }
  if (!grouping %in% names(metrics)) {
    return(insufficient_stat_result(metric, grouping, test_type, note = "Selected grouping variable is not available."))
  }
  if (!test_type %in% c("welch_t", "wilcoxon")) {
    stop("Unsupported test type.", call. = FALSE)
  }

  analysis <- data.frame(
    value = as.numeric(metrics[[metric]]),
    group = as.character(metrics[[grouping]]),
    stringsAsFactors = FALSE
  )
  analysis <- analysis[!is.na(analysis$value) & !is.na(analysis$group) & nzchar(analysis$group), , drop = FALSE]
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
