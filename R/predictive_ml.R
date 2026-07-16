predictive_default_parameters <- function(
  horizon_minutes = 30L,
  target = "low",
  model = "glm",
  min_examples = 80L,
  min_events = 5L,
  threshold = 0.5,
  lag_minutes = c(5L, 15L, 30L, 60L),
  window_minutes = c(15L, 30L, 60L)
) {
  list(
    horizon_minutes = as.integer(horizon_minutes),
    target = as.character(target),
    model = as.character(model),
    min_examples = as.integer(min_examples),
    min_events = as.integer(min_events),
    threshold = as.numeric(threshold),
    lag_minutes = as.integer(lag_minutes),
    window_minutes = as.integer(window_minutes)
  )
}

normalize_predictive_parameters <- function(parameters = predictive_default_parameters()) {
  defaults <- predictive_default_parameters()
  parameters <- modifyList(defaults, parameters %||% list())
  parameters$horizon_minutes <- if (as.integer(parameters$horizon_minutes %||% 30L) >= 45L) 60L else 30L
  parameters$target <- match.arg(tolower(as.character(parameters$target %||% "low")), c("low", "high"))
  parameters$model <- match.arg(tolower(as.character(parameters$model %||% "glm")), c("glm", "ranger"))
  parameters$min_examples <- max(20L, as.integer(parameters$min_examples %||% defaults$min_examples))
  parameters$min_events <- max(1L, as.integer(parameters$min_events %||% defaults$min_events))
  parameters$threshold <- suppressWarnings(as.numeric(parameters$threshold %||% defaults$threshold))
  if (!is.finite(parameters$threshold) || parameters$threshold <= 0 || parameters$threshold >= 1) {
    parameters$threshold <- defaults$threshold
  }
  parameters$lag_minutes <- sort(unique(pmax(1L, as.integer(parameters$lag_minutes %||% defaults$lag_minutes))))
  parameters$window_minutes <- sort(unique(pmax(5L, as.integer(parameters$window_minutes %||% defaults$window_minutes))))
  parameters
}

predictive_target_choices <- function(thresholds = default_cgm_thresholds()) {
  stats::setNames(
    c("low", "high"),
    c(
      paste0("Below range risk (<", thresholds$tir_lower, " mg/dL)"),
      paste0("Above range risk (>", thresholds$tir_upper, " mg/dL)")
    )
  )
}

predictive_model_choices <- function() {
  c("Logistic regression" = "glm", "Random forest" = "ranger")
}

predictive_horizon_choices <- function() {
  c("30 minutes" = 30L, "60 minutes" = 60L)
}

predictive_subject_choices <- function(values) {
  values <- sort(clean_filter_values(values))
  stats::setNames(values, values)
}

predictive_default_subject_selection <- function(values) {
  values <- sort(clean_filter_values(values))
  if (!length(values)) {
    return("")
  }
  values[[1L]]
}

preserve_predictive_subject_selection <- function(selected, values) {
  values <- sort(clean_filter_values(values))
  selected <- normalize_filter_value(selected %||% predictive_default_subject_selection(values))
  if (nzchar(selected) && selected %in% values) {
    selected
  } else {
    predictive_default_subject_selection(values)
  }
}

filter_predictive_data_by_subject <- function(data, subject) {
  subject <- normalize_filter_value(subject)
  if (!is.data.frame(data) || !nrow(data) || !nzchar(subject) || !"id" %in% names(data)) {
    return(data[FALSE, , drop = FALSE])
  }
  data[as.character(data$id) == subject, , drop = FALSE]
}

predictive_ranger_available <- function() {
  requireNamespace("ranger", quietly = TRUE)
}

predictive_proc_available <- function() {
  requireNamespace("pROC", quietly = TRUE)
}

empty_predictive_examples <- function() {
  data.frame(
    id = character(),
    timestamp = as.POSIXct(character(), tz = "UTC"),
    date = as.Date(character()),
    glucose_current = numeric(),
    target = integer(),
    target_value = numeric(),
    target_label = character(),
    stringsAsFactors = FALSE
  )
}

predictive_feature_columns <- function(examples) {
  if (!is.data.frame(examples) || !nrow(examples)) {
    return(character())
  }
  candidates <- c(
    "glucose_current",
    grep("^lag_[0-9]+_glucose$", names(examples), value = TRUE),
    grep("^window_[0-9]+_", names(examples), value = TRUE),
    "time_sin",
    "time_cos"
  )
  candidates[candidates %in% names(examples)]
}

predictive_target_label <- function(parameters, thresholds = default_cgm_thresholds()) {
  parameters <- normalize_predictive_parameters(parameters)
  if (identical(parameters$target, "high")) {
    paste0("Above ", thresholds$tir_upper, " mg/dL within ", parameters$horizon_minutes, " minutes")
  } else {
    paste0("Below ", thresholds$tir_lower, " mg/dL within ", parameters$horizon_minutes, " minutes")
  }
}

predictive_event_label <- function(parameters = predictive_default_parameters()) {
  parameters <- normalize_predictive_parameters(parameters)
  if (identical(parameters$target, "high")) {
    "Observed future above-range event"
  } else {
    "Observed future below-range event"
  }
}

predictive_row_windows <- function(time_minutes, glucose, window_minutes) {
  n <- length(glucose)
  out <- data.frame(row.names = seq_len(n))
  for (window in window_minutes) {
    lower <- findInterval(time_minutes - window, time_minutes)
    mean_values <- sd_values <- min_values <- max_values <- slope_values <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      start <- max(1L, lower[[i]] + 1L)
      idx <- start:i
      values <- glucose[idx]
      times <- time_minutes[idx]
      finite <- is.finite(values) & is.finite(times)
      if (!any(finite)) {
        next
      }
      values <- values[finite]
      times <- times[finite]
      mean_values[[i]] <- mean(values)
      sd_values[[i]] <- if (length(values) > 1L) stats::sd(values) else 0
      min_values[[i]] <- min(values)
      max_values[[i]] <- max(values)
      if (length(values) > 1L && max(times) > min(times)) {
        slope_values[[i]] <- (values[[length(values)]] - values[[1L]]) / ((max(times) - min(times)) / 60)
      }
    }
    prefix <- paste0("window_", window, "_")
    out[[paste0(prefix, "mean")]] <- mean_values
    out[[paste0(prefix, "sd")]] <- sd_values
    out[[paste0(prefix, "min")]] <- min_values
    out[[paste0(prefix, "max")]] <- max_values
    out[[paste0(prefix, "slope")]] <- slope_values
  }
  out
}

predictive_subject_examples <- function(subject_data, parameters, thresholds) {
  subject_data <- subject_data[order(subject_data$timestamp), , drop = FALSE]
  subject_data <- subject_data[is_finite_cgm_timestamp(subject_data$timestamp) & is.finite(subject_data$glucose), , drop = FALSE]
  if (nrow(subject_data) < 3L) {
    return(empty_predictive_examples())
  }
  time_minutes <- as.numeric(subject_data$timestamp) / 60
  glucose <- as.numeric(subject_data$glucose)
  n <- length(glucose)
  examples <- data.frame(
    id = as.character(subject_data$id),
    timestamp = as.POSIXct(subject_data$timestamp, origin = "1970-01-01", tz = "UTC"),
    date = as.Date(subject_data$timestamp),
    glucose_current = glucose,
    stringsAsFactors = FALSE
  )
  for (lag in parameters$lag_minutes) {
    examples[[paste0("lag_", lag, "_glucose")]] <- as.numeric(stats::approx(
      x = time_minutes,
      y = glucose,
      xout = time_minutes - lag,
      rule = 1,
      ties = "ordered"
    )$y)
  }
  examples <- cbind(examples, predictive_row_windows(time_minutes, glucose, parameters$window_minutes))
  minute_of_day <- time_window_minutes(examples$timestamp)
  examples$time_sin <- sin(2 * pi * minute_of_day / 1440)
  examples$time_cos <- cos(2 * pi * minute_of_day / 1440)

  future_value <- future_count <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    future <- which(time_minutes > time_minutes[[i]] & time_minutes <= time_minutes[[i]] + parameters$horizon_minutes)
    if (!length(future)) {
      next
    }
    values <- glucose[future]
    future_count[[i]] <- sum(is.finite(values))
    if (identical(parameters$target, "high")) {
      future_value[[i]] <- if (any(is.finite(values))) max(values, na.rm = TRUE) else NA_real_
    } else {
      future_value[[i]] <- if (any(is.finite(values))) min(values, na.rm = TRUE) else NA_real_
    }
  }
  examples$future_points <- as.integer(future_count)
  examples$target_value <- future_value
  examples$target <- if (identical(parameters$target, "high")) {
    as.integer(is.finite(future_value) & future_value > thresholds$tir_upper)
  } else {
    as.integer(is.finite(future_value) & future_value < thresholds$tir_lower)
  }
  examples$target_label <- predictive_target_label(parameters, thresholds)
  examples[is.finite(examples$target_value) & time_minutes <= max(time_minutes) - parameters$horizon_minutes, , drop = FALSE]
}

build_predictive_examples <- function(data, parameters = predictive_default_parameters(), thresholds = default_cgm_thresholds()) {
  parameters <- normalize_predictive_parameters(parameters)
  if (!is.data.frame(data) || !all(c("id", "timestamp", "glucose") %in% names(data))) {
    return(empty_predictive_examples())
  }
  data <- data[!is.na(data$id) & nzchar(as.character(data$id)), , drop = FALSE]
  ids <- subject_id_values(data)
  if (!length(ids)) {
    return(empty_predictive_examples())
  }
  rows <- lapply(ids, function(subject_id) {
    predictive_subject_examples(
      data[as.character(data$id) == subject_id, , drop = FALSE],
      parameters,
      thresholds
    )
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  feature_cols <- predictive_feature_columns(out)
  if (length(feature_cols)) {
    out <- out[stats::complete.cases(out[, feature_cols, drop = FALSE]) & !is.na(out$target), , drop = FALSE]
  }
  row.names(out) <- NULL
  out
}

predictive_split_examples <- function(examples) {
  if (!is.data.frame(examples) || !nrow(examples)) {
    return(examples)
  }
  out <- examples
  out$split <- "train"
  ids <- sort(unique(as.character(out$id)))
  if (length(ids) >= 2L) {
    set.seed(1)
    test_n <- max(1L, ceiling(length(ids) * 0.30))
    test_ids <- sort(sample(ids, test_n))
    out$split[as.character(out$id) %in% test_ids] <- "test"
    return(out)
  }
  dates <- sort(unique(as.Date(out$date)))
  if (length(dates) >= 2L) {
    test_n <- max(1L, ceiling(length(dates) * 0.30))
    test_dates <- utils::tail(dates, test_n)
    out$split[as.Date(out$date) %in% test_dates] <- "test"
    return(out)
  }
  ord <- order(out$timestamp)
  test_n <- max(1L, floor(nrow(out) * 0.30))
  out$split[ord[seq.int(nrow(out) - test_n + 1L, nrow(out))]] <- "test"
  out
}

predictive_model_formula <- function(feature_cols) {
  stats::as.formula(paste("target_factor ~", paste(feature_cols, collapse = " + ")))
}

fit_predictive_model <- function(examples, parameters = predictive_default_parameters()) {
  parameters <- normalize_predictive_parameters(parameters)
  feature_cols <- predictive_feature_columns(examples)
  train <- examples[examples$split == "train", , drop = FALSE]
  if (nrow(examples) < parameters$min_examples) {
    return(list(status = "insufficient_examples", message = paste("Needs at least", parameters$min_examples, "examples.")))
  }
  if (!length(feature_cols) || !nrow(train)) {
    return(list(status = "insufficient_features", message = "No complete predictive feature rows are available."))
  }
  event_count <- sum(train$target == 1L, na.rm = TRUE)
  non_event_count <- sum(train$target == 0L, na.rm = TRUE)
  if (event_count < parameters$min_events || non_event_count < parameters$min_events) {
    return(list(
      status = "insufficient_events",
      message = paste("Training data needs at least", parameters$min_events, "events and non-events.")
    ))
  }
  train$target_factor <- factor(train$target, levels = c(0, 1))
  formula <- predictive_model_formula(feature_cols)
  if (identical(parameters$model, "ranger")) {
    if (!predictive_ranger_available()) {
      return(list(status = "package_unavailable", message = "The ranger package is not available for random forest modeling."))
    }
    model <- ranger::ranger(
      formula,
      data = train[, c("target_factor", feature_cols), drop = FALSE],
      probability = TRUE,
      num.trees = 200,
      importance = "impurity",
      seed = 1
    )
  } else {
    model <- suppressWarnings(stats::glm(
      formula,
      data = train[, c("target_factor", feature_cols), drop = FALSE],
      family = stats::binomial()
    ))
  }
  list(
    status = "ready",
    message = "Predictive model trained for retrospective risk scoring.",
    model = model,
    feature_cols = feature_cols,
    model_type = parameters$model
  )
}

predict_predictive_probabilities <- function(fit, examples) {
  if (!identical(fit$status, "ready") || !is.data.frame(examples) || !nrow(examples)) {
    return(rep(NA_real_, nrow(examples %||% data.frame())))
  }
  rows <- examples[, fit$feature_cols, drop = FALSE]
  if (identical(fit$model_type, "ranger")) {
    pred <- stats::predict(fit$model, data = rows)$predictions
    if (is.matrix(pred) && "1" %in% colnames(pred)) {
      return(as.numeric(pred[, "1"]))
    }
    return(rep(NA_real_, nrow(rows)))
  }
  as.numeric(stats::predict(fit$model, newdata = rows, type = "response"))
}

predictive_performance <- function(scores, threshold = 0.5) {
  if (!is.data.frame(scores) || !nrow(scores)) {
    return(data.frame())
  }
  test <- scores[scores$split == "test" & is.finite(scores$risk_probability) & !is.na(scores$target), , drop = FALSE]
  if (!nrow(test)) {
    return(data.frame())
  }
  actual <- as.integer(test$target)
  predicted <- as.integer(test$risk_probability >= threshold)
  tp <- sum(actual == 1L & predicted == 1L)
  tn <- sum(actual == 0L & predicted == 0L)
  fp <- sum(actual == 0L & predicted == 1L)
  fn <- sum(actual == 1L & predicted == 0L)
  auc <- NA_real_
  if (length(unique(actual)) == 2L && predictive_proc_available()) {
    auc <- tryCatch(
      as.numeric(pROC::auc(pROC::roc(actual, test$risk_probability, quiet = TRUE))),
      error = function(error) NA_real_
    )
  }
  data.frame(
    test_rows = nrow(test),
    test_events = sum(actual == 1L),
    auc = auc,
    sensitivity = if ((tp + fn) > 0L) tp / (tp + fn) else NA_real_,
    specificity = if ((tn + fp) > 0L) tn / (tn + fp) else NA_real_,
    brier_score = mean((test$risk_probability - actual)^2),
    threshold = threshold,
    stringsAsFactors = FALSE
  )
}

predictive_feature_importance <- function(fit) {
  if (!identical(fit$status, "ready")) {
    return(data.frame(feature = character(), importance = numeric(), stringsAsFactors = FALSE))
  }
  if (identical(fit$model_type, "ranger") && !is.null(fit$model$variable.importance)) {
    values <- fit$model$variable.importance
    out <- data.frame(feature = names(values), importance = as.numeric(values), stringsAsFactors = FALSE)
  } else {
    coefs <- stats::coef(fit$model)
    coefs <- coefs[setdiff(names(coefs), "(Intercept)")]
    out <- data.frame(feature = names(coefs), importance = abs(as.numeric(coefs)), stringsAsFactors = FALSE)
  }
  out <- out[order(out$importance, decreasing = TRUE), , drop = FALSE]
  row.names(out) <- NULL
  out
}

predictive_feature_label <- function(feature) {
  feature <- as.character(feature)
  out <- feature
  out[feature == "glucose_current"] <- "Current glucose"
  out[feature == "time_sin"] <- "Time of day pattern (sine)"
  out[feature == "time_cos"] <- "Time of day pattern (cosine)"

  lag_match <- regexec("^lag_([0-9]+)_glucose$", feature)
  lag_parts <- regmatches(feature, lag_match)
  for (i in seq_along(lag_parts)) {
    if (length(lag_parts[[i]]) == 2L) {
      out[[i]] <- paste0("Glucose ", lag_parts[[i]][[2L]], " min ago")
    }
  }

  window_match <- regexec("^window_([0-9]+)_(mean|sd|min|max|slope)$", feature)
  window_parts <- regmatches(feature, window_match)
  labels <- c(
    mean = "Mean glucose over previous %s min",
    sd = "Glucose variability over previous %s min",
    min = "Minimum glucose over previous %s min",
    max = "Maximum glucose over previous %s min",
    slope = "Glucose trend over previous %s min"
  )
  for (i in seq_along(window_parts)) {
    if (length(window_parts[[i]]) == 3L) {
      minutes <- window_parts[[i]][[2L]]
      statistic <- window_parts[[i]][[3L]]
      out[[i]] <- sprintf(labels[[statistic]], minutes)
    }
  }
  out
}

prepare_predictive_feature_importance_display <- function(importance) {
  if (!is.data.frame(importance) || !nrow(importance)) {
    return(data.frame(Feature = character(), `Model input` = character(), Importance = numeric(), check.names = FALSE))
  }
  data.frame(
    Feature = predictive_feature_label(importance$feature),
    `Model input` = importance$feature,
    Importance = round(importance$importance, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

compute_predictive_risk_bundle <- function(
  data,
  parameters = predictive_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  parameters <- normalize_predictive_parameters(parameters)
  source_ids <- subject_id_values(data)
  subject_id <- if (length(source_ids) == 1L) source_ids[[1L]] else NA_character_
  examples <- build_predictive_examples(data, parameters = parameters, thresholds = thresholds)
  examples <- predictive_split_examples(examples)
  fit <- fit_predictive_model(examples, parameters = parameters)
  scores <- examples
  scores$risk_probability <- predict_predictive_probabilities(fit, examples)
  performance <- predictive_performance(scores, threshold = parameters$threshold)
  importance <- predictive_feature_importance(fit)
  list(
    parameters = parameters,
    examples = examples,
    scores = scores,
    performance = performance,
    importance = importance,
    fit_status = fit$status %||% "unknown",
    fit_message = fit$message %||% "",
    model_type = parameters$model,
    target_label = predictive_target_label(parameters, thresholds),
    subject_id = subject_id
  )
}

predictive_summary_cards <- function(bundle) {
  scores <- bundle$scores
  data.frame(
    Label = c("Status", "Examples", "Test events", "AUC", "Brier"),
    Value = c(
      predictive_status_label(bundle$fit_status),
      format_count(nrow(scores %||% data.frame())),
      if (is.data.frame(bundle$performance) && nrow(bundle$performance)) format_count(bundle$performance$test_events[[1L]]) else "Not available",
      if (is.data.frame(bundle$performance) && nrow(bundle$performance) && is.finite(bundle$performance$auc[[1L]])) round(bundle$performance$auc[[1L]], 3) else "Not available",
      if (is.data.frame(bundle$performance) && nrow(bundle$performance) && is.finite(bundle$performance$brier_score[[1L]])) round(bundle$performance$brier_score[[1L]], 3) else "Not available"
    ),
    stringsAsFactors = FALSE
  )
}

predictive_status_label <- function(status) {
  labels <- c(
    ready = "Ready",
    insufficient_examples = "Needs more rows",
    insufficient_features = "Needs features",
    insufficient_events = "Needs events",
    package_unavailable = "Package unavailable"
  )
  labels[[status %||% ""]] %||% "Not ready"
}

predictive_status_ui_message <- function(bundle) {
  if (identical(bundle$fit_status, "ready")) {
    return("")
  }
  bundle$fit_message %||% "Predictive Risk is not ready for the current selection."
}

prepare_predictive_performance_display <- function(bundle) {
  perf <- bundle$performance
  if (!is.data.frame(perf) || !nrow(perf)) {
    return(data.frame())
  }
  out <- data.frame(
    `Subject ID` = bundle$subject_id %||% NA_character_,
    Model = predictive_model_label(bundle$model_type),
    Target = bundle$target_label,
    Horizon = paste0(bundle$parameters$horizon_minutes, " minutes"),
    `Test rows` = perf$test_rows,
    `Test events` = perf$test_events,
    AUC = round(perf$auc, 3),
    Sensitivity = round(perf$sensitivity, 3),
    Specificity = round(perf$specificity, 3),
    `Brier score` = round(perf$brier_score, 3),
    Threshold = round(perf$threshold, 2),
    check.names = FALSE
  )
  out
}

predictive_model_label <- function(model) {
  labels <- c(glm = "Logistic regression", ranger = "Random forest")
  labels[[model %||% ""]] %||% model
}

prepare_predictive_scores_export <- function(bundle) {
  scores <- bundle$scores
  if (!is.data.frame(scores) || !nrow(scores)) {
    return(data.frame())
  }
  out <- scores[, intersect(c(
    "id", "timestamp", "date", "split", "glucose_current", "target", "target_value",
    "risk_probability", "target_label"
  ), names(scores)), drop = FALSE]
  names(out) <- c(
    id = "Subject ID",
    timestamp = "Timestamp",
    date = "Date",
    split = "Split",
    glucose_current = "Current glucose",
    target = "Observed event",
    target_value = "Future glucose summary",
    risk_probability = "Risk probability",
    target_label = "Target"
  )[names(out)]
  out$Model <- predictive_model_label(bundle$model_type)
  out$Horizon <- paste0(bundle$parameters$horizon_minutes, " minutes")
  out
}

prepare_predictive_performance_export <- function(bundle) {
  prepare_predictive_performance_display(bundle)
}

prepare_predictive_feature_importance_export <- function(bundle) {
  display <- prepare_predictive_feature_importance_display(bundle$importance)
  if (!is.data.frame(display) || !nrow(display)) {
    return(data.frame())
  }
  display$`Subject ID` <- bundle$subject_id %||% NA_character_
  display$Model <- predictive_model_label(bundle$model_type)
  display$Target <- bundle$target_label
  display$Horizon <- paste0(bundle$parameters$horizon_minutes, " minutes")
  display[, c("Subject ID", "Model", "Target", "Horizon", "Feature", "Model input", "Importance"), drop = FALSE]
}

predictive_risk_date_axis <- function(timestamps) {
  timestamps <- timestamps[is_finite_cgm_timestamp(timestamps)]
  empty <- list(
    breaks = as.POSIXct(character(), tz = "UTC"),
    labels = character(),
    rotate = FALSE,
    date_count = 0L,
    interval = "none"
  )
  if (!length(timestamps)) {
    return(empty)
  }
  min_date <- as.Date(min(timestamps), tz = "UTC")
  max_date <- as.Date(max(timestamps), tz = "UTC")
  if (is.na(min_date) || is.na(max_date)) {
    return(empty)
  }
  date_count <- as.integer(max_date - min_date) + 1L
  interval <- if (date_count <= 14L) {
    "day"
  } else if (date_count <= 31L) {
    "2 days"
  } else {
    "week"
  }
  dates <- seq(min_date, max_date, by = interval)
  crosses_year <- length(unique(format(dates, "%Y"))) > 1L
  labels <- if (crosses_year) {
    format(dates, "%b %d\n%Y")
  } else {
    format(dates, "%b %d")
  }
  list(
    breaks = as.POSIXct(dates, tz = "UTC"),
    labels = labels,
    rotate = date_count > 7L,
    date_count = date_count,
    interval = interval
  )
}

create_predictive_risk_plot <- function(
  scores,
  subject = "",
  target_label = "",
  threshold = 0.5,
  event_label = "Observed future target event",
  show_legend = FALSE
) {
  if (!is.data.frame(scores) || !nrow(scores)) {
    return(empty_plot("No predictive risk scores are available."))
  }
  subject <- normalize_filter_value(subject)
  rows <- scores
  if (nzchar(subject)) {
    rows <- rows[as.character(rows$id) == subject, , drop = FALSE]
  }
  rows <- rows[is.finite(rows$risk_probability) & is_finite_cgm_timestamp(rows$timestamp), , drop = FALSE]
  if (!nrow(rows)) {
    return(empty_plot("No predictive risk scores are available for the selected Subject ID."))
  }
  threshold <- suppressWarnings(as.numeric(threshold %||% 0.5))
  if (!is.finite(threshold) || threshold <= 0 || threshold >= 1) {
    threshold <- 0.5
  }
  elevated_floor <- if (threshold <= 0.25) max(0.01, threshold / 2) else 0.25
  rows$risk_band <- cut(
    rows$risk_probability,
    breaks = c(-Inf, elevated_floor, threshold, Inf),
    labels = c("Low risk", "Elevated risk", "High risk"),
    include.lowest = TRUE
  )
  rows$current_glucose_label <- ifelse(
    is.finite(rows$glucose_current),
    paste0(round(rows$glucose_current, 1), " mg/dL"),
    "Not available"
  )
  rows$hover <- paste0(
    "Subject ID: ", rows$id,
    "<br>Time: ", format(rows$timestamp, "%Y-%m-%d %H:%M"),
    "<br>Target: ", rows$target_label,
    "<br>Risk probability: ", round(rows$risk_probability * 100, 1), "%",
    "<br>Current glucose: ", rows$current_glucose_label,
    "<br>", event_label, ": ", ifelse(rows$target == 1L, "yes", "no"),
    "<br>Split: ", rows$split
  )
  bands <- data.frame(
    xmin = min(rows$timestamp, na.rm = TRUE),
    xmax = max(rows$timestamp, na.rm = TRUE),
    ymin = c(0, elevated_floor, threshold),
    ymax = c(elevated_floor, threshold, 1),
    band = factor(c("Low risk", "Elevated risk", "High risk"), levels = c("Low risk", "Elevated risk", "High risk"))
  )
  events <- rows[rows$target == 1L, , drop = FALSE]
  date_axis <- predictive_risk_date_axis(rows$timestamp)
  plot <- ggplot2::ggplot(rows, ggplot2::aes(x = timestamp, y = risk_probability, text = hover)) +
    ggplot2::geom_rect(
      data = bands,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = band),
      inherit.aes = FALSE,
      alpha = 0.12,
      show.legend = FALSE
    ) +
    ggplot2::geom_hline(
      data = data.frame(yintercept = threshold, label = "Risk threshold", stringsAsFactors = FALSE),
      ggplot2::aes(yintercept = yintercept, linetype = label),
      inherit.aes = FALSE,
      linewidth = 0.55,
      color = "#374151"
    ) +
    suppressWarnings(ggplot2::geom_line(ggplot2::aes(group = 1), color = "#355C7D", linewidth = 0.55, alpha = 0.55, na.rm = TRUE)) +
    suppressWarnings(ggplot2::geom_point(ggplot2::aes(color = risk_band), size = 1.25, alpha = 0.72, na.rm = TRUE)) +
    suppressWarnings(ggplot2::geom_point(
      data = events,
      ggplot2::aes(x = timestamp, y = pmin(risk_probability + 0.045, 1), shape = event_label, text = hover),
      inherit.aes = FALSE,
      size = 2.1,
      fill = "#B91C1C",
      color = "#7F1D1D",
      alpha = 0.9,
      na.rm = TRUE
    )) +
    ggplot2::scale_x_datetime(
      breaks = date_axis$breaks,
      labels = date_axis$labels,
      expand = ggplot2::expansion(mult = c(0.01, 0.01)),
      guide = ggplot2::guide_axis(check.overlap = !isTRUE(show_legend))
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(x * 100), "%")) +
    ggplot2::scale_color_manual(
      values = c("Low risk" = "#3B82A0", "Elevated risk" = "#D97706", "High risk" = "#DC2626"),
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c("Low risk" = "#3B82A0", "Elevated risk" = "#D97706", "High risk" = "#DC2626"),
      drop = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Risk threshold" = "dashed"),
      name = NULL
    ) +
    ggplot2::scale_shape_manual(
      values = stats::setNames(24, event_label),
      name = NULL
    ) +
    ggplot2::labs(
      x = "Timestamp",
      y = "Risk probability",
      color = "Predicted risk",
      fill = "Risk band",
      title = target_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = if (isTRUE(show_legend)) "bottom" else "none",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 9),
      legend.box = "horizontal",
      axis.text.x = if (isTRUE(show_legend) && isTRUE(date_axis$rotate)) ggplot2::element_text(angle = 35, hjust = 1) else ggplot2::element_text(),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      plot.margin = ggplot2::margin(8, 12, 12, 12)
    )
  if (!isTRUE(show_legend)) {
    plot <- plot + ggplot2::guides(fill = "none", color = "none", linetype = "none", shape = "none")
  } else {
    plot <- plot + ggplot2::guides(
      fill = "none",
      color = ggplot2::guide_legend(order = 1, override.aes = list(size = 2.2, alpha = 0.9)),
      linetype = ggplot2::guide_legend(order = 2),
      shape = ggplot2::guide_legend(order = 3, override.aes = list(fill = "#B91C1C", color = "#7F1D1D", size = 2.8))
    )
  }
  plot
}

create_predictive_importance_plot <- function(importance) {
  if (!is.data.frame(importance) || !nrow(importance)) {
    return(empty_plot("No feature importance values are available."))
  }
  rows <- utils::head(importance, 12L)
  rows$feature_label <- predictive_feature_label(rows$feature)
  rows$feature_label <- stats::reorder(rows$feature_label, rows$importance)
  rows$hover <- paste0(
    "Feature: ", predictive_feature_label(rows$feature),
    "<br>Model input: ", rows$feature,
    "<br>Importance: ", round(rows$importance, 4)
  )
  ggplot2::ggplot(rows, ggplot2::aes(x = feature_label, y = importance, text = hover)) +
    ggplot2::geom_col(fill = "#3B82A0") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Importance") +
    ggplot2::theme_minimal(base_size = 12)
}
