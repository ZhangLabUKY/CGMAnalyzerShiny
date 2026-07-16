export_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "cgm-export-dashboard",
    shiny::div(
      class = "cgm-export-overview",
      shiny::h3("Export")
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-export-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Data")
      ),
      shiny::div(
        class = "cgm-dashboard-section-body",
        shiny::uiOutput(ns("analysis_download_ui"))
      )
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-export-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Results")
      ),
      shiny::div(
        class = "cgm-dashboard-section-body",
        shiny::downloadButton(ns("download_metrics"), "Metrics"),
        shiny::downloadButton(ns("download_complexity_metrics"), "Complexity metrics"),
        shiny::downloadButton(ns("download_functional_features"), "Glycemic pattern features"),
        shiny::downloadButton(ns("download_functional_curves"), "Glycemic pattern curves"),
        shiny::downloadButton(ns("download_predictive_scores"), "Predictive risk scores"),
        shiny::downloadButton(ns("download_predictive_performance"), "Predictive model performance"),
        shiny::downloadButton(ns("download_predictive_importance"), "Predictive feature importance")
      )
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-export-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Predictive Risk export settings")
      ),
      shiny::div(
        class = "cgm-dashboard-section-body",
        shiny::div(
          class = "predictive-control-grid",
          shiny::selectInput(ns("predictive_target"), "Risk target", choices = predictive_target_choices(), selected = "low"),
          shiny::selectInput(ns("predictive_horizon_minutes"), "Future window", choices = predictive_horizon_choices(), selected = 30),
          shiny::selectInput(ns("predictive_model"), "Model", choices = predictive_model_choices(), selected = "glm"),
          shiny::numericInput(ns("predictive_min_examples"), "Minimum examples", value = 80, min = 20, step = 10),
          shiny::numericInput(ns("predictive_min_events"), "Minimum events", value = 5, min = 1, step = 1),
          shiny::numericInput(ns("predictive_threshold"), "Risk threshold", value = 0.5, min = 0.05, max = 0.95, step = 0.05)
        )
      )
    ),
    shiny::tags$section(
      class = "cgm-dashboard-section cgm-export-section",
      shiny::div(
        class = "cgm-dashboard-section-header",
        shiny::h4("Figures")
      ),
      shiny::div(
        class = "cgm-dashboard-section-body",
        shiny::radioButtons(
          ns("plot_format"),
          "Plot format",
          choices = export_plot_format_choices(),
          selected = "png",
          inline = TRUE
        ),
        shiny::downloadButton(ns("download_plots"), "Download plots"),
        shiny::downloadButton(ns("download_complexity_plots"), "Download complexity plots"),
        shiny::downloadButton(ns("download_predictive_plots"), "Download Predictive Risk plots")
      )
    )
  )
}

subject_cache_status_rows <- function(data, generated_ids, output = "Cache status", metric = "Cache status", definition = "Selected-on-demand cache coverage at export time.") {
  ids <- subject_id_values(data)
  generated_ids <- clean_filter_values(generated_ids)
  if (!length(ids)) {
    return(data.frame())
  }
  out <- data.frame(
    `Subject ID` = ids,
    Output = output,
    Metric = metric,
    Value = ifelse(ids %in% generated_ids, "Generated", "Not generated"),
    Definition = definition,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (is.data.frame(data) && "group" %in% names(data)) {
    group_lookup <- stats::setNames(as.character(data$group), as.character(data$id))
    out$Group <- unname(group_lookup[out[["Subject ID"]]])
    out <- out[, c("Subject ID", "Group", setdiff(names(out), c("Subject ID", "Group"))), drop = FALSE]
  }
  out
}

prepare_metrics_cached_export <- function(metrics, data, thresholds = default_cgm_thresholds()) {
  display <- prepare_metrics_display(metrics, thresholds = thresholds, show_subject_id = TRUE)
  generated_ids <- if (is.data.frame(metrics) && "id" %in% names(metrics)) unique(as.character(metrics$id)) else character()
  status <- subject_cache_status_rows(
    data,
    generated_ids,
    output = "Cache status",
    metric = "Metrics cache status",
    definition = "Selected-on-demand Metrics cache coverage at export time."
  )
  if (nrow(status)) {
    status$Period <- ""
    status$Category <- "Cache status"
    status$Units <- ""
    status <- status[, c(intersect(c("Subject ID", "Group"), names(status)), "Period", "Category", "Metric", "Definition", "Value", "Units"), drop = FALSE]
  }
  out <- data.table::rbindlist(list(display, status), fill = TRUE)
  as.data.frame(out, stringsAsFactors = FALSE)
}

prepare_complete_metrics_export <- function(data, thresholds = default_cgm_thresholds()) {
  metrics <- cgm_suppress_non_cgma_messages(
    compute_core_metrics(data, thresholds = thresholds, by = default_metric_groups(data))
  )
  prepare_metrics_display(metrics, thresholds = thresholds, show_subject_id = TRUE)
}

compute_complete_complexity_export_bundle <- function(data, parameters = complexity_default_parameters()) {
  cgm_suppress_non_cgma_messages(
    compute_complexity_bundle(data, parameters, include_mse = TRUE)
  )
}

add_complexity_time_basis <- function(export) {
  export$`Time basis` <- if (nrow(export)) "Full dataset" else character()
  leading <- intersect(c("Subject ID", "Group"), names(export))
  export[, c(leading, "Time basis", setdiff(names(export), c(leading, "Time basis"))), drop = FALSE]
}

prepare_complete_complexity_export <- function(data, parameters = complexity_default_parameters()) {
  bundle <- compute_complete_complexity_export_bundle(data, parameters)
  export <- prepare_complexity_export(
    bundle$metrics,
    bundle$curves,
    data,
    show_subject_id = TRUE
  )
  add_complexity_time_basis(export)
}

compute_complete_functional_profile_bundle <- function(
  data,
  parameters = functional_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  cgm_suppress_non_cgma_messages(
    compute_functional_profile_bundle(data, parameters = parameters, thresholds = thresholds)
  )
}

prepare_complete_functional_features_export <- function(
  data,
  parameters = functional_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  bundle <- compute_complete_functional_profile_bundle(data, parameters = parameters, thresholds = thresholds)
  prepare_functional_features_export(bundle, data = data)
}

prepare_complete_functional_curves_export <- function(
  data,
  parameters = functional_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  bundle <- compute_complete_functional_profile_bundle(data, parameters = parameters, thresholds = thresholds)
  prepare_functional_curves_export(bundle)
}

compute_complete_predictive_risk_bundle <- function(
  data,
  parameters = predictive_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  cgm_suppress_non_cgma_messages(
    compute_predictive_risk_bundle(data, parameters = parameters, thresholds = thresholds)
  )
}

compute_complete_predictive_risk_bundles <- function(
  data,
  parameters = predictive_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  ids <- subject_id_values(data)
  if (!length(ids)) {
    return(list())
  }
  stats::setNames(
    lapply(ids, function(subject_id) {
      compute_complete_predictive_risk_bundle(
        filter_predictive_data_by_subject(data, subject_id),
        parameters = parameters,
        thresholds = thresholds
      )
    }),
    ids
  )
}

prepare_complete_predictive_scores_export <- function(
  data,
  parameters = predictive_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  bundles <- compute_complete_predictive_risk_bundles(data, parameters = parameters, thresholds = thresholds)
  rows <- lapply(bundles, prepare_predictive_scores_export)
  if (!length(rows)) {
    return(data.frame())
  }
  as.data.frame(data.table::rbindlist(rows, fill = TRUE), stringsAsFactors = FALSE)
}

prepare_complete_predictive_performance_export <- function(
  data,
  parameters = predictive_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  bundles <- compute_complete_predictive_risk_bundles(data, parameters = parameters, thresholds = thresholds)
  rows <- lapply(bundles, prepare_predictive_performance_export)
  if (!length(rows)) {
    return(data.frame())
  }
  as.data.frame(data.table::rbindlist(rows, fill = TRUE), stringsAsFactors = FALSE)
}

prepare_complete_predictive_feature_importance_export <- function(
  data,
  parameters = predictive_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  bundles <- compute_complete_predictive_risk_bundles(data, parameters = parameters, thresholds = thresholds)
  rows <- lapply(bundles, prepare_predictive_feature_importance_export)
  if (!length(rows)) {
    return(data.frame())
  }
  as.data.frame(data.table::rbindlist(rows, fill = TRUE), stringsAsFactors = FALSE)
}

prepare_complexity_cached_export <- function(results, curves, data, generated_ids, show_subject_id = NULL) {
  export <- if (is.data.frame(results) && nrow(results)) {
    prepare_complexity_export(results, curves, data, show_subject_id = show_subject_id)
  } else {
    data.frame()
  }
  status <- subject_cache_status_rows(
    data,
    generated_ids,
    output = "Cache status",
    metric = "Complexity cache status",
    definition = "Selected-on-demand Complexity cache coverage at export time."
  )
  if (nrow(status)) {
    status$`Units / scale` <- ""
    status$`Scale / parameters` <- ""
    status$`Scale variable` <- ""
    status$`Scale value` <- NA_real_
    status$`Derived scalar` <- ""
    status$`Derived scalar value` <- NA_real_
    status$Notes <- ""
    status <- status[, c(
      intersect(c("Subject ID", "Group"), names(status)),
      "Output",
      "Metric",
      "Value",
      "Units / scale",
      "Scale / parameters",
      "Scale variable",
      "Scale value",
      "Derived scalar",
      "Derived scalar value",
      "Definition",
      "Notes"
    ), drop = FALSE]
  }
  out <- data.table::rbindlist(list(export, status), fill = TRUE)
  as.data.frame(out, stringsAsFactors = FALSE)
}

export_plot_format_choices <- function() {
  c("PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff", "PNG" = "png", "JPEG" = "jpeg")
}

normalize_export_plot_format <- function(format) {
  values <- tolower(trimws(as.character(format %||% character())))
  aliases <- c(jpg = "jpeg", tif = "tiff")
  values <- vapply(values, function(value) {
    if (value %in% names(aliases)) aliases[[value]] else value
  }, character(1), USE.NAMES = FALSE)
  values <- values[values %in% unname(export_plot_format_choices())]
  if (!length(values)) {
    return("png")
  }
  values[[1L]]
}

normalize_export_plot_formats <- function(formats) {
  normalize_export_plot_format(formats)
}

export_plot_types <- function() {
  c("trace", "daily_overlay", "agp")
}

export_plot_type_filename_label <- function(plot_type = "trace") {
  switch(
    plot_type %||% "trace",
    daily_overlay = "daily-overlay",
    agp = "agp",
    trace = "trace",
    sanitize_plot_filename_part(plot_type)
  )
}

export_plot_file_stem <- function(subject_id, plot_type, time_window = default_time_window()) {
  parts <- c(
    "cgm",
    plot_subject_filename_label(subject_id),
    export_plot_type_filename_label(plot_type),
    if (identical(plot_type, "daily_overlay")) "all-days" else character(),
    gsub("^time-", "", time_window_filename_label(time_window))
  )
  paste(parts[nzchar(parts)], collapse = "_")
}

export_plots_pdf_filename <- function(time_window = default_time_window()) {
  paste0("cgm_plots_per-subject_", gsub("^time-", "", time_window_filename_label(time_window)), ".pdf")
}

export_plots_zip_filename <- function(time_window = default_time_window()) {
  paste0("cgm_plots_per-subject_", gsub("^time-", "", time_window_filename_label(time_window)), ".zip")
}

export_plot_manifest <- function(data, formats = "png", time_window = default_time_window()) {
  format <- normalize_export_plot_format(formats)
  ids <- subject_id_values(data)
  rows <- list()
  if (!identical(format, "pdf") && length(ids)) {
    rows <- lapply(ids, function(subject_id) {
      do.call(rbind, lapply(export_plot_types(), function(plot_type) {
        data.frame(
          subject_id = subject_id,
          plot_type = plot_type,
          format = format,
          filename = paste0(export_plot_file_stem(subject_id, plot_type, time_window), ".", format),
          stringsAsFactors = FALSE
        )
      }))
    })
  }
  manifest <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      subject_id = character(),
      plot_type = character(),
      format = character(),
      filename = character(),
      stringsAsFactors = FALSE
    )
  }
  row.names(manifest) <- NULL
  manifest
}

export_plot_for_subject <- function(data, subject_id, plot_type, thresholds = default_cgm_thresholds(), time_window = default_time_window()) {
  switch(
    plot_type,
    trace = create_trace_plot(
      data,
      thresholds = thresholds,
      participant_id = subject_id,
      time_window = time_window,
      max_points_per_participant = Inf
    ),
    daily_overlay = create_daily_overlay_plot(
      data,
      thresholds = thresholds,
      participant = subject_id,
      day = all_filter_value(),
      time_window = time_window,
      max_points_per_participant = Inf
    ),
    agp = create_agp_summary_plot(
      data,
      thresholds = thresholds,
      participant = subject_id,
      time_window = time_window
    ),
    stop("Unknown plot type: ", plot_type, call. = FALSE)
  )
}

write_export_plot_image <- function(plot, file, format, width = 10, height = 6, dpi = 300) {
  format <- normalize_export_plot_format(format)
  device <- switch(
    format,
    png = grDevices::png,
    jpeg = grDevices::jpeg,
    tiff = grDevices::tiff,
    svg = grDevices::svg,
    stop("Unsupported image format: ", format, call. = FALSE)
  )
  if (identical(format, "tiff")) {
    return(ggplot2::ggsave(file, plot = plot, device = device, width = width, height = height, dpi = dpi, bg = "white", compression = "lzw"))
  }
  ggplot2::ggsave(file, plot = plot, device = device, width = width, height = height, dpi = dpi, bg = "white")
}

write_export_plots_pdf <- function(data, file, thresholds = default_cgm_thresholds(), time_window = default_time_window(), width = 10, height = 6) {
  ids <- subject_id_values(data)
  if (!length(ids)) {
    stop("No Subject IDs are available for plot export.", call. = FALSE)
  }
  grDevices::pdf(file, width = width, height = height, onefile = TRUE, paper = "special")
  on.exit(grDevices::dev.off(), add = TRUE)
  for (subject_id in ids) {
    for (plot_type in export_plot_types()) {
      print(export_plot_for_subject(data, subject_id, plot_type, thresholds = thresholds, time_window = time_window))
    }
  }
  invisible(file)
}

write_export_plot_bundle <- function(data, file, formats = "png", thresholds = default_cgm_thresholds(), time_window = default_time_window()) {
  format <- normalize_export_plot_format(formats)
  ids <- subject_id_values(data)
  if (!length(ids)) {
    stop("No Subject IDs are available for plot export.", call. = FALSE)
  }
  if (identical(format, "pdf")) {
    write_export_plots_pdf(data, file, thresholds = thresholds, time_window = time_window)
    return(invisible(file))
  }

  bundle_dir <- tempfile("cgm-plot-export-")
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- export_plot_manifest(data, formats = format, time_window = time_window)
  files <- character()
  if (nrow(manifest)) {
    for (i in seq_len(nrow(manifest))) {
      plot <- export_plot_for_subject(
        data,
        manifest$subject_id[[i]],
        manifest$plot_type[[i]],
        thresholds = thresholds,
        time_window = time_window
      )
      out_file <- file.path(bundle_dir, manifest$filename[[i]])
      write_export_plot_image(plot, out_file, manifest$format[[i]])
      files <- c(files, manifest$filename[[i]])
    }
  }
  zip::zipr(zipfile = file, files = files, root = bundle_dir)
  invisible(file)
}

export_complexity_plot_types <- function() {
  c("complexity_summary", "complexity_scale_curves")
}

export_complexity_plot_type_filename_label <- function(plot_type = "complexity_summary") {
  switch(
    plot_type %||% "complexity_summary",
    complexity_scale_curves = "complexity-scale-curves",
    complexity_summary = "complexity-summary",
    sanitize_plot_filename_part(plot_type)
  )
}

export_complexity_plot_file_stem <- function(subject_id, plot_type = "complexity_summary") {
  paste(
    c(
      "cgm",
      plot_subject_filename_label(subject_id),
      export_complexity_plot_type_filename_label(plot_type)
    ),
    collapse = "_"
  )
}

export_complexity_plots_pdf_filename <- function() {
  "cgm_complexity-plots_per-subject.pdf"
}

export_complexity_plots_zip_filename <- function() {
  "cgm_complexity-plots_per-subject.zip"
}

export_complexity_plot_manifest <- function(data, format = "png") {
  format <- normalize_export_plot_format(format)
  ids <- subject_id_values(data)
  rows <- list()
  if (!identical(format, "pdf") && length(ids)) {
    rows <- lapply(ids, function(subject_id) {
      do.call(rbind, lapply(export_complexity_plot_types(), function(plot_type) {
        data.frame(
          subject_id = subject_id,
          plot_type = plot_type,
          format = format,
          filename = paste0(export_complexity_plot_file_stem(subject_id, plot_type), ".", format),
          stringsAsFactors = FALSE
        )
      }))
    })
  }
  manifest <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      subject_id = character(),
      plot_type = character(),
      format = character(),
      filename = character(),
      stringsAsFactors = FALSE
    )
  }
  row.names(manifest) <- NULL
  manifest
}

export_complexity_plot_for_subject <- function(bundle, data, subject_id, plot_type = "complexity_summary") {
  subject_id <- as.character(subject_id)
  subject_data <- data[as.character(data$id) == subject_id, , drop = FALSE]
  metrics <- if (is.data.frame(bundle$metrics) && "id" %in% names(bundle$metrics)) {
    bundle$metrics[as.character(bundle$metrics$id) == subject_id, , drop = FALSE]
  } else {
    complexity_result_columns()
  }
  curves <- if (is.data.frame(bundle$curves) && "id" %in% names(bundle$curves)) {
    bundle$curves[as.character(bundle$curves$id) == subject_id, , drop = FALSE]
  } else {
    empty_complexity_curve_rows()
  }
  switch(
    plot_type,
    complexity_summary = create_complexity_summary_plot(
      prepare_complexity_plot_data(
        metrics,
        subject_data,
        metric = all_filter_value(),
        show_subject_id = TRUE
      ),
      metric = all_filter_value()
    ),
    complexity_scale_curves = create_complexity_scale_curve_plot(
      prepare_complexity_curve_plot_data(
        curves,
        subject_data,
        curve_metric = all_filter_value(),
        show_subject_id = TRUE
      )
    ),
    stop("Unknown Complexity plot type: ", plot_type, call. = FALSE)
  )
}

write_export_complexity_plots_pdf <- function(data, file, bundle, width = 10, height = 6) {
  ids <- subject_id_values(data)
  if (!length(ids)) {
    stop("No Subject IDs are available for Complexity plot export.", call. = FALSE)
  }
  grDevices::pdf(file, width = width, height = height, onefile = TRUE, paper = "special")
  on.exit(grDevices::dev.off(), add = TRUE)
  for (subject_id in ids) {
    for (plot_type in export_complexity_plot_types()) {
      print(export_complexity_plot_for_subject(bundle, data, subject_id, plot_type))
    }
  }
  invisible(file)
}

write_export_complexity_plot_bundle <- function(data, file, format = "png", parameters = complexity_default_parameters()) {
  format <- normalize_export_plot_format(format)
  ids <- subject_id_values(data)
  if (!length(ids)) {
    stop("No Subject IDs are available for Complexity plot export.", call. = FALSE)
  }
  bundle <- compute_complete_complexity_export_bundle(data, parameters)
  if (identical(format, "pdf")) {
    write_export_complexity_plots_pdf(data, file, bundle)
    return(invisible(file))
  }

  bundle_dir <- tempfile("cgm-complexity-plot-export-")
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- export_complexity_plot_manifest(data, format = format)
  files <- character()
  if (nrow(manifest)) {
    for (i in seq_len(nrow(manifest))) {
      plot <- export_complexity_plot_for_subject(
        bundle,
        data,
        manifest$subject_id[[i]],
        manifest$plot_type[[i]]
      )
      out_file <- file.path(bundle_dir, manifest$filename[[i]])
      write_export_plot_image(plot, out_file, manifest$format[[i]])
      files <- c(files, manifest$filename[[i]])
    }
  }
  zip::zipr(zipfile = file, files = files, root = bundle_dir)
  invisible(file)
}

export_predictive_plot_types <- function() {
  c("predictive_risk", "predictive_feature_importance")
}

export_predictive_plot_type_filename_label <- function(plot_type = "predictive_risk") {
  switch(
    plot_type %||% "predictive_risk",
    predictive_feature_importance = "predictive-feature-importance",
    predictive_risk = "predictive-risk",
    sanitize_plot_filename_part(plot_type)
  )
}

export_predictive_target_filename_label <- function(parameters = predictive_default_parameters()) {
  parameters <- normalize_predictive_parameters(parameters)
  if (identical(parameters$target, "high")) "above-range" else "below-range"
}

export_predictive_plot_file_stem <- function(subject_id, plot_type = "predictive_risk", parameters = predictive_default_parameters()) {
  parameters <- normalize_predictive_parameters(parameters)
  paste(
    c(
      "cgm",
      plot_subject_filename_label(subject_id),
      export_predictive_plot_type_filename_label(plot_type),
      export_predictive_target_filename_label(parameters),
      paste0(parameters$horizon_minutes, "-min"),
      sanitize_plot_filename_part(parameters$model)
    ),
    collapse = "_"
  )
}

export_predictive_plots_pdf_filename <- function(parameters = predictive_default_parameters()) {
  parameters <- normalize_predictive_parameters(parameters)
  paste0(
    "cgm_predictive-risk-plots_per-subject_",
    export_predictive_target_filename_label(parameters),
    "_",
    parameters$horizon_minutes,
    "-min_",
    sanitize_plot_filename_part(parameters$model),
    ".pdf"
  )
}

export_predictive_plots_zip_filename <- function(parameters = predictive_default_parameters()) {
  parameters <- normalize_predictive_parameters(parameters)
  paste0(
    "cgm_predictive-risk-plots_per-subject_",
    export_predictive_target_filename_label(parameters),
    "_",
    parameters$horizon_minutes,
    "-min_",
    sanitize_plot_filename_part(parameters$model),
    ".zip"
  )
}

export_predictive_plot_manifest <- function(data, format = "png", parameters = predictive_default_parameters()) {
  format <- normalize_export_plot_format(format)
  ids <- subject_id_values(data)
  rows <- list()
  if (!identical(format, "pdf") && length(ids)) {
    rows <- lapply(ids, function(subject_id) {
      do.call(rbind, lapply(export_predictive_plot_types(), function(plot_type) {
        data.frame(
          subject_id = subject_id,
          plot_type = plot_type,
          format = format,
          filename = paste0(export_predictive_plot_file_stem(subject_id, plot_type, parameters), ".", format),
          stringsAsFactors = FALSE
        )
      }))
    })
  }
  manifest <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      subject_id = character(),
      plot_type = character(),
      format = character(),
      filename = character(),
      stringsAsFactors = FALSE
    )
  }
  row.names(manifest) <- NULL
  manifest
}

export_predictive_status_plot <- function(bundle, subject_id, plot_type = "predictive_risk") {
  title <- if (identical(plot_type, "predictive_feature_importance")) {
    "Predictive Risk feature importance is not available"
  } else {
    "Predictive Risk timeline is not available"
  }
  message <- bundle$fit_message %||% "Predictive Risk is not ready for this Subject ID."
  empty_plot(paste(title, paste0("Subject ID: ", subject_id), message, sep = "\n"))
}

export_predictive_plot_for_subject <- function(bundle, subject_id, plot_type = "predictive_risk") {
  subject_id <- as.character(subject_id)
  if (!identical(bundle$fit_status, "ready")) {
    return(export_predictive_status_plot(bundle, subject_id, plot_type))
  }
  switch(
    plot_type,
    predictive_risk = create_predictive_risk_plot(
      bundle$scores,
      subject = subject_id,
      target_label = bundle$target_label,
      threshold = bundle$parameters$threshold,
      event_label = predictive_event_label(bundle$parameters),
      show_legend = TRUE
    ),
    predictive_feature_importance = create_predictive_importance_plot(bundle$importance),
    stop("Unknown Predictive Risk plot type: ", plot_type, call. = FALSE)
  )
}

write_export_predictive_plots_pdf <- function(data, file, bundles, parameters = predictive_default_parameters(), width = 10, height = 6) {
  ids <- subject_id_values(data)
  if (!length(ids)) {
    stop("No Subject IDs are available for Predictive Risk plot export.", call. = FALSE)
  }
  grDevices::pdf(file, width = width, height = height, onefile = TRUE, paper = "special")
  on.exit(grDevices::dev.off(), add = TRUE)
  for (subject_id in ids) {
    bundle <- bundles[[subject_id]]
    for (plot_type in export_predictive_plot_types()) {
      print(export_predictive_plot_for_subject(bundle, subject_id, plot_type))
    }
  }
  invisible(file)
}

write_export_predictive_plot_bundle <- function(
  data,
  file,
  format = "png",
  parameters = predictive_default_parameters(),
  thresholds = default_cgm_thresholds()
) {
  format <- normalize_export_plot_format(format)
  ids <- subject_id_values(data)
  if (!length(ids)) {
    stop("No Subject IDs are available for Predictive Risk plot export.", call. = FALSE)
  }
  parameters <- normalize_predictive_parameters(parameters)
  bundles <- compute_complete_predictive_risk_bundles(data, parameters = parameters, thresholds = thresholds)
  if (identical(format, "pdf")) {
    write_export_predictive_plots_pdf(data, file, bundles, parameters = parameters)
    return(invisible(file))
  }

  bundle_dir <- tempfile("cgm-predictive-plot-export-")
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- export_predictive_plot_manifest(data, format = format, parameters = parameters)
  files <- character()
  if (nrow(manifest)) {
    for (i in seq_len(nrow(manifest))) {
      subject_id <- manifest$subject_id[[i]]
      plot <- export_predictive_plot_for_subject(
        bundles[[subject_id]],
        subject_id,
        manifest$plot_type[[i]]
      )
      out_file <- file.path(bundle_dir, manifest$filename[[i]])
      write_export_plot_image(plot, out_file, manifest$format[[i]])
      files <- c(files, manifest$filename[[i]])
    }
  }
  zip::zipr(zipfile = file, files = files, root = bundle_dir)
  invisible(file)
}

imputed_export_available <- function(status) {
  identical((status %||% list())$state %||% "", "complete")
}

export_module_server <- function(id, analysis_data, settings, imputation_status = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    current_imputation_status <- shiny::reactive({
      if (is.function(imputation_status)) {
        return(imputation_status())
      }
      list(state = "not_run")
    })

    predictive_export_parameters <- shiny::reactive({
      predictive_default_parameters(
        horizon_minutes = as.integer(input$predictive_horizon_minutes %||% 30L),
        target = input$predictive_target %||% "low",
        model = input$predictive_model %||% "glm",
        min_examples = input$predictive_min_examples %||% 80L,
        min_events = input$predictive_min_events %||% 5L,
        threshold = input$predictive_threshold %||% 0.5
      )
    })

    shiny::observeEvent(settings()$thresholds_mg_dl, {
      selected <- input$predictive_target %||% "low"
      shiny::updateSelectInput(
        session,
        "predictive_target",
        choices = predictive_target_choices(settings()$thresholds_mg_dl),
        selected = selected
      )
    }, ignoreInit = FALSE)

    output$analysis_download_ui <- shiny::renderUI({
      label <- if (imputed_export_available(current_imputation_status())) {
        "Imputed analysis dataset"
      } else {
        "Analysis dataset"
      }
      shiny::downloadButton(session$ns("download_analysis_dataset"), label)
    })

    output$download_analysis_dataset <- shiny::downloadHandler(
      filename = function() {
        if (imputed_export_available(current_imputation_status())) {
          "cgm_imputed_analysis_dataset.csv"
        } else {
          "cgm_analysis_dataset.csv"
        }
      },
      content = function(file) {
        progress_title <- if (imputed_export_available(current_imputation_status())) {
          "Exporting imputed analysis data"
        } else {
          "Exporting analysis data"
        }
        cgm_with_progress(
          progress_title,
          detail = "Preparing CSV...",
          value = 0.1,
          session = session,
          {
            out <- prepare_cgm_data_export(analysis_data())
            shiny::incProgress(0.6, detail = "Writing CSV...")
            data.table::fwrite(out, file)
          }
        )
      }
    )

    output$download_metrics <- shiny::downloadHandler(
      filename = function() "cgm_core_metrics.csv",
      content = function(file) {
        cgm_with_progress(
          "Exporting Metrics",
          detail = "Calculating metrics for all Subject IDs...",
          value = 0.1,
          session = session,
          {
            out <- prepare_complete_metrics_export(analysis_data(), thresholds = settings()$thresholds_mg_dl)
            shiny::incProgress(0.6, detail = "Writing CSV...")
            data.table::fwrite(out, file)
          }
        )
      }
    )

    output$download_complexity_metrics <- shiny::downloadHandler(
      filename = function() "cgm_complexity_metrics.csv",
      content = function(file) {
        cgm_with_progress(
          "Exporting Complexity metrics",
          detail = "Calculating Complexity metrics for all Subject IDs...",
          value = 0.1,
          session = session,
          {
            out <- prepare_complete_complexity_export(analysis_data())
            shiny::incProgress(0.6, detail = "Writing CSV...")
            data.table::fwrite(out, file)
          }
        )
      }
    )

    output$download_functional_features <- shiny::downloadHandler(
      filename = glycemic_pattern_features_filename,
      content = function(file) {
        cgm_with_progress(
          "Exporting glycemic pattern features",
          detail = "Calculating glycemic patterns for all Subject IDs...",
          value = 0.1,
          session = session,
          {
            out <- prepare_complete_functional_features_export(
              analysis_data(),
              thresholds = settings()$thresholds_mg_dl
            )
            shiny::incProgress(0.6, detail = "Writing CSV...")
            data.table::fwrite(out, file)
          }
        )
      }
    )

    output$download_functional_curves <- shiny::downloadHandler(
      filename = glycemic_pattern_curves_filename,
      content = function(file) {
        cgm_with_progress(
          "Exporting glycemic pattern curves",
          detail = "Calculating smoothed curves for all Subject IDs...",
          value = 0.1,
          session = session,
          {
            out <- prepare_complete_functional_curves_export(
              analysis_data(),
              thresholds = settings()$thresholds_mg_dl
            )
            shiny::incProgress(0.6, detail = "Writing CSV...")
            data.table::fwrite(out, file)
          }
        )
      }
    )

    output$download_predictive_scores <- shiny::downloadHandler(
      filename = function() "cgm_predictive_risk_scores.csv",
      content = function(file) {
        cgm_with_progress(
          "Exporting Predictive Risk scores",
          detail = "Training retrospective risk model...",
          value = 0.1,
          session = session,
          {
            out <- prepare_complete_predictive_scores_export(
              analysis_data(),
              parameters = predictive_export_parameters(),
              thresholds = settings()$thresholds_mg_dl
            )
            shiny::incProgress(0.6, detail = "Writing CSV...")
            data.table::fwrite(out, file)
          }
        )
      }
    )

    output$download_predictive_performance <- shiny::downloadHandler(
      filename = function() "cgm_predictive_model_performance.csv",
      content = function(file) {
        cgm_with_progress(
          "Exporting Predictive Risk performance",
          detail = "Training retrospective risk model...",
          value = 0.1,
          session = session,
          {
            out <- prepare_complete_predictive_performance_export(
              analysis_data(),
              parameters = predictive_export_parameters(),
              thresholds = settings()$thresholds_mg_dl
            )
            shiny::incProgress(0.6, detail = "Writing CSV...")
            data.table::fwrite(out, file)
          }
        )
      }
    )

    output$download_predictive_importance <- shiny::downloadHandler(
      filename = function() "cgm_predictive_feature_importance.csv",
      content = function(file) {
        cgm_with_progress(
          "Exporting Predictive Risk feature importance",
          detail = "Training per-subject retrospective risk models...",
          value = 0.1,
          session = session,
          {
            out <- prepare_complete_predictive_feature_importance_export(
              analysis_data(),
              parameters = predictive_export_parameters(),
              thresholds = settings()$thresholds_mg_dl
            )
            shiny::incProgress(0.6, detail = "Writing CSV...")
            data.table::fwrite(out, file)
          }
        )
      }
    )

    output$download_plots <- shiny::downloadHandler(
      filename = function() {
        format <- normalize_export_plot_format(input$plot_format %||% "png")
        if (identical(format, "pdf")) {
          export_plots_pdf_filename()
        } else {
          export_plots_zip_filename()
        }
      },
      content = function(file) {
        cgm_with_progress(
          "Exporting plots",
          detail = "Preparing per-subject figures...",
          value = 0.1,
          session = session,
          {
            write_export_plot_bundle(
              analysis_data(),
              file,
              formats = input$plot_format %||% "png",
              thresholds = settings()$thresholds_mg_dl,
              time_window = default_time_window()
            )
          }
        )
      }
    )

    output$download_complexity_plots <- shiny::downloadHandler(
      filename = function() {
        format <- normalize_export_plot_format(input$plot_format %||% "png")
        if (identical(format, "pdf")) {
          export_complexity_plots_pdf_filename()
        } else {
          export_complexity_plots_zip_filename()
        }
      },
      content = function(file) {
        cgm_with_progress(
          "Exporting Complexity plots",
          detail = "Calculating full-dataset Complexity plots for all Subject IDs...",
          value = 0.1,
          session = session,
          {
            write_export_complexity_plot_bundle(
              analysis_data(),
              file,
              format = input$plot_format %||% "png"
            )
          }
        )
      }
    )

    output$download_predictive_plots <- shiny::downloadHandler(
      filename = function() {
        format <- normalize_export_plot_format(input$plot_format %||% "png")
        if (identical(format, "pdf")) {
          export_predictive_plots_pdf_filename(predictive_export_parameters())
        } else {
          export_predictive_plots_zip_filename(predictive_export_parameters())
        }
      },
      content = function(file) {
        cgm_with_progress(
          "Exporting Predictive Risk plots",
          detail = "Training per-subject predictive models and preparing figures...",
          value = 0.1,
          session = session,
          {
            write_export_predictive_plot_bundle(
              analysis_data(),
              file,
              format = input$plot_format %||% "png",
              parameters = predictive_export_parameters(),
              thresholds = settings()$thresholds_mg_dl
            )
          }
        )
      }
    )
  })
}
