export_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Export"),
    shiny::downloadButton(ns("download_standardized"), "Original standardized data"),
    shiny::downloadButton(ns("download_analysis"), "Analysis data"),
    shiny::downloadButton(ns("download_metrics"), "Metrics"),
    shiny::downloadButton(ns("download_qc"), "QC summary"),
    shiny::downloadButton(ns("download_settings"), "Settings log")
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

export_module_server <- function(id, standardized, analysis_data, metrics, qc_summary, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    output$download_standardized <- shiny::downloadHandler(
      filename = function() "cgm_standardized_data.csv",
      content = function(file) data.table::fwrite(prepare_cgm_data_export(standardized()), file)
    )

    output$download_metrics <- shiny::downloadHandler(
      filename = function() "cgm_core_metrics.csv",
      content = function(file) data.table::fwrite(prepare_metrics_cached_export(metrics(), analysis_data(), thresholds = settings()$thresholds_mg_dl), file)
    )

    output$download_analysis <- shiny::downloadHandler(
      filename = function() "cgm_analysis_data.csv",
      content = function(file) data.table::fwrite(prepare_cgm_data_export(analysis_data()), file)
    )

    output$download_qc <- shiny::downloadHandler(
      filename = function() "cgm_qc_summary.csv",
      content = function(file) data.table::fwrite(prepare_qc_display(qc_summary(), analysis_data()), file)
    )

    output$download_settings <- shiny::downloadHandler(
      filename = function() "cgm_reproducibility_settings.txt",
      content = function(file) {
        utils::capture.output(utils::str(settings()), file = file)
      }
    )
  })
}
