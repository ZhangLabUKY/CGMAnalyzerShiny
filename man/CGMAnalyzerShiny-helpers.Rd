\name{CGMAnalyzerShiny-helpers}
\alias{compute_core_metrics}
\alias{compute_cgmanalyzer_metrics}
\alias{compute_iglu_metrics}
\alias{compare_missingness_summaries}
\alias{compute_missingness_summary}
\alias{compute_qc_summary}
\alias{cgmissingdata_available}
\alias{create_missingness_timeline_plot}
\alias{create_agp_summary_plot}
\alias{create_daily_overlay_plot}
\alias{create_reproducibility_settings}
\alias{create_tir_plot}
\alias{create_trace_plot}
\alias{detect_gap_periods}
\alias{apply_imputed_glucose}
\alias{run_cgmissingdata_imputation}
\alias{load_demo_cgm_data}
\alias{load_missingness_demo_cgm_data}
\alias{metric_display_catalog}
\alias{metric_test_choices}
\alias{parse_cgm_timestamp}
\alias{plot_filter_available}
\alias{prepare_metrics_display}
\alias{prepare_time_of_day_plot_data}
\alias{filter_metrics_display}
\alias{regularize_cgm_series}
\alias{run_metric_stat_test}
\alias{run_app}
\alias{standardize_cgm_data}
\alias{summarize_imputation_status}
\alias{to_iglu_data}
\title{CGMAnalyzerShiny exported helpers}
\description{
Foundational helpers and app entrypoints used by the CGMAnalyzerShiny Shiny
application. These functions support timestamp parsing, data standardization,
quality control summaries, core metrics, plots, reproducibility settings, and
launching the app.
}
\usage{
parse_cgm_timestamp(x, tz = "UTC")
standardize_cgm_data(data, mapping, units = "mg/dL", source_file = NA_character_, tz = "UTC")
compute_qc_summary(data, valid_day_hours = 14, glucose_min = 40, glucose_max = 400)
compute_missingness_summary(data, valid_day_hours = 14)
compare_missingness_summaries(original_data, analysis_data, valid_day_hours = 14)
summarize_imputation_status(original_data, analysis_data, settings)
detect_gap_periods(data, gap_multiplier = 1.5)
create_missingness_timeline_plot(data)
prepare_time_of_day_plot_data(data, participant = "", group = "", visit = "", day = "")
create_daily_overlay_plot(data, thresholds = default_cgm_thresholds(), participant = "", group = "", visit = "", day = "")
create_agp_summary_plot(data, thresholds = default_cgm_thresholds(), participant = "", group = "", visit = "", bin_minutes = 30)
plot_filter_available(data, column, min_values = 2L)
cgmissingdata_available()
run_cgmissingdata_imputation(data, model = "mice_only", seed = 42)
apply_imputed_glucose(data, imputation_result, model = "mice_only")
compute_core_metrics(data, thresholds = default_cgm_thresholds(), by = default_metric_groups(data))
to_iglu_data(data)
regularize_cgm_series(data, interval_minutes = NULL, max_gap_intervals = 4)
compute_cgmanalyzer_metrics(data, by = default_metric_groups(data), conga_hours = 2, max_gap_intervals = 4)
compute_iglu_metrics(data, by = default_metric_groups(data))
create_trace_plot(data, thresholds = default_cgm_thresholds(), participant_id = NULL)
create_tir_plot(metrics)
load_demo_cgm_data()
load_missingness_demo_cgm_data()
metric_display_catalog()
metric_test_choices(metrics)
prepare_metrics_display(metrics)
filter_metrics_display(display, participant = "", group = "", visit = "", category = "", include_category = TRUE)
run_metric_stat_test(metrics, metric, grouping = "group", test_type = "welch_t")
create_reproducibility_settings(
  column_mapping = list(),
  thresholds = default_cgm_thresholds(),
  units = "mg/dL",
  valid_day_hours = 14,
  imputation_method = "none",
  imputation_model = "mice_only",
  imputation_seed = 42,
  imputation_available = cgmissingdata_available(),
  selected_metrics = "core",
  selected_tests = character()
)
run_app(...)
}
\arguments{
\item{x}{Timestamp vector.}
\item{tz}{Time zone used for timestamp parsing.}
\item{data}{Input or standardized CGM data frame.}
\item{original_data}{Standardized CGM data before optional imputation.}
\item{analysis_data}{Current analysis CGM data after preprocessing settings.}
\item{settings}{Reproducibility settings list.}
\item{mapping}{Named column mapping list.}
\item{units}{Source glucose units.}
\item{source_file}{Optional source file label.}
\item{valid_day_hours}{Minimum observed hours for a valid CGM day.}
\item{glucose_min}{Minimum plausible glucose value in mg/dL.}
\item{glucose_max}{Maximum plausible glucose value in mg/dL.}
\item{gap_multiplier}{Gap threshold as a multiple of the median sampling interval.}
\item{day}{Date filter as \code{YYYY-MM-DD}.}
\item{column}{Column name to check for usable filter values.}
\item{min_values}{Minimum number of concrete values required for a filter control.}
\item{bin_minutes}{Time-of-day bin width in minutes.}
\item{model}{Imputation model name.}
\item{seed}{Imputation seed.}
\item{imputation_result}{Result from CGMissingDataR imputation.}
\item{thresholds}{Named glucose threshold list in mg/dL.}
\item{by}{Metric grouping columns.}
\item{interval_minutes}{Target regular sampling interval in minutes.}
\item{max_gap_intervals}{Maximum gap size eligible for interpolation, expressed as a number of intervals.}
\item{conga_hours}{Lag in hours for CONGA.}
\item{participant_id}{Optional participant ID filter.}
\item{metrics}{Metric summary data frame.}
\item{metric}{Raw metric column name.}
\item{grouping}{Grouping column name.}
\item{test_type}{Statistical test type.}
\item{display}{Long-format user-facing metric display data.}
\item{participant}{Participant filter value.}
\item{group}{Group filter value.}
\item{visit}{Visit filter value.}
\item{category}{Metric category filter value.}
\item{include_category}{Whether to apply the category filter.}
\item{column_mapping}{Named list of source column mappings.}
\item{imputation_method}{Imputation method label.}
\item{imputation_model}{Imputation model label.}
\item{imputation_seed}{Imputation seed.}
\item{imputation_available}{Whether CGMissingDataR imputation is available.}
\item{selected_metrics}{Selected metric label or vector.}
\item{selected_tests}{Selected statistical tests.}
\item{...}{Additional arguments passed to \code{shiny::shinyApp()}.}
}
\value{
Depending on the function, a POSIXct vector, standardized data frame, summary
data frame, ggplot object, settings list, or Shiny application object.
}
