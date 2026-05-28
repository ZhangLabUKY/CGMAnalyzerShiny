<!-- badges: start -->
[![R-CMD-check](https://github.com/ZhangLabUKY/CGMAnalyzerShiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZhangLabUKY/CGMAnalyzerShiny/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# CGMAnalyzerShiny

`CGMAnalyzerShiny` is a package-style Shiny application scaffold for continuous
glucose monitoring analysis. The app starts with upload, column mapping,
quality control, core metrics, trace plots, TIR plots, and CSV exports.

The analysis roadmap prefers `CGManalyzer` for standard CGM analysis, uses
`iglu` selectively for validation or gaps, and reserves `CGMissingDataR` for
missing glucose imputation workflows after the non-imputed MVP is stable.

## Run Locally

Open the project in RStudio and run:

```r
source("app.R")
```

Once the package is loaded or installed, you can also run:

```r
CGMAnalyzerShiny::run_app()
```

## Current Status

This is the Phase 0/Phase 1 foundation. Advanced imputation, statistical
testing, and complexity analytics are represented in the module structure and
will be implemented after the core upload-QC-metrics pipeline is validated.
