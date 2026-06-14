cgm_native_symbol_names <- function() {
  c(
    "_CGManalyzer2_lttb_indices_cpp",
    "_CGManalyzer2_dfa_details_cpp",
    "_CGManalyzer2_higuchi_details_cpp",
    "_CGManalyzer2_optional_lag_metrics_by_time_cpp",
    "_CGManalyzer2_optional_metrics_cpp",
    "_CGManalyzer2_mse_scales_cpp"
  )
}

cgm_native_function_names <- function() {
  sub("^_CGManalyzer2_", "", cgm_native_symbol_names())
}

cgm_native_symbol_environment <- function(env = NULL) {
  if (!is.null(env)) {
    return(env)
  }
  if (exists("lttb_indices_cpp", mode = "function")) {
    return(environment(lttb_indices_cpp))
  }
  parent.frame()
}

cgm_native_sourcecpp_loaded <- function(env) {
  isTRUE(get0(".cgm_native_sourcecpp_loaded", envir = env, inherits = FALSE))
}

cgm_native_dll_loaded <- function() {
  app_package_name() %in% names(getLoadedDLLs())
}

cgm_source_tree_native_dll_candidate <- function(dll_name) {
  description_path <- "DESCRIPTION"
  if (!file.exists(description_path)) {
    return(character())
  }
  package_name <- tryCatch(
    as.character(read.dcf(description_path, fields = "Package")[[1L]]),
    error = function(error) NA_character_
  )
  candidate <- file.path("src", dll_name)
  if (identical(package_name, app_package_name()) && file.exists(candidate)) {
    return(candidate)
  }
  character()
}

cgm_native_dll_candidates <- function() {
  package_name <- app_package_name()
  dll_name <- paste0(package_name, .Platform$dynlib.ext)
  candidates <- c(
    system.file("libs", .Platform$r_arch, dll_name, package = package_name),
    system.file("libs", dll_name, package = package_name),
    cgm_source_tree_native_dll_candidate(dll_name)
  )
  candidates[nzchar(candidates) & file.exists(candidates)]
}

cgm_load_native_dll <- function(quiet = TRUE) {
  if (cgm_native_dll_loaded()) {
    return(TRUE)
  }
  candidates <- cgm_native_dll_candidates()
  if (!length(candidates)) {
    if (!isTRUE(quiet)) {
      warning("CGManalyzer2 native DLL was not found.", call. = FALSE)
    }
    return(FALSE)
  }
  for (dll in candidates) {
    loaded <- tryCatch({
      dyn.load(normalizePath(dll, winslash = "/", mustWork = TRUE))
      TRUE
    }, error = function(error) FALSE)
    if (loaded && cgm_native_dll_loaded()) {
      return(TRUE)
    }
  }
  if (!isTRUE(quiet)) {
    warning("CGManalyzer2 native DLL could not be loaded.", call. = FALSE)
  }
  FALSE
}

cgm_source_tree_cpp_candidate <- function() {
  description_path <- "DESCRIPTION"
  source_path <- file.path("src", "lttb.cpp")
  if (!file.exists(description_path) || !file.exists(source_path)) {
    return(character())
  }
  package_name <- tryCatch(
    as.character(read.dcf(description_path, fields = "Package")[[1L]]),
    error = function(error) NA_character_
  )
  if (identical(package_name, app_package_name())) {
    return(source_path)
  }
  character()
}

cgm_load_native_sourcecpp <- function(env = NULL, quiet = TRUE) {
  env <- cgm_native_symbol_environment(env)
  if (cgm_native_sourcecpp_loaded(env)) {
    return(TRUE)
  }
  source_path <- cgm_source_tree_cpp_candidate()
  if (!length(source_path)) {
    return(FALSE)
  }
  loaded <- tryCatch({
    Rcpp::sourceCpp(source_path, env = env, rebuild = FALSE, verbose = FALSE)
    assign(".cgm_native_sourcecpp_loaded", TRUE, envir = env)
    TRUE
  }, error = function(error) {
    if (!isTRUE(quiet)) {
      warning(
        paste("CGManalyzer2 native C++ source could not be compiled:", conditionMessage(error)),
        call. = FALSE
      )
    }
    FALSE
  })
  loaded
}

cgm_bootstrap_native_symbols <- function(env = NULL, quiet = TRUE) {
  env <- cgm_native_symbol_environment(env)
  symbols <- cgm_native_symbol_names()
  available <- vapply(symbols, exists, logical(1), envir = env, inherits = FALSE)
  if (all(available)) {
    return(TRUE)
  }
  if (!cgm_load_native_dll(quiet = quiet)) {
    return(cgm_load_native_sourcecpp(env = env, quiet = quiet))
  }
  ok <- TRUE
  for (symbol in symbols) {
    if (exists(symbol, envir = env, inherits = FALSE)) {
      next
    }
    info <- tryCatch(
      getNativeSymbolInfo(symbol, PACKAGE = app_package_name()),
      error = function(error) NULL
    )
    if (is.null(info)) {
      ok <- FALSE
    } else {
      assigned <- tryCatch({
        assign(symbol, info, envir = env)
        TRUE
      }, error = function(error) FALSE)
      ok <- ok && assigned
    }
  }
  if (!ok && !isTRUE(quiet)) {
    warning("Some CGManalyzer2 native symbols could not be registered.", call. = FALSE)
  }
  ok
}

cgm_native_symbols_available <- function(env = NULL) {
  env <- cgm_native_symbol_environment(env)
  if (all(vapply(cgm_native_symbol_names(), exists, logical(1), envir = env, inherits = FALSE))) {
    return(TRUE)
  }
  cgm_native_sourcecpp_loaded(env) &&
    all(vapply(cgm_native_function_names(), exists, logical(1), envir = env, inherits = FALSE))
}

cgm_require_native_symbols <- function(symbols = cgm_native_symbol_names(), env = NULL) {
  env <- cgm_native_symbol_environment(env)
  if (!cgm_bootstrap_native_symbols(env = env, quiet = TRUE)) {
    stop(
      "CGManalyzer2 Rcpp native symbols are unavailable. Run Rcpp::compileAttributes(), rebuild or reload the package, and restart the app.",
      call. = FALSE
    )
  }
  if (cgm_native_sourcecpp_loaded(env)) {
    missing_functions <- cgm_native_function_names()[!vapply(cgm_native_function_names(), exists, logical(1), envir = env, inherits = FALSE)]
    if (!length(missing_functions)) {
      return(TRUE)
    }
  }
  missing <- symbols[!vapply(symbols, exists, logical(1), envir = env, inherits = FALSE)]
  if (length(missing)) {
    stop(
      paste0(
        "CGManalyzer2 Rcpp native symbols are unavailable: ",
        paste(missing, collapse = ", "),
        ". Rebuild or reload the package and restart the app."
      ),
      call. = FALSE
    )
  }
  TRUE
}
