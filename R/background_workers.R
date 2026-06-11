background_worker_state <- new.env(parent = emptyenv())
background_worker_state$generation <- 0L
background_worker_state$configured <- FALSE
background_worker_state$workers <- NULL

background_worker_count <- function(available_cores = NULL) {
  cores <- available_cores
  if (is.null(cores)) {
    cores <- tryCatch(
      future::availableCores(),
      error = function(error) 2L
    )
  }
  cores <- suppressWarnings(as.integer(cores[[1L]]))
  if (is.na(cores) || cores < 1L) {
    cores <- 2L
  }
  min(4L, max(2L, cores - 1L))
}

configure_background_workers <- function(workers = background_worker_count(), plan_fn = NULL) {
  if (!requireNamespace("future", quietly = TRUE)) {
    return(NA_integer_)
  }
  plan_fn <- plan_fn %||% future::plan
  workers <- background_worker_count(workers + 1L)
  if (isTRUE(background_worker_state$configured) && identical(background_worker_state$workers, workers)) {
    background_worker_state$generation <- background_worker_state$generation + 1L
    return(background_worker_state$generation)
  }
  plan_fn(future::multisession, workers = workers)
  background_worker_state$generation <- background_worker_state$generation + 1L
  background_worker_state$configured <- TRUE
  background_worker_state$workers <- workers
  background_worker_state$generation
}

cleanup_background_workers <- function(plan_fn = NULL) {
  if (!requireNamespace("future", quietly = TRUE)) {
    return(FALSE)
  }
  plan_fn <- plan_fn %||% future::plan
  plan_fn(future::sequential)
  background_worker_state$configured <- FALSE
  background_worker_state$workers <- NULL
  TRUE
}

schedule_background_worker_cleanup <- function(
  token = NULL,
  delay = 300,
  scheduler = NULL,
  cleanup_fn = NULL
) {
  if (!requireNamespace("later", quietly = TRUE)) {
    return(FALSE)
  }
  scheduler <- scheduler %||% later::later
  cleanup_fn <- cleanup_fn %||% cleanup_background_workers
  token <- token %||% background_worker_state$generation

  scheduler(function() {
    if (identical(background_worker_state$generation, token)) {
      cleanup_fn()
    }
    invisible(NULL)
  }, delay = delay)
  TRUE
}
