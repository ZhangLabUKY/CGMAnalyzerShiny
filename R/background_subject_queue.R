subject_background_batch_size <- function(context = "metrics") {
  context <- context %||% "metrics"
  if (identical(context, "complexity")) {
    return(5L)
  }
  20L
}

subject_queue_order <- function(ids, selected = "") {
  ids <- sort(clean_filter_values(ids))
  selected <- normalize_filter_value(selected)
  if (nzchar(selected) && selected %in% ids) {
    return(c(selected, setdiff(ids, selected)))
  }
  ids
}

subject_queue_new <- function(ids, selected = "", batch_size = subject_background_batch_size()) {
  ids <- subject_queue_order(ids, selected)
  batch_size <- suppressWarnings(as.integer(batch_size %||% subject_background_batch_size()))
  if (is.na(batch_size) || batch_size < 1L) {
    batch_size <- subject_background_batch_size()
  }
  list(
    pending = ids,
    complete = character(),
    failed = character(),
    running = FALSE,
    batch_size = batch_size
  )
}

subject_queue_reprioritize <- function(queue, selected = "") {
  selected <- normalize_filter_value(selected)
  if (!nzchar(selected) || is.null(queue) || !length(queue$pending %||% character())) {
    return(queue)
  }
  if (!selected %in% queue$pending) {
    return(queue)
  }
  queue$pending <- c(selected, setdiff(queue$pending, selected))
  queue
}

subject_queue_next_batch <- function(queue) {
  if (is.null(queue) || isTRUE(queue$running) || !length(queue$pending %||% character())) {
    return(character())
  }
  utils::head(queue$pending, queue$batch_size %||% subject_background_batch_size())
}

subject_queue_mark_running <- function(queue, running = TRUE) {
  queue$running <- isTRUE(running)
  queue
}

subject_queue_mark_finished <- function(queue, ids, failed = character()) {
  ids <- clean_filter_values(ids)
  failed <- clean_filter_values(failed)
  if (!length(ids)) {
    queue$running <- FALSE
    return(queue)
  }
  queue$pending <- setdiff(queue$pending %||% character(), ids)
  queue$complete <- sort(unique(c(queue$complete %||% character(), setdiff(ids, failed))))
  queue$failed <- sort(unique(c(queue$failed %||% character(), failed)))
  queue$running <- FALSE
  queue
}

subject_queue_progress <- function(queue, total_ids) {
  total_ids <- clean_filter_values(total_ids)
  done <- length(unique(c(queue$complete %||% character(), queue$failed %||% character())))
  list(
    done = done,
    total = length(total_ids),
    running = isTRUE(queue$running),
    complete = length(total_ids) > 0L && done >= length(total_ids)
  )
}

subject_queue_progress_text <- function(label, queue, total_ids) {
  progress <- subject_queue_progress(queue, total_ids)
  if (!progress$total) {
    return("")
  }
  verb <- if (isTRUE(progress$complete)) "cached" else if (isTRUE(progress$running)) "running" else "cached"
  sprintf(
    "%s %s for %s of %s Subject IDs",
    label,
    verb,
    format_count(progress$done),
    format_count(progress$total)
  )
}
