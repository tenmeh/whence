attach_replay <- function(trace) {
  replay <- if (!identical(getOption("whence.hover"), "none")) {
    tryCatch(
      replay_trace(trace),
      error = function(e) list(ok = FALSE, reason = "the replay failed.")
    )
  }
  # Keep successful replays for clicks and only the reason for failed ones.
  trace$replay <- if (isTRUE(replay$ok)) replay
  trace$hover_reason <- if (isFALSE(replay$ok)) group_reason(trace, replay)
  trace
}


hover_grid <- function(trace) {
  replay <- trace$replay
  if (is.null(replay) || is.null(trace$keys)) {
    reason <- trace$hover_reason %||% if (is.null(trace$keys)) {
      "the table does not keep the group columns of its last summary."
    } else {
      "the pipeline was not replayed."
    }
    reason <- sub("^Counts for this group are not known: ", "", reason)
    reason <- sub("^Counts for this group are not known past ", "the replay stops at ", reason)
    return(list(steps = NULL, counts = NULL, reason = paste0("Counts for this row are not known: ", reason)))
  }

  stages <- trace$stages
  shown <- which(stages$shown)
  n_rows <- min(nrow(trace$value), max_table_rows)
  # Count all visible groups in one pass per step. Codes are final row numbers.
  counts <- lapply(shown, function(step) {
    codes <- if (step <= length(replay$codes)) replay$codes[[step]]
    if (is.null(codes)) rep(NA_integer_, n_rows) else tabulate(codes, nbins = n_rows)
  })
  list(
    steps = lapply(shown, function(step) list(label = stages$label[[step]], kind = stages$kind[[step]])),
    counts = lapply(seq_len(n_rows), function(row) vapply(counts, `[[`, integer(1), row)),
    reason = NULL
  )
}
