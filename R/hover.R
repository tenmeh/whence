#' Keep the replay so hover counts and row clicks share the same pass
#' @noRd
attach_replay <- function(trace) {
  replay <- if (!identical(getOption("whence.hover"), "none")) {
    tryCatch(
      replay_trace(trace),
      error = function(e) c(list(ok = FALSE), lost_identity(NA_character_, "the replay failed"))
    )
  }
  # Keep successful replays for clicks and only the reason for failed ones.
  trace$replay <- if (isTRUE(replay$ok)) replay
  trace$hover_reason <- if (isFALSE(replay$ok)) {
    reason <- row_count_reason(trace, replay)
    if (is.null(replay$why)) paste0("the replay stops at ", reason) else reason
  }
  trace
}


#' Give the browser counts for shown rows so hover can open without a request
#' @noRd
hover_grid <- function(trace) {
  replay <- trace$replay
  if (is.null(replay) || is.null(trace$keys)) {
    reason <- trace$hover_reason %||% row_count_reason(trace, NULL)
    return(list(steps = NULL, counts = NULL, reason = paste0("Counts for this row are not known: ", reason, ".")))
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


#' The reason for missing counts, without the wording of a panel or preview
#' @noRd
row_count_reason <- function(table, replay) {
  if (is.null(replay)) {
    if (is.null(table$keys)) {
      return("the table does not keep the group columns of its last summary")
    }
    return("the pipeline was not replayed")
  }
  if (!replay$ok) {
    return(replay$why %||% replay_stop_step(replay$verb))
  }
  row_origin(table)$prefix_reason
}
