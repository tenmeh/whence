# Requests from the browser, for every whence output of a session.
#
# The traces of a session are in one environment, by output id, and one
# observer answers every request. The browser sends a list to one input:
#
#   id      the output id
#   action  "rows"    the rows that make the value
#           "removed" the rows that one step removed (with `step`)
#           "group"   the panel of one row of a table (with `row`)
#           "point"   the panel of the point of a plot that was clicked
#                     (with `click`, the click data of shiny)
#           "gone"    the output was removed from the page
#   row     for a table or a plot: the row of the table that the request is for
#
# The leading dot keeps the input out of the inputs that an app lists.
request_input <- ".whence_request"

# The most rows that "View the rows" sends to the browser.
max_rows_sent <- 10000L


#' Keep the latest trace of an output, with its options
#'
#' A new trace drops the panels of rows that were made for the old one.
#' @noRd
remember_trace <- function(session, name, trace, format = NULL) {
  session <- root_session(session)
  traces <- session_traces(session)
  trace$format <- format
  assign(name, trace, envir = traces)
  forget_groups(session, name)
  invisible(traces)
}


#' The root session, also inside a module
#' @noRd
root_session <- function(session) {
  if (is.function(session$rootScope)) session$rootScope() else session
}


#' The environment of traces of a session, made on first use
#' @noRd
session_traces <- function(session) {
  traces <- session$userData$.whence_traces
  if (is.null(traces)) {
    traces <- new.env(parent = emptyenv())
    session$userData$.whence_traces <- traces
    session$userData$.whence_groups <- new.env(parent = emptyenv())
    shiny::observeEvent(
      session$input[[request_input]],
      handle_request(session, traces, session$input[[request_input]]),
      ignoreInit = TRUE,
      domain = session
    )
  }
  traces
}


#' The explanation of one row of a table, made once and then kept
#' @noRd
cached_group <- function(session, name, table, row) {
  groups <- session$userData$.whence_groups
  key <- paste0(name, "\037", row)
  group <- if (!is.null(groups)) get0(key, envir = groups, inherits = FALSE)
  if (is.null(group)) {
    group <- whence_group(table, row = row)
    if (!is.null(groups)) {
      assign(key, group, envir = groups)
    }
  }
  group
}


#' Drop the kept explanations of the rows of one output
#' @noRd
forget_groups <- function(session, name) {
  groups <- session$userData$.whence_groups
  if (is.null(groups)) {
    return(invisible())
  }
  keys <- ls(groups, all.names = TRUE)
  keys <- keys[startsWith(keys, paste0(name, "\037"))]
  if (length(keys) > 0) {
    rm(list = keys, envir = groups)
  }
  invisible()
}


#' Answer one request from the browser
#'
#' The request comes from the browser, so its fields are checked before use.
#' A trace that is not there any more gets a clear answer, not silence: the
#' browser can show an output again from its cache after "gone" was sent.
#' @noRd
handle_request <- function(session, traces, request) {
  id <- request$id
  if (!is.character(id) || length(id) != 1 || !nzchar(id)) {
    return(invisible())
  }
  action <- request$action %||% "rows"

  if (identical(action, "gone")) {
    if (exists(id, envir = traces, inherits = FALSE)) {
      rm(list = id, envir = traces)
    }
    forget_groups(session, id)
    return(invisible())
  }

  reply <- if (action %in% c("group", "point")) "whence:group" else "whence:rows"
  fail <- function(message, row = NULL) {
    session$sendCustomMessage(reply, list(id = id, row = row, ok = FALSE, message = message))
    invisible()
  }

  trace <- get0(id, envir = traces, inherits = FALSE)
  if (is.null(trace)) {
    return(fail("The rows are not available any more. Change an input to show the value again."))
  }

  # An error in an observer ends the session, so each failure is sent to the
  # panel instead.
  tryCatch(
    answer_request(session, id, trace, action, request, fail),
    error = function(e) fail("Row attribution unavailable: the rows could not be shown.", request_row(request))
  )
  invisible()
}


#' @noRd
answer_request <- function(session, id, trace, action, request, fail) {
  row <- request_row(request)

  if (identical(action, "point")) {
    row <- clicked_row(trace$value, request$click, trace$plot_values)
    if (is.na(row)) {
      return(fail("Click on a bar or a point of the plot."))
    }
    action <- "group"
  }

  target <- trace
  if (!is.null(row)) {
    if (!inherits(trace, "whence_table") || is.null(trace$keys) || row > nrow(trace$value)) {
      return(fail("This row cannot be explained.", row))
    }
    target <- cached_group(session, id, trace, row)
  }

  if (identical(action, "group")) {
    if (is.null(row)) {
      return(fail("This row cannot be explained."))
    }
    session$sendCustomMessage("whence:group", list(
      id = id,
      row = row,
      ok = TRUE,
      panel = group_payload(target, trace$format)
    ))
    return(invisible())
  }

  if (identical(action, "removed")) {
    step <- request$step
    valid <- tryCatch({
      check_step(target, step)
      TRUE
    }, error = function(e) FALSE)
    if (is.null(step) || !valid) {
      return(fail("This step cannot show its rows.", row))
    }
    label <- target$stages$label[target$stages$step == step]
    send_rows(session, id, target, step = step, row = row, title = paste("removed by", label))
    return(invisible())
  }

  send_rows(session, id, target, row = row)
}


#' The row of a request, as a whole number, or `NULL`
#' @noRd
request_row <- function(request) {
  row <- request$row
  if (is.numeric(row) && length(row) == 1 && !is.na(row) && row >= 1 && row == round(row)) {
    as.integer(row)
  }
}


#' Find the rows and send them to the browser
#'
#' `x` is a trace or a group. With `step`, the rows are those that the step
#' removed.
#' @noRd
send_rows <- function(session, name, x, has_reactable = requireNamespace("reactable", quietly = TRUE),
                      step = NULL, row = NULL, title = NULL) {
  result <- tryCatch(
    rows_query(x, step),
    error = function(e) {
      list(rows = NULL, reason = "Row attribution unavailable: a step failed.")
    }
  )

  if (is.null(result$rows)) {
    message <- list(id = name, row = row, ok = FALSE, message = result$reason)
  } else {
    rows <- result$rows
    total <- nrow(rows)
    cap <- rows_cap(has_reactable)
    if (total > cap) {
      rows <- rows[seq_len(cap), , drop = FALSE]
    }
    content <- rows_content(rows, has_reactable)
    message <- list(
      id = name,
      row = row,
      ok = TRUE,
      title = title,
      total = total,
      totalText = format_count(total),
      shown = nrow(rows),
      shownText = format_count(nrow(rows)),
      html = content$html,
      deps = content$deps
    )
  }

  session$sendCustomMessage("whence:rows", message)
}


#' The rows as HTML, with their dependencies
#'
#' 'reactable' gives a table with pages and a search box. Without it the
#' rows are a plain HTML table.
#' @noRd
rows_content <- function(rows, has_reactable = requireNamespace("reactable", quietly = TRUE)) {
  rows <- as.data.frame(rows)
  table <- if (has_reactable) {
    reactable::reactable(
      rows,
      compact = TRUE,
      searchable = TRUE,
      defaultPageSize = 10,
      highlight = TRUE
    )
  } else {
    plain_table(rows)
  }

  rendered <- htmltools::renderTags(table)
  deps <- htmltools::resolveDependencies(rendered$dependencies)
  list(
    html = as.character(rendered$html),
    deps = lapply(deps, shiny::createWebDependency)
  )
}


#' @noRd
rows_cap <- function(has_reactable) {
  if (has_reactable) max_rows_sent else 200L
}


#' @noRd
plain_table <- function(rows) {
  shown <- utils::head(rows, rows_cap(FALSE))
  cells <- lapply(shown, function(col) format(col, trim = TRUE))
  htmltools::tags$div(
    class = "whence-rows-table",
    htmltools::tags$table(
      class = "table table-sm",
      htmltools::tags$thead(
        htmltools::tags$tr(lapply(names(rows), htmltools::tags$th))
      ),
      htmltools::tags$tbody(
        lapply(seq_len(nrow(shown)), function(i) {
          htmltools::tags$tr(lapply(cells, function(col) htmltools::tags$td(col[[i]])))
        })
      )
    )
  )
}
