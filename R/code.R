# Reproduce code.
#
# The source and the stages are written back as one native-pipe chain.
#
# Only two forms are replaced by their values: `input$name` and
# `input[["name"]]`. Nothing else is resolved. A wider rule would one day
# replace a name that exists in the calling environment but that the verb
# reads as a column, and it would give wrong code with no warning.


#' Build the reproduce code for a pipeline
#' @noRd
build_code <- function(source, stages, env) {
  code <- deparse_one(substitute_inputs(source, env), collapse = "\n")

  for (stage in stages) {
    call <- substitute_inputs(stage$call, env)
    if (stage$dot) {
      # The native pipe cannot put the data in a position other than the
      # first. The upstream code is thus nested in the call.
      text <- deparse_one(call, collapse = "\n")
      code <- gsub(".whence_data", paste0("(", code, ")"), text, fixed = TRUE)
    } else {
      step <- deparse_one(call[-2], collapse = "\n")
      code <- paste0(code, " |>\n  ", step)
    }
  }

  code
}


#' Replace `input$name` and `input[["name"]]` with their values
#'
#' A reference that cannot be evaluated is left as it is.
#' @noRd
substitute_inputs <- function(expr, env) {
  if (is_input_ref(expr)) {
    failed <- FALSE
    value <- tryCatch(eval(expr, env), error = function(e) failed <<- TRUE)
    if (failed) {
      return(expr)
    }
    literal <- value_literal(value)
    if (is.null(literal)) {
      return(expr)
    }
    return(literal[[1]])
  }

  if (is.call(expr)) {
    for (i in seq_along(expr)) {
      part <- expr[[i]]
      if (!is.null(part) && !identical(part, quote(expr = ))) {
        expr[i] <- list(substitute_inputs(part, env))
      }
    }
  }
  expr
}


#' Is `expr` the form `input$name` or `input[["name"]]`?
#' @noRd
is_input_ref <- function(expr) {
  if (!is.call(expr) || length(expr) != 3) {
    return(FALSE)
  }
  if (!identical(expr[[2]], quote(input))) {
    return(FALSE)
  }
  key <- expr[[3]]
  if (identical(expr[[1]], quote(`$`))) {
    return(is.symbol(key) || (is.character(key) && length(key) == 1))
  }
  if (identical(expr[[1]], quote(`[[`))) {
    return(is.character(key) && length(key) == 1)
  }
  FALSE
}


#' An expression that makes `value` again, wrapped in a list
#'
#' The list wrapper lets the result be `NULL` itself. The function gives
#' `NULL` (with no list) when `value` has no simple literal form.
#' @noRd
value_literal <- function(value) {
  if (is.null(value)) {
    return(list(NULL))
  }
  if (inherits(value, "Date")) {
    return(list(call("as.Date", combine_literal(format(value)))))
  }
  if (inherits(value, "POSIXct")) {
    tz <- attr(value, "tzone") %||% ""
    text <- combine_literal(format(value, "%Y-%m-%d %H:%M:%S"))
    return(list(call("as.POSIXct", text, tz = tz[[1]])))
  }
  if (is.factor(value)) {
    return(list(combine_literal(as.character(value))))
  }
  if (is.atomic(value) && all(names(attributes(value)) == "names")) {
    return(list(combine_literal(value)))
  }
  NULL
}


#' A vector as a literal: one value as it is, more values in `c()`
#' @noRd
combine_literal <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  as.call(c(list(as.symbol("c")), as.list(x)))
}
