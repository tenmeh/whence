# Stage decomposition.
#
# This file manipulates expressions only. It must not refer to dplyr or to
# shiny, and it must not evaluate anything. That keeps the hardest logic of
# the package testable in isolation.
#
# The native pipe is syntactic: `x |> f() |> g()` parses to `g(f(x))`, and
# the parsed call holds no trace of the pipe. A pipeline written with `|>`
# thus cannot be told apart from nested calls. The decomposition therefore
# knows a list of verbs, and treats the first argument of a verb call as the
# data that comes from upstream. The magrittr pipe is different: `%>%` is a
# real call, so each of its right-hand sides is a stage.


# The verbs whose row semantics whence knows. A call to one of these is a
# stage. Its first argument is the upstream data.
whence_verbs <- c(
  "filter", "mutate", "transmute", "select", "rename", "arrange",
  "slice", "slice_head", "slice_tail", "slice_min", "slice_max",
  "slice_sample", "group_by", "ungroup", "summarise", "summarize",
  "count", "tally", "distinct", "drop_na", "head", "tail",
  "pivot_longer", "pivot_wider", "pull"
)

# The names that the first argument of a verb can have. The native pipe puts
# the data first and unnamed, but a hand-written call can name it.
data_arg_names <- c("", ".data", "data", "x")

# The symbol that stands for the upstream data inside a stage. Each stage is
# evaluated in a child environment where this symbol is bound.
placeholder_sym <- as.symbol(".whence_data")


#' Split a pipeline expression into a source and its stages
#'
#' @param expr An unevaluated expression.
#' @return A list with `source` (an expression) and `stages` (a list). Each
#'   stage is a list with `call` (the call, with the placeholder where the
#'   upstream data goes), `verb` (the function name, or `NA`), `dot` (`TRUE`
#'   when a magrittr `.` gave the position of the data) and `magrittr`
#'   (`TRUE` when the stage came from `%>%`).
#' @noRd
decompose_pipeline <- function(expr) {
  stages <- list()

  repeat {
    if (is_magrittr_pipe(expr)) {
      stage <- magrittr_stage(expr[[3]])
      if (is.null(stage)) {
        break
      }
      stages <- c(list(stage), stages)
      expr <- expr[[2]]
      next
    }

    if (is_verb_call(expr)) {
      stages <- c(list(verb_stage(expr)), stages)
      expr <- expr[[2]]
      next
    }

    break
  }

  list(source = expr, stages = stages)
}


#' Is `expr` a call to the magrittr pipe?
#' @noRd
is_magrittr_pipe <- function(expr) {
  is.call(expr) &&
    length(expr) == 3 &&
    identical(safe_call_name(expr), "%>%")
}


#' Is `expr` a call to a known verb, with data in its first argument?
#' @noRd
is_verb_call <- function(expr) {
  if (!is.call(expr) || length(expr) < 2) {
    return(FALSE)
  }
  name <- safe_call_name(expr)
  if (is.null(name) || !name %in% whence_verbs) {
    return(FALSE)
  }
  arg_names <- names(expr) %||% rep("", length(expr))
  arg_names[[2]] %in% data_arg_names
}


#' The function name of a call, or `NULL`
#'
#' `rlang::call_name()` resolves `dplyr::filter(...)` to `"filter"`. It gives
#' `NULL` for a call whose function is not a name, such as `f()()`.
#' @noRd
safe_call_name <- function(expr) {
  if (!is.call(expr)) {
    return(NULL)
  }
  rlang::call_name(expr)
}


#' Make a stage from a verb call, by putting the placeholder in argument 1
#' @noRd
verb_stage <- function(expr) {
  expr[[2]] <- placeholder_sym
  new_stage(expr, dot = FALSE, magrittr = FALSE)
}


#' Make a stage from the right-hand side of `%>%`
#'
#' This follows the rules of magrittr:
#'
#' * a bare symbol, as in `x %>% collect`, becomes a call: `collect(.)`.
#' * when `.` is one of the arguments, the data goes there and nothing is
#'   put first.
#' * otherwise the data goes first.
#'
#' A right-hand side that magrittr treats in a special way, such as a block
#' in braces or a function in parentheses, is not split. The function gives
#' `NULL`, and the pipe and all that is upstream of it become the source.
#' @noRd
magrittr_stage <- function(rhs) {
  if (is.symbol(rhs)) {
    return(new_stage(as.call(list(rhs, placeholder_sym)), FALSE, TRUE))
  }

  if (!is.call(rhs)) {
    return(NULL)
  }
  if (is.null(safe_call_name(rhs)) || is_special_rhs(rhs)) {
    return(NULL)
  }

  args <- as.list(rhs)[-1]
  is_dot <- vapply(args, identical, logical(1), quote(.))
  if (any(is_dot)) {
    for (i in which(is_dot)) {
      rhs[[i + 1]] <- placeholder_sym
    }
    return(new_stage(rhs, dot = TRUE, magrittr = TRUE))
  }

  fn <- rhs[[1]]
  rhs <- as.call(c(list(fn, placeholder_sym), args))
  new_stage(rhs, dot = FALSE, magrittr = TRUE)
}


#' Does magrittr treat this right-hand side in a special way?
#' @noRd
is_special_rhs <- function(rhs) {
  identical(rhs[[1]], quote(`{`)) ||
    identical(rhs[[1]], quote(`(`)) ||
    identical(rhs[[1]], quote(`function`)) ||
    identical(rhs[[1]], quote(`$`))
}


#' @noRd
new_stage <- function(call, dot, magrittr) {
  list(
    call = call,
    verb = safe_call_name(call) %||% NA_character_,
    dot = dot,
    magrittr = magrittr
  )
}


#' The stage call as the user would write it in a pipe
#'
#' For a stage whose data goes first, argument 1 is dropped:
#' `filter(.whence_data, a > 1)` gives `filter(a > 1)`. For a stage whose
#' data went to a magrittr `.`, the placeholder is shown as `.`.
#' @noRd
stage_display_call <- function(stage) {
  call <- stage$call
  if (stage$dot) {
    return(replace_symbol(call, placeholder_sym, quote(.)))
  }
  call[-2]
}


#' Replace each occurrence of one symbol in an expression
#' @noRd
replace_symbol <- function(expr, from, to) {
  if (identical(expr, from)) {
    return(to)
  }
  if (is.call(expr)) {
    for (i in seq_along(expr)) {
      if (!is.null(expr[[i]]) && !identical(expr[[i]], quote(expr = ))) {
        expr[[i]] <- replace_symbol(expr[[i]], from, to)
      }
    }
  }
  expr
}
