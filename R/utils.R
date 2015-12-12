#' match_arg is a better match.arg
#'
#' A simple wrapper around match.arg that returns a slightly
#' better error message when used programmatically.
match_arg <- function(arg, choices, several.ok = FALSE) {
  param_name <- deparse(substitute(arg))
  tryCatch(match.arg(arg, choices, several.ok), error = function(e) {
    stop(gsub("'arg'", paste0("'", param_name, "'"), as.character(e)))
  })
}

`%||%` <- function(x, y) { if (is.null(x)) { y } else { x } }

is_single_numeric <- function(x) {
  is.numeric(x) && identical(length(x), 1L) && !is.na(x)
}

validate_positive_numeric <- function(x, var = 'arg') {
  stop_if_not(is_single_numeric(x) && (x > 0), paste0(var, ' must be a positive numeric.')
  x
}

validate_simple_function <- function(x, param) {
  if (!is.function(x)) stop(param, ' must be a function')
  x
}

stop_if_not <- function(condition, error_message) {
  if (!isTRUE(condition)) stop(error_message)
}
