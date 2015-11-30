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
