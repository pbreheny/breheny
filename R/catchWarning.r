#' Catch warnings
#'
#' This is mainly used for code that either (a) generates a bunch of warnings, which makes it hard
#' to find a warning that may have happened 78 warnings ago, or (b) generates a warning and an error,
#' but it's hard to get at the warning because the error causes the code to crash before the warning
#' is signalled.
#'
#' @param expr   An R expression
#'
#' @examples
#' out <- catchWarning(log("a"))
#' out$value
#' out <- catchWarning(as.numeric(c("1", "a")))
#' out
#' out <- catchWarning(1 + 1)
#' out
#' @export

catchWarning <- function(expr) {
  W <- NULL
  w.handler <- function(w) { # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(
    value = withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler),
    warning = W
  )
}
