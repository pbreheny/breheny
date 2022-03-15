#' Catch warnings
#'
#' @examples
#' out <- catchWarning(log('a'))
#' out$value
#' out <- catchWarning(as.numeric(c('1', 'a')))
#' out
#' out <- catchWarning(1+1)
#' out
#' @export

catchWarning <- function(expr) {
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler),
       warning = W)
}
