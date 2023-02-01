#' Setup errors and warnings to use crayon coloring
#'
#' @export

use_crayon <- function() {
  if (!suppressMessages({requireNamespace("crayon")})) stop("crayon package is not installed", call.=FALSE)
  options(error = quote({cat(crayon::red$bold((geterrmessage()))); if (!interactive()) q(status=1)}), show.error.messages=FALSE)
  globalCallingHandlers(
    warning = function(cnd) { 
      cnd$message <- crayon::magenta(conditionMessage(cnd))
      warning(cnd)
      tryInvokeRestart("muffleWarning")
    }
  )
}
