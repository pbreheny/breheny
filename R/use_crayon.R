#' Setup errors and warnings to use crayon coloring
#'
#' @export

use_crayon <- function() {
  if (!suppressMessages({requireNamespace("crayon")})) stop("crayon package is not installed", call.=FALSE)
  options(error = quote({cat(crayon::red$bold((geterrmessage()))); if (!interactive()) q()}), show.error.messages=FALSE)
  #options(warning.expression = quote(cat(crayon::magenta$bold(paste0("Warning: ", names(warnings()), ": ", as.character(warnings()), "\n")))))
}
