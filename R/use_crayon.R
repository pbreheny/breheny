#' Setup errors and warnings to use crayon coloring
#'
#' @export

use_crayon <- function() {
  suppressMessages({library(crayon)})
  options(error = quote(cat(red$bold((geterrmessage())))), show.error.messages=FALSE)
  options(warning.expression = quote(cat(magenta$bold(paste0("Warning: ", names(warnings()), ": ", as.character(warnings()), "\n")))))
}
