#' Run `devtools::load_all()` silently
#'
#' Intended to be used as an Rstudio addin (which is why it doesn't accept
#' arguments).
#'
#' @export

silent_load <- function() {
  suppressPackageStartupMessages(devtools::load_all())
}
