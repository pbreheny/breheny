#' Copies a generic DESCRIPTION file to current working directory
#'
#' @export

generic_description <- function() {
  x <- readLines(system.file("generic-description.txt", package="breheny"))
  x[grep('^Package', x)] <- paste0('Package: ', basename(getwd()))
  x[grep('^Date', x)] <- paste0('Date: ', Sys.Date())
  writeLines(x, 'DESCRIPTION')
}
