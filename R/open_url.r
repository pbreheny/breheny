#' Open a url: quietly, in the background, and in a new window
#'
#' @param x   URL to open
#'
#' @examples
#' \donttest{
#' open_url("www.google.com")
#' }
#' @export

open_url <- function(x) {
  system2("sensible-browser", c("--new-window", x), stdout = NULL, stderr = NULL, wait = FALSE)
}
