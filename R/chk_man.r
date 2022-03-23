#' Check code/documentation agreement using `devtools::check_man()`
#'
#' Intended to be used as an Rstudio addin (which is why it doesn't accept
#' arguments).
#'
#' @export

chk_man <- function() {
  devtools::check_man()
}
