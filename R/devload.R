#' Load a package from its dev/ location (from source, not installed)
#'
#' @param pkg   Package name (character)
#'
#' @examples
#' \dontrun{
#' devload(ncvreg)
#' }
#' @export

devload <- function(pkg) {
  pkgname <- as.character(match.call()$pkg)
  devtools::load_all(paste0('~/dev/', pkgname))
}
