#' Compute percentile rank
#'
#' @param x   A vector
#'
#' @examples
#' prank(airquality$Ozone)
#' @export

prank <- function(x) {
  rank(x)/length(x)
}
