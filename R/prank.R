#' Compute percentile rank
#'
#' @param x   A vector
#'
#' @examples
#' prank(airquality$Ozone)

prank <- function(x) {
  rank(x)/length(x)
}
