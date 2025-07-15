#' Compute percentile rank
#'
#' @param x   A vector
#'
#' @examples
#' x <- c(1, 2, NA, 10)
#' prank(x)
#' prank(airquality$Ozone)
#' @export

prank <- function(x) {
  rank(x, na.last = "keep") / sum(!is.na(x))
}
