#' Generate a nice palette of colors
#'
#' @param n       Number of colors to generate, evenly spaced on hcl scale
#' @param alpha   Partial transparency
#'
#' @examples
#' pal(3)
#'
#' @export

pal <- function(n, alpha = 1) {
  if (n == 2) {
    val <- hcl(seq(15, 375, len = 4), l = 60, c = 150, alpha = alpha)[c(1, 3)]
  } else {
    val <- hcl(seq(15, 375, len = n + 1), l = 60, c = 150, alpha = alpha)[1:n]
  }
  val
}
