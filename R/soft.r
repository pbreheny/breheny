#' Soft-thresholding operator
#'
#' @param x   The thing to be thresholded
#' @param l   The threshold; must either be length 1 or same langth as x
#'
#' @examples
#' soft(-10:10, 2.5)
#' soft(rep(5.5, 10), 1:10)
#' soft(matrix(1:30, ncol = 3), 10)
#'
#' @export

soft <- function(x, l) {
  if (length(l) != 1 & length(l) != length(x)) stop("Threshold must either be a scalar or same length as x", call. = FALSE)
  sign(x) * (abs(x) > l) * (abs(x) - l)
}
