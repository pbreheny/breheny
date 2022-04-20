#' Compute a quadratic form (x'Ax)
#'
#' @param x   A vector
#' @param A   A matrix compatible with x
#'
#' @examples
#' x <- rnorm(10)
#' A <- matrix(rnorm(100), 10, 10)
#' qd(x, A)
#' @export

qd <- function(x, A) {
  drop(crossprod(x, A) %*% x)
}
