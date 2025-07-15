#' Generate random variates from the Dirichlet distribution
#'
#' @param n      Number of random variates to generate (single integer)
#' @param alpha  Concentration parameter (vector of positive numbers)
#'
#' @returns An `n` by `d` matrix, where `d` is the length of `alpha`.
#'
#' @examples
#' rdir(10, 1:4)
#' @export

rdir <- function(n, alpha) {
  d <- length(alpha)
  Y <- matrix(rgamma(n * d, alpha, 1), d, n)
  t(sweep(Y, 2, colSums(Y), "/"))
}
