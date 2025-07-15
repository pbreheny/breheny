#' Generate random bernoulli (non-homogeneous binomial) variates
#'
#' Returns the sum of bernoulli variables with different probabilities.
#'
#' @param N      Number of random variates to generate.
#' @param prob   Vector of probabilities.
#'
#' @examples
#' rbern(5, c(.3, .6, .1))
#'
#' @export

rbern <- function(N, prob) {
  n <- length(prob)
  X <- matrix(rbinom(N * n, 1, prob), nrow = n, ncol = N)
  return(apply(X, 2, sum))
}
rnhb <- function(N, prob) {
  n <- length(prob)
  U <- matrix(runif(N * n), nrow = n)
  return(apply(U < prob, 2, sum))
}
