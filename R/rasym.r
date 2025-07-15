#' Generate asymptotic sequence of random normal sums
#'
#' Returns the sum of bernoulli variables with different probabilities.
#'
#' @param n      Number of random variates to generate.
#' @param N      Maximum sample size.
#' @param len    Length of sequence; default: all
#' @param mu     Mean of normal distribution; default: 0
#' @param mean   Return means (instead of sums?); default: FALSE
#'
#' @examples
#' rasym(5, 100, 5)
#' rasym(7, 100, 5, mean = TRUE)
#'
#' @export
#'
#'

rasym <- function(n, N, len = N, mu = 0, mean = FALSE) {
  if (N %% len != 0) stop("N is not an even multiple of length", call. = FALSE)
  gap <- floor(N / len) # Should warn?
  nn <- seq(gap, N, gap)
  Y <- matrix(rnorm(n * len, sd = sqrt(gap)), len, n) + gap * mu
  if (mean) {
    Z <- apply(Y, 2, cumsum) / nn
  } else {
    Z <- apply(Y, 2, cumsum)
  }
  dimnames(Z) <- list(nn, 1:n)
  Z
}
