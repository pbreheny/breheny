#' Power for regression (or any continuous outcome)
#'
#' @param rsq     Fraction of variance explained by the features to be tested
#' @param n       Sample size
#' @param k       Number of parameters to test
#' @param p       Number of parameters in model (default: k)
#' @param alpha   Type 1 error rate
#' @param power   Desired power
#'
#' @examples
#' rpower(0.05, 40)
#' rpower(0.05, 40, 5)
#' rpower(0.05, 40, 5, 20)
#' rpower(0.05, 40, 5, 20)
#' rsamsize(0.05)
#' @export

rpower <- function(rsq, n, k=1, p=k, alpha=0.05) {
  pf(qf(1-alpha, k, n-p), k, n-p, ncp=n*rsq, lower.tail=FALSE)
}

#' @rdname rpower
#' @export

rsamsize <- function(rsq, k=1, p=k, power=0.8, alpha=0.05) {
  u <- uniroot(rsamsize_f, c(2, 1e6), rsq=rsq, k=k, p=p, power=power, alpha=alpha)
  ceiling(u$root)
}

rsamsize_f <- function(n, rsq, k=1, p=k, power=0.8, alpha=0.05) {
  rpower(rsq, n, k, p, alpha) - power
}
