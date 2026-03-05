#' Power for a one-sample binomial test
#'
#' Test is two-sided.
#'
#' @param n Sample size
#' @param p0 Null hypothesis proportion
#' @param p1 Alternative hypothesis proportion
#' @param alpha Type 1 error rate (default: 0.05)
#' @param approx Use z approximation? (default: false)
#'
#' @examples
#' binom_power(200, 0.10, 0.05)
#' binom_power(200, 0.90, 0.95)
#' binom_power(200, 0.90, 0.95, approx = TRUE)
#' @export

binom_power <- function(n, p0, p1, alpha = 0.05, approx = FALSE) {
  if (approx) {
    cv <- qnorm(alpha / 2, max(p0, p1), sd = sqrt(p0 * (1 - p0) / n))
    power <- pnorm(cv, min(p0, p1), sd = sqrt(p1 * (1 - p1) / n))
  } else if (p1 < p0) {
    cv <- qbinom(alpha / 2, n, p0) - 1
    power <- pbinom(cv, n, p1)
  } else {
    cv <- qbinom(1 - alpha / 2, n, p0)
    power <- pbinom(cv, n, p1, lower.tail = FALSE)
    cv <- cv + 1
  }
  power
}
