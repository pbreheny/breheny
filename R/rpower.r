#' Power for regression (or any continuous outcome)
#'
#' Power and sample size for testing `k` coefficients in a linear regression
#' model with `p` predictors (plus an intercept), based on the noncentral F
#' distribution. These match G*Power's "Linear multiple regression: Fixed
#' model, R² increase" calculation.
#'
#' Effect size is specified as the partial R², the fraction of variability
#' *remaining after adjusting for the other predictors* that is explained by
#' the `k` predictors being tested:
#' \deqn{R^2_{partial} = \frac{R^2_{full} - R^2_{reduced}}{1 - R^2_{reduced}}.}
#' Equivalently, Cohen's \eqn{f^2 = R^2_{partial} / (1 - R^2_{partial})}. The
#' noncentrality parameter is \eqn{\lambda = n f^2} with `k` and `n - p - 1`
#' degrees of freedom.
#'
#' For a single binary predictor with balanced groups, \eqn{R^2_{partial} =
#' d^2 / (d^2 + 4)}, where `d` is Cohen's d; in this case, `rpower()` with
#' `p = 1` agrees exactly with a two-sample t-test.
#'
#' `rpower_beta()` converts between partial R² and the corresponding
#' standardized regression coefficient (the coefficient for a predictor with
#' unit variance when the residual standard deviation is 1).
#'
#' @param rsq     Partial R² of the predictors to be tested (see Details)
#' @param n       Sample size
#' @param k       Number of parameters to test
#' @param p       Number of predictors in the model, not counting the
#'   intercept (default: `k`)
#' @param alpha   Type 1 error rate
#' @param power   Desired power
#'
#' @returns `rpower()` returns power; `rsamsize()` returns the smallest sample
#'   size achieving the desired power.
#'
#' @examples
#' rpower(0.05, 40)
#' rpower(0.05, 40, 5)
#' rpower(0.05, 40, 5, 20)
#' rsamsize(0.05)
#'
#' # Cohen's d = 0.5, n = 128: same as a two-sample t-test with 64 per group
#' d <- 0.5
#' rpower(d^2 / (d^2 + 4), 128)
#'
#' rpower_beta(0.1)
#' rpower_beta(beta = 0.13)
#' @export

rpower <- function(rsq, n, k = 1, p = k, alpha = 0.05) {
  df2 <- n - p - 1
  pf(qf(1 - alpha, k, df2), k, df2, ncp = n * rsq / (1 - rsq), lower.tail = FALSE)
}

#' @rdname rpower
#' @export

rsamsize <- function(rsq, k = 1, p = k, power = 0.8, alpha = 0.05) {
  u <- uniroot(rsamsize_f, c(p + 2, 1e6), rsq = rsq, k = k, p = p, power = power, alpha = alpha)
  ceiling(u$root)
}

rsamsize_f <- function(n, rsq, k = 1, p = k, power = 0.8, alpha = 0.05) {
  rpower(rsq, n, k, p, alpha) - power
}

#' @rdname rpower
#' @param beta   Standardized regression coefficient
#' @export

rpower_beta <- function(rsq, beta) {
  if (missing(beta)) {
    sqrt((rsq) / (1 - rsq))
  } else {
    (beta^2) / (1 + beta^2)
  }
}
