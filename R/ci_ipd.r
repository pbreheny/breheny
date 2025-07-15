#' Given a confidence interval, simulate individual patient data
#'
#' @param ci   Confidence interval
#' @param n    Sample size
#' @param df   Degrees of freedom (default: infinite)
#'
#' @returns A numeric vector of individual patient data
#'
#' @examples
#' ci_ipd(c(-0.31, -0.02), 10)
#' @export

ci_ipd <- function(ci, n, df = Inf) {
  se <- abs(ci[2] - ci[1]) / (2 * qt(0.975, df))
  sd <- se * sqrt(n)
  m <- mean(ci)
  list(
    mean = m,
    sd = sd,
    ipd = rnorm(n, m, sd)
  )
}
