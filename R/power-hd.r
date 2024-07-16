#' Power calculator for high-dimensional testing
#'
#' The function sets up a root-finding problem in which the appropriate per-test
#' cutoff alpha is found that satisfies the supplied FDR (in expectation), then
#' reports overall power for tests carried out at that alpha level. Vectorized
#' over n, but nothing else.
#'
#' **Further reading:**
#' * Pawitan2005: Original idea
#' * wiki: Derivation and explanation
#'
#' @param n     Sample size (per group)
#' @param p     Number of features (genes)
#' @param d     Number of differential features (differentially expressed genes)
#' @param m     Minimum difference among differential features, in terms of mean difference divided by SD (effect size)
#' @param FDR   Target FDR
#'
#' @return A list containing three items:
#' * `expected_hits`: Expected number of discoveries (marginal power multiplied by number of differential features)
#' * `marginal_power`: Power to detect a given feature (function assumes this is the same for all features)
#' * `disjunctive_power`: Power to detect at least one feature
#'
#' @examples
#' power_hd(10, 1000, 100, 1)
#' power_hd(seq(10, 100, by=10), 1000, 100, 1)
#' power_hd(300, 100000, 50, 0.2)
#' @export

power_hd <- function(n, p, d, m, FDR=0.1) {
  N <- length(n)
  expected_hits <- marginal_power <- disjunctive_power <- numeric(N)
  for (i in 1:N) {
    f <- function(log_a) {
      a <- exp(log_a)
      bb <- tpower(n=2*n[i], delta=m, alpha=a, verbose=FALSE)
      a*p / (a*(p-d) + bb*d) - FDR
    }
    if (f(-20) > 0) {
      marginal_power[i] <- expected_hits[i] <- disjunctive_power[i] <- 0
    } else {
      log_a <- uniroot(f, c(-20, -0.01))$root
      b <- tpower(n=2*n[i], delta=m, alpha=exp(log_a), verbose=FALSE)
      marginal_power[i] <- b
      expected_hits[i] <- b*d
      disjunctive_power[i] <- 1-(1-b)^d
    }
  }
  list(expected_hits = expected_hits,
       marginal_power = marginal_power,
       disjunctive_power = disjunctive_power)
}
