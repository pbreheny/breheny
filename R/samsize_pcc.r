#' Sample size for classifier development
#'
#' Determine the sample size necessary to estimate the probability of correct
#' classification (PCC) to within a certain tolerance of the optimal (Bayes)
#' PCC.
#'
#' Assumes a multivariate normal distribution with spherical variance.
#' Loosely based on the function `MKmisc::ssize.pcc()`, but with two primary
#' differences:
#' 1. Doesn't solve for worst case scenario over 1:dfeat, just uses dfeat.
#' 2. Uses `tpower()` rather than approximating the power of the t-test.
#'
#' @param effect      Effect size (difference in means divided by SD)
#' @param tolerance   Sample size is found such that \eqn{PCC(\infty) - PCC(n)} is less than `tolerance`.
#' @param p           Proportion of less common class (default 0.5)
#' @param nfeat       Number of features
#' @param dfeat       Number of differential features. Note that Dobbin & Simon recommend using dfeat=1.
#'
#' @return Object of class `power.htest``, a list of the arguments augmented with method and note elements.
#'
#' @examples
#' samsize_pcc(0.5, 0.001)
#' samsize_pcc(1, 0.1, nfeat=22000)
#' samsize_pcc(0.8, 0.1, p=1/3, nfeat=22000, dfeat=20)
#' @export

samsize_pcc <- function(effect, tolerance, p = 0.5, nfeat = 1, dfeat = 1) {
  delta <- effect/2
  q <- 1 - p
  k <- 1/2 * log((1-p)/p)
  m <- dfeat
  n1 <- 2
  repeat {
    n1 <- n1 + 1
    n2 <- floor(n1*(1-p)/p)
    n <- c(n1, n2)
    pcc_n <- optimize(f = pcc_lower_bound, interval = c(0, 1), maximum = TRUE,
                      n = n, delta = delta, nfeat = nfeat, m = m, p=p,
                      tol = 1e-10)$objective
    pcc_inf <- p * pnorm((m * delta^2 - k) / sqrt(m * delta^2)) + q * pnorm((m * delta^2 + k) / sqrt(m * delta^2))
    if (pcc_inf - pcc_n < tolerance) break
  }

  structure(
    list(
      tolerance = tolerance,
      p = p,
      effect = effect,
      nfeat = nfeat,
      dfeat = dfeat,
      pcc_inf = pcc_inf,
      pcc_n = pcc_n,
      n1 = n[1], n2 = n[2],
      method = "Sample Size Planning for Developing Classifiers Using High Dimensional Data"),
    class = "power.htest")
}

pcc_lower_bound <- function(alpha, n, delta, nfeat, m, p) {
  k <- 1/2 * log((1-p)/p)
  pow <- tpower(n1=n[1], n2=n[2], delta=2*delta, alpha = alpha, verbose=FALSE)
  q <- 1 - p
  p*pnorm((delta * m * pow - k) / sqrt(m * pow + (nfeat - m) * alpha)) +
    q*pnorm((k + delta * m * pow) / sqrt(m * pow + (nfeat - m) * alpha))
}
