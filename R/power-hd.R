#' Power calculator for high-dimensional testing
#'
#' @param n   Sample size (per group)
#' @param p   Number of features (genes)
#' @param d   Number of differential features (differentially expressed genes)
#' @param m   Minimum difference among differential features, in terms of mean difference divided by SD (effect size)
#' @param FDR   Target FDR
#'
#' @examples
#' power_hd(10, 1000, 100, 1)
#' power_hd(seq(10, 100, by=10), 1000, 100, 1)
#'
#' @export

power_hd <- function(n, p, d, m, FDR=0.1) {
  N <- length(n)
  Hits <- Power <- numeric(N)
  for (i in 1:N) {
    f <- function(a) {
      bb <- tpower(n=2*n[i], delta=m, alpha=a)
      a*p / (a*(p-d) + bb*d) - FDR
    }
    a <- uniroot(f, c(1e-6, 0.9))$root
    b <- tpower(n=2*n[i], delta=m, alpha=a)
    Power[i] <- b
    Hits[i] <- b*d
  }
  list(Hits=Hits, Power=Power)
}
