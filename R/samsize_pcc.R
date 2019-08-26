#' Sample size for classifier development
#'
#' Targets the probability of correct classification (PCC) to within a certain tolerance of the optimal PCC.
#'
#' Loosely based on the function `MKmisc::ssize.pcc()`, but more accurate.  I think it is also more accurate than the results presented by Dobbin (2008), but I'm not 100% clear on their method when n1 != n2.  When n1=n2, all methods agree.
#'
#' @param tolerance   Sample size is found such that the absolute value of \eqn{PCC(\infty) - PCC(n)} is less than `tolerance`.
#' @param stdfc       Standardized fold change (difference in means on log scale divided by standard deviation)
#' @param p           Proportion of less common class (default 0.5)
#' @param nfeat       Number of features
#' @param dfeat       Number of differential features
#'
#' @return Object of class `power.htest``, a list of the arguments augmented with method and note elements.
#'
#' @examples
#' samsize_pcc(0.1, 2, 0.3, 22000)
#'
#' @export

samsize_pcc <- function(tolerance, stdfc, p = 0.5, nfeat, dfeat = 20){
  pcc.n.a <- function(alpha, n, stdfc, nfeat, m){
    pow <- tpower(n1=n[1], n2=n[2], delta=stdfc, alpha = alpha)
    T1 <- stdfc*m*pow/2
    T2 <- sqrt(m*pow + alpha*(nfeat-m))
    pnorm(T1/T2)
  }
  n1 <- 2
  M <- dfeat
  repeat {
    n1 <- n1 + 1
    n2 <- floor(n1*(1-p)/p)
    #n2 <- n2 + 1
    #n1 <- floor(n2*p/(1-p))
    #if (n1 < 2) next
    n <- c(n1, n2)
    m <- seq_len(M)
    alpha <- numeric(M)
    for(mi in m){
      alpha[mi] <- optimize(f = pcc.n.a, interval = c(0, 1), maximum = TRUE,
                            n = n, stdfc = stdfc, nfeat = nfeat, m = mi,
                            tol = 1e-10)$maximum
    }
    pow <- tpower(n1=n[1], n2=n[2], delta=stdfc, alpha = alpha)
    T1 <- stdfc*m*pow/2
    T2 <- sqrt(m*pow + alpha*(nfeat-m))
    crit <- max(pnorm(stdfc*sqrt(m)/2) - pnorm(T1/T2))
    if (crit < tolerance) break
  }
  METHOD <- "Sample Size Planning for Developing Classifiers Using High Dimensional Data"

  res <- structure(list(tolerance = tolerance, p = p,
                        stdfc = stdfc,
                        nfeat = nfeat,
                        n1 = n[1], n2 = n[2],
                        method = METHOD),
                   class = "power.htest")
  res
}
