#' Power and sample size for time-to-event data based on exponential closed form solution
#'
#' Power is calculated for two-sample studies.  Can be vectorized, but `w`, `lam`, `cens`, must always be length 2.
#'
#' @param n       Total sample size.
#' @param r       Effect size (ratio of hazards/medians).
#' @param alpha   Type I error rate.
#' @param w       Controls sample size balance between two groups.  Default: Equal size groups.
#' @param n1,n2   Sample size in each group.
#' @param lam     Hazard rate in each group (if specified, overrides `r`).
#' @param cens    Censoring rate.
#' @param power   Desired power.  Default: 0.8.
#'
#' @examples
#' epower(30, 2)
#' epower(30, lam=c(2,1), cens=c(0.2, 0.5))
#' epower(seq(30, 100, 10), lam=c(2,1), cens=c(0.2, 0.5))
#' epower(seq(30, 100, 10), w=c(2,1), lam=c(2,1), cens=c(0.2, 0.5))
#' epower(30, seq(1.5, 2.5, 0.1))
#' epower(30, 2, alpha=c(0.01, 0.05, 0.1))
#' esamsize(1.5)
#' @export

epower <- function(n, r, alpha=.05, w=c(1,1), n1, n2, lam, cens) {

  # Determine N / check for agreement
  if (!missing(n1) & !missing(n2)) {
    if (length(n1) != length(n2)) stop('Length of n1 must equal length of n2', call.=FALSE)
    n <- n1 + n2
  } else {
    if (length(w) != 2) stop('w must be length 2', call.=FALSE)
    w <- w/sum(w)
    n1 <- round(w[1] * n)
    n2 <- round(w[2] * n)
  }
  if (!missing(lam)) {
    if (length(lam) != 2) stop('lam must be length 2', call.=FALSE)
    r <- lam[1]/lam[2]
  }
  nn <- length(n)
  nr <- length(r)
  na <- length(alpha)
  M <- max(nn, nr, na)
  if (length(setdiff(c(nn, nr, na), c(1, M)))) {
    stop('n/r/alpha must either be length 1 or same length as longest vector')
  }

  # Check for agreement among arguments / recycle
  n <- rep_len(n, M)
  n1 <- rep_len(n1, M)
  n2 <- rep_len(n2, M)
  r <- rep_len(r, M)
  alpha <- rep_len(alpha, M)
  N <- matrix(c(n1, n2), ncol=2, nrow=M)
  if (!missing(cens)) N <- sweep(N, 2, lam/(lam+cens), '*')

  C <- qnorm(1-alpha/2)
  d <- abs(log(r) / sqrt(apply(1/N, 1, sum)))
  if (M==1) cat(paste("Group ", 1:2, ": ", round(N), " events\n", sep="", collapse=""))
  return(1 - pnorm(C, d) + pnorm(-C, d))
}

#' @rdname epower
#' @export

esamsize <- function(r, w=c(1,1), alpha=.05, power=.8, lam, cens) {
  w <- w/sum(w)
  if (!missing(lam)) r <- lam[1]/lam[2]
  a <- w[1]/w[2]
  nn <- numeric(2)
  nn[1] <- (1+a)*(qnorm(1-alpha/2)+qnorm(power))^2/(log(r))^2
  nn[2] <- nn[1]/a
  if (!missing(cens)) {
    nn <- nn*(lam+cens)/lam
  }
  nn <- ceiling(nn)
  if (sd(nn) < .0000001) cat("Group: ",nn[1],"\n",sep="")
  else cat(paste("Group ",1:2,": ",nn,"\n",sep="",collapse=""))
  if (!missing(cens)) cat("Total n: ",sum(nn),"\n",sep="")
  else cat("Total events: ",sum(nn),"\n",sep="")
  return(invisible(sum(nn)))
}
