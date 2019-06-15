#' Power and sample size for time-to-event data based on exponential closed form solution
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
#' epower(30, 1.5)
#' esamsize(1.5)
#' @name epower
NULL

#' @describeIn epower   Calculate power
#' @export

epower <- function(n, r, alpha=.05, w=c(1,1), n1, n2, lam, cens) {
  if (!missing(lam)) r <- lam[1]/lam[2]
  if (!missing(n1) & !missing(n2)) {
    n <- n1+n2
    nn <- length(n)
    W <- matrix(c(n1,n2), ncol=2,nrow=nn)
  } else {
    nn <- length(n)
    W <- matrix(w,byrow=TRUE,ncol=2,nrow=nn)
  }
  for (i in 1:nn) W[i,] <- W[i,]/sum(W[i,])

  if (!missing(cens)) N <- n*W*lam/(lam+cens)
  else N <- W*n

  C <- qnorm(1-alpha/2)
  d <- abs(log(r)/sqrt(apply(1/N,1,sum)))
  if (!missing(cens) & nn==1) cat(paste("Group ",1:2,": ",round(nn)," events\n",sep="",collapse=""))
  return(1-pnorm(C,mean=d)+pnorm(-C,mean=d))
}

#' @describeIn epower   Calculate sample size
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
