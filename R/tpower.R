#' Power for t tests, including arbitrary linear hypotheses
#'
#' @param n       Sample size (total).
#' @param delta   Effect size.
#' @param lam     Contrast.
#' @param b       Coefficient vector.
#' @param sd      Standard deviation of outcome.
#' @param alpha   Type I error rate.
#' @param w       Weights for unequal allocation (normalized to 1).
#' @param n1      Sample size for group 1.
#' @param n2      Sample size for group 2.
#' @param power   Desired power.
#' @param upper   Upper bound for `tsamsize()`; increase if tsamsize hits this bound.  Default: 5000.
#' @param ...     For `tsamsize()`, additional arguments to be passed to `tpower()`.
#'
#' @name tpower
#'
#' @examples
#' tpower(100, 0.5)
#' tsamsize(0.5)
#'
NULL

#' @describeIn tpower   Calculates power
#' @export
tpower <- function(n, delta, lam=c(1,-1), b=c(delta,0), sd=1, alpha=.05, w=rep(1,g), n1, n2) {
  g <- length(b)
  if (!missing(n1) & !missing(n2)) {
    if (g!=2) stop("b does not agree with n1/n2; use w instead")
    n <- n1+n2
    nn <- length(n)
    W <- matrix(c(n1,n2), byrow=TRUE, ncol=g, nrow=nn)
  } else {
    nn <- length(n)
    W <- matrix(w, byrow=TRUE, ncol=g, nrow=nn)
  }
  for (i in 1:nn) W[i,] <- W[i,]/sum(W[i,])
  df <- n-g

  power <- numeric(nn)
  for (i in 1:nn) {
    X <- diag(1/(n[i]*W[i,])) ## XtX^(-1)
    C <- qt(1-alpha/2,df[i])
    ncp <- abs(crossprod(lam,b)/(sd*sqrt(qd(lam,X))))
    power[i] <- 1-pt(C,df[i],ncp)
  }
  return(power)
}

#' @describeIn tpower   Calculates sample size
#' @export
tsamsize <- function(delta, b=c(delta,0), w=rep(1,g), power=.8, upper=5000,...) {
  g <- length(b)
  if (length(w) != g) stop("w does not match b")
  w <- w/sum(w)
  f <- function(n){tpower(n, b=b, ...)-power}
  n <- uniroot(f, interval=c(2*g, upper))$root
  nn <- ceiling(n*w)
  ##if (sd(nn) < .0000001) cat(nn[1]," in each group\n",sep="")
  ##else cat(paste("Group ",1:g,": ",nn,"\n",sep="",collapse=""))
  ##cat("Total n: ",ceiling(n),"\n",sep="")
  val <- w*n
  names(val) <- paste0("n", 1:g)
  val
}
