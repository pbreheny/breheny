#' Power for t tests, including arbitrary linear hypotheses
#'
#' @param n       Sample size (total).
#' @param delta   Effect size; if specified, a two-group design is assumed.  If more than two groups, use `b` instead.
#' @param lam     Contrast.
#' @param b       If a vector, the coefficient for each group.  If a matrix, must have `length(n)` rows.
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
#' tpower(10*(6:9), 0.5)                # Vectorize sample size
#' tpower(100, seq(0.25, 1, by=0.25))   # Vectorize effect size
#' tpower(100, b=c(0.5, 0.5, 0), lam=c(1,1,-1))   # A multi-group example
#' tsamsize(0.5)
#'
NULL

#' @describeIn tpower   Calculates power
#' @export

tpower <- function(n, delta, lam=c(1,-1), b, sd=1, alpha=.05, w=rep(1,g), n1, n2) {

  # Determine N / check for agreement
  if (!missing(n1) & !missing(n2)) n <- n1+n2
  nn <- length(n)
  if (!missing(b)) {
    nd <- nrow(b)
  } else {
    nd <- length(delta)
  }
  ns <- length(sd)
  N <- max(nn, nd, ns)
  if (length(setdiff(c(nn, nd, ns), c(1, N)))) {
    stop('n/delta/b/sigma must either be length 1 or same length as longest vector')
  }

  # Check for agreement among arguments / recycle
  n <- rep_len(n, N)
  sd <- rep_len(sd, N)
  if (!missing(delta) & !missing(b)) {
    stop('Specify only one of "delta" and "b"')
  }
  if (missing(b)) {
    g <- 2
    B <- matrix(NA, nrow=N, ncol=2)
    B[,1] <- rep_len(delta, N)
    B[,2] <- 0
  } else if (is.matrix(b)) {
    g <- ncol(b)
    B <- matrix(NA, nrow=N, ncol=g)
    for (j in 1:g) B[,j] <- rep_len(b[,1], N)
  } else {
    g <- length(b)
    B <- matrix(b, nrow = N, ncol = g, byrow = TRUE)
  }
  if (!missing(n1) & !missing(n2)) {
    if (g!=2) stop("b does not agree with n1/n2; use w instead")
  }

  # Construct W
  if (!missing(n1) & !missing(n2)) {
    W <- matrix(c(n1,n2), byrow=TRUE, ncol=g, nrow=N)
  } else {
    W <- matrix(w, byrow=TRUE, ncol=g, nrow=N)
  }
  for (i in 1:N) W[i,] <- W[i,]/sum(W[i,])

  # Calculate power
  df <- n-g
  power <- numeric(N)
  for (i in 1:N) {
    X <- diag(1/(n[i]*W[i,])) ## XtX^(-1)
    C <- qt(1-alpha/2,df[i])
    ncp <- abs(crossprod(lam, B[i,])/(sd[i]*sqrt(qd(lam,X))))
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
