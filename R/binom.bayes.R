binom.bayes <- function(x, n, a=1, b=1, level=.95, plot=FALSE, add=FALSE, xlab="p", ylab="Posterior density", col="blue", ...)
{
  l.p <- qbeta((1-level)/2,a+x,n-x+b)
  u.p <- qbeta((1+level)/2,a+x,n-x+b)
  sample <- x/n
  mean.post <- (a+x)/(a+n+b)
  mode.post <- (a+x-1)/(a+n+b-2)
  var.post <- (a+x)*(b+n-x)/((a+b+n)^2*(a+b+n+1))
  ci.central <- c(l.p, u.p)
  f <- function(L) {
    l <- qbeta(L, a+x, n-x+b)
    u <- qbeta(L+level, a+x, n-x+b)
    u-l
  }
  L <- optimize(f, lower=0, upper=1-level, tol=1e-10)$minimum
  l.h <- qbeta(L, a+x, n-x+b)
  if (l.h < 1e-8) l.h <- 0
  u.h <- qbeta(L+level, a+x, n-x+b)
  if (u.h > 1-1e-8) u.h <- 1
  ci.hpd <- c(l.h, u.h)
  if (plot | add) {
    xx <- seq(0,1,len=399)
    if (add) {
      lines(xx, dbeta(xx,a+x,n-x+b), lwd=3, col=col, ...)
    } else {
      plot(xx,dbeta(xx,a+x,n-x+b), col=col, type="l", lwd=3, xlab=xlab, ylab=ylab, las=1, ...)
    }
  }
  structure(list(sample=sample, mean.post=mean.post, mode.post=mode.post, var.post=var.post, ci.central=ci.central, ci.hpd=ci.hpd, level=level), class="onepar.bayes")
}
print.onepar.bayes <- function(obj) {
  cat("Sample proportion:", obj$sample, "\n")
  cat("Posterior mean:", obj$mean, "\n")
  cat("Posterior mode:", obj$mode, "\n")
  cat("Posterior SD:", sqrt(obj$var), "\n")
  cat(100*obj$level,"% central interval: (",obj$ci.central[1],", ",obj$ci.central[2],")\n",sep="")
  cat(100*obj$level,"% HPD interval: (",obj$ci.hpd[1],", ",obj$ci.hpd[2],")\n",sep="")  
}
