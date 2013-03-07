binom.bayes <- function(x, n, a=1, b=1, level=.95, plot=FALSE, add=FALSE, xlab="p", ylab="Posterior density", col="blue", ...)
{
  A <- a+x
  B <- n-x+b
  l.p <- qbeta((1-level)/2, A, B)
  u.p <- qbeta((1+level)/2, A, B)
  sample <- x/n
  mean.post <- A/(A+B)
  var.post <- (a+x)*(b+n-x)/((a+b+n)^2*(a+b+n+1))
  ci.central <- c(l.p, u.p)
  f <- function(L) {
    l <- qbeta(L, a+x, n-x+b)
    u <- qbeta(L+level, a+x, n-x+b)
    u-l
  }
  if (A > 1 & B > 1) {
    mode.post <- (A-1)/(A+B-2)
    L <- optimize(f, lower=0, upper=1-level, tol=1e-10)$minimum
    l.h <- qbeta(L, a+x, n-x+b)
    if (l.h < 1e-8) l.h <- 0
    u.h <- qbeta(L+level, a+x, n-x+b)
    if (u.h > 1-1e-8) u.h <- 1    
  } else if (A > B) {
    mode.post <- 1
    l.h <- qbeta(1-level, A, B)
    u.h <- 1
  } else {
    mode.post <- 0
    l.h <- 0
    u.h <- qbeta(level, A, B)
  }
  ci.hpd <- c(l.h, u.h)
  if (plot | add) {
    xx <- seq(0,1,len=399)
    if (add) {
      lines(xx, dbeta(xx,a+x,n-x+b), lwd=3, col=col, ...)
    } else {
      plot(xx,dbeta(xx, A, B), col=col, type="l", lwd=3, xlab=xlab, ylab=ylab, las=1, ...)
    }
  }
  structure(list(sample=sample, mean.post=mean.post, mode.post=mode.post, var.post=var.post, ci.central=ci.central, ci.hpd=ci.hpd, level=level), class="onepar.bayes")
}
print.onepar.bayes <- function(obj) {
  cat("Sample proportion:", formatC(obj$sample, digits=3, format="f"), "\n")
  cat("Posterior mean:", formatC(obj$mean, digits=3, format="f"), "\n")
  cat("Posterior mode:", formatC(obj$mode, digits=3, format="f"), "\n")
  cat("Posterior SD:", formatC(sqrt(obj$var), digits=3, format="f"), "\n")
  cat(100*obj$level,"% central interval: (",formatC(obj$ci.central[1], digits=3, format="f"),", ",formatC(obj$ci.central[2], digits=3, format="f"),")\n",sep="")
  cat(100*obj$level,"% HPD interval: (",formatC(obj$ci.hpd[1], digits=3, format="f"),", ",formatC(obj$ci.hpd[2], digits=3, format="f"),")\n",sep="")  
}
