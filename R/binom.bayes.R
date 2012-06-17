binom.bayes <- function(x,n,a=1,b=1,ci=.95,plot=FALSE)
  {
    l.p <- qbeta((1-ci)/2,a+x,n-x+b)
    u.p <- qbeta((1+ci)/2,a+x,n-x+b)
    cat("Sample proportion:",x/n,"\n")
    cat("Posterior mean:",(a+x)/(a+n+b),"\n")
    cat("Posterior mode:",(a+x-1)/(a+n+b-2),"\n")
    cat(100*ci,"% percentile interval: (",l.p,", ",u.p,")\n",sep="")
    f <- function(l) {
      y <- qbeta(pbeta(l,a+x,n-x+b)+ci,a+x,n-x+b)
      dbeta(y,a+x,n-x+b)-dbeta(l,a+x,n-x+b)
    }
    l.h <- uniroot(f,c(0,qbeta(1-ci,a+x,n-x+b)))$root
    u.h <- qbeta(pbeta(l.h,a+x,n-x+b)+ci,a+x,n-x+b)
    cat(100*ci,"% HPD interval: (",l.h,", ",u.h,")\n",sep="")
    if (plot) {
      xx <- seq(0,1,len=99)
      plot(xx,dbeta(xx,a+x,n-x+b),col="blue",type="l",lwd=3,xlab="p",ylab="Posterior density")
    }
  }
