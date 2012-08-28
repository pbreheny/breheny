cvTree <- function(formula, Data, y, ..., nfolds=10, seed, p=seq(.01,.2,.01)) {
  if (!missing(seed)) set.seed(seed)
  n <- length(y)
  E <- matrix(NA, nrow=n, ncol=length(p))
  for (j in 1:length(p)) E[,j] <- cvTree1(formula, Data, y, control=ctree_control(testtype="Univariate", mincriterion=1-p[j]), ..., nfolds=nfolds)
  cve <- apply(E, 2, mean)
  cvse <- apply(E, 2, sd) / sqrt(n)
  min <- which.min(cve)
  list(cve=cve, cvse=cvse, min=min, p=p, p.min=p[min])
}
plotCV <- function(x, rev=FALSE, ...)
{
  L <- x$cve - x$cvse
  U <- x$cve + x$cvse
  xx <- x$p
  xlim <- if (rev) rev(range(xx)) else range(xx)
  
  plot.args = list(x=x$p, y=x$cve, ylim=range(c(L,U)), xlab="Complexity", ylab="Cross-validation error", type="n", xlim=xlim)
  new.args = list(...)
  if (length(new.args)) plot.args[names(new.args)] = new.args
  do.call("plot", plot.args)
  abline(v=xx[x$min],lty=2,lwd=.5)
  arrows(x0=xx,x1=xx,y0=L,y1=U,code=3,angle=90,col="gray80",length=.05)
  points(xx,x$cve,col="red",pch=19,cex=.5)
  
}
cvTree1 <- function(formula, Data, y, ..., nfolds)
{
  E <- numeric(length(y))
  classification <- class(y) %in% c("factor", "logical")
  n <- length(y)
  if (classification) {
    yy <- y
    if (class(y)=="factor") y <- as.numeric(y) - 1
    if (class(y)=="logical") y <- as.numeric(y)
    if (identical(sort(unique(as.numeric(Data$Success)-1)),0:1)) stop("Multinomial CV not implemented yet")
    ind1 <- which(y==1)
    ind0 <- which(y==0)
    n1 <- length(ind1)
    n0 <- length(ind0)
    cv.ind1 <- ceiling(sample(1:n1)/n1*nfolds)
    cv.ind0 <- ceiling(sample(1:n0)/n0*nfolds)
    cv.ind <- numeric(n)
    cv.ind[y==1] <- cv.ind1
    cv.ind[y==0] <- cv.ind0        
  } else if (class(y)=="numeric") {
    cv.ind <- ceiling(sample(1:n)/n*nfolds)    
  } else stop(paste("class",class(y),"not recognized"))
  
  for (i in 1:nfolds) {
    D1 <- Data[cv.ind!=i,]
    D2 <- Data[cv.ind==i,]
    
    fit.i <- ctree(formula, data=D1, ...)
    yhat <- predict(fit.i, newdata=D2, type="response")
    E[cv.ind==i] <- if (classification) yhat != yy[cv.ind==i] else (y[cv.ind==i]-yhat)^2
  }
  E
}
