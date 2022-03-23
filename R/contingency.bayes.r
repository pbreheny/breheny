## Bayesian analysis of 2x2 contingency tables
## Provides OR, CI for OR, and 'p'-value
contingency.bayes <- function(X, N=10000, a=1, b=1, level=.95, null, plot=FALSE, add=FALSE, xlab="p", ylab="Posterior density", col="blue", ...) {
  p1 <- rbeta(N, X[1,1] + a, X[1,2] + b)
  p2 <- rbeta(N, X[2,1] + a, X[2,2] + b)
  lOR <- log(p1/(1-p1) / (p2/(1-p2)))
  dens <- density(lOR)
  ind0 <- which.min(abs(dens$x)) ## Index closest to null
  ind.nonnull <- which(dens$y > dens$y[ind0])
  HPDnull <- range(dens$x[ind.nonnull])
  p <- mean(lOR < HPDnull[1]) + mean(lOR > HPDnull[2])
  val <- list(estimate = exp(mean(lOR)), CI = exp(quantile(lOR, probs=c((1-level)/2, (1+level)/2))), p=p)
  val
}
chisq.bayes <- function(X, N=10000, a=1, b=1) {
  d <- dim(X)
  n <- apply(X, 1, sum)
  XX <- array(NA, dim=c(N, d))
  p <- rbeta(N, sum(X[,1]) + a, sum(X[,2]) + b)
  for (i in 1:d[1]) {
    x <- rbinom(N, size=n[i], prob=p)
    XX[,i,1] <- x
    XX[,i,2] <- n[i]-x
  }
  t1 <- apply(XX, 1, function(X) chisq.test(X)$statistic)
  t0 <- chisq.test(X)$statistic
  mean(t0 < t1)
}
