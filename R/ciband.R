ciband.survfit <- function(fit, col, fun=as.numeric) {
  K <- length(fit$strata)
  if (K==0) {
    s <- c(0, length(fit$time))
    if (missing(col)) col <- rgb(0.5, 0.5, 0.5, alpha=0.3)
    K <- 1
  } else {
    s <- c(0, cumsum(fit$strata))
    if (missing(col)) col <- pal(K, alpha=0.4)
  }
  for (i in 1:K) {
    ind1 <- (s[i]+1):s[i+1]
    ind2 <- (s[i]+1):(s[i+1]-1)
    x <- c(0, fit$time[ind1])
    l <- fun(c(1, fit$lower[ind2]))
    u <- fun(c(1, fit$upper[ind2]))
    polygon.step(x, l, u, col=col[i])
  }
}
ciband.survfitms <- function(fit, col, fun=as.numeric) {
  if(length(fit$strata)) stop('Not implemented for stratified multi-state models')
  K <- ncol(fit$prev)
  if (missing(col)) col <- pal(K, alpha=0.4)
  n <- length(fit$time)
  for (k in 1:K) {
    polygon.step(fit$time, fit$lower[-n,k], fit$upper[-n,k], col=col[k])
  }
}
ciband <- function(obj,...) UseMethod("ciband")
