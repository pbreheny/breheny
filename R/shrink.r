## James-Stein shrinkage
shrink <- function(x, n) {
  p <- length(x)
  lam <- (p-3)/(n*p-p)
  xbar <- mean(x)
  xbar + (x-xbar)/(1+lam)
}

