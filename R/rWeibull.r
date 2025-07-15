## Alternative parameterization of Weibull
rWeibull <- function(n, alpha, sigma, eta = 0) {
  W <- log(rexp(n))
  Y <- alpha + eta * (-sigma) + sigma * W
  exp(Y)
}
