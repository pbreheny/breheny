## Alternative parameterization of Weibull
rWeibull <- function(n, alpha, sigma) {
  W <- log(rexp(n))
  Y <- alpha + sigma*W
  exp(Y)
}