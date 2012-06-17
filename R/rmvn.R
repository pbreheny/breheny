rmvn <- function(n, d, rho,...)
  {
    require(mvtnorm)
    mu <- rep(0,d)
    sigma <- matrix(rho,ncol=d,nrow=d)
    diag(sigma) <- 1
    return(rmvnorm(n, mean = mu, sigma=sigma,...))
  }
