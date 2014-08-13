## Generates correlated (block-diagonal, compound symmetric) data
## n = Number of observations
## J = Number of covariates
## G = Number of groups
genCorrData <- function(n, J, G, beta, J0=ceiling(J/2), SNR=1, sig=c("homogeneous","heterogeneous"), rho=0) {
  sig <- match.arg(sig)
  H <- n/G
  if (H!=round(H)) stop("Sample size must be a multiple of number of groups.")
  
  ## Generate X, S
  X <- matrix(rnorm(n*J), n, J)
  if (missing(beta) || length(beta)==1) {
    j <- 1:J
    b <- (j <= J0)
    s <- c(-1,1)[1+j%%2]
    if (missing(beta)) {
      if (sig=="heterogeneous") b <- b*j
      b <- b*s
      beta <- b*sqrt(SNR)/sqrt(crossprod(b))
    } else {
      beta <- b*s*beta
    }
  }
  
  ## Generate noise
  group <- paste0("G", rep(1:G, rep(H,G)))
  e <- sqrt(1-rho)
  a <- sqrt(e^2*rho/(1-rho))
  u <- rnorm(G)
  E <- matrix(rnorm(G*H), H, G)
  noise <- as.numeric(sweep(e*E, 2, a*u, "+"))
  Sigma <- matrix(0, n, n)
  for (i in 1:G) Sigma[(i-1)*H+1:H,(i-1)*H+1:H] <- rho
  diag(Sigma) <- 1
  
  ## Generate Y
  y <- X%*%beta + noise
  
  list(X=X, y=y, beta=beta, group=group, Sigma=Sigma)
}
