genData <- function(n, J, K=1, beta, family=c("gaussian","binomial"), J0=ceiling(J/2), K0=K, SNR=1, sig = c("homogeneous","heterogeneous"), sig.g = c("homogeneous","heterogeneous"), rho = 0, rho.g = rho)
  {
    family <- match.arg(family)
    sig <- match.arg(sig)
    sig.g <- match.arg(sig.g)
    
    ## Gen X, S
    X <- genX(n=n,J=J,K=K,rho=rho,rho.g=rho.g)
    S <- matrix(rho,nrow=J*K,ncol=J*K)
    for (i in 1:J) S[(i-1)*K+1:K,(i-1)*K+1:K] <- rho.g
    diag(S) <- rep(1,J*K)

    ## Gen beta
    j <- rep(1:J,rep(K,J))
    if (missing(beta))
      {
        k <- rep(1:K,J)
        b <- (j <= J0) * (k <= K0)
        if (sig=="heterogeneous") b <- b*j
        if (sig.g=="heterogeneous") b <- b*k
        s <- c(1,-1)[1+j%%2] * c(1,-1)[1+k%%2]
        b <- b*s
        beta <- b*sqrt(SNR)/sqrt(crossprod(b,S)%*%b)
      }

    ## Gen y
    y <- genY(X%*%beta, family=family, sigma=1)
    return(list(X=X,y=y,beta=beta,family=family,group=j))
  }

## rho  : correlation across all explanatory variables
## rho.g: correlation within group (must be at least rho)
genX <- function(n,J,K=1,rho=0,rho.g=rho)
  {
    a <- sqrt(rho/(1-rho.g))
    b <- sqrt((rho.g-rho)/(1-rho.g))
    Z <- rnorm(n)
    ZZ <- t(matrix(rep(rnorm(n*J),rep(K,n*J)),ncol=n))
    ZZZ <- matrix(rnorm(n*J*K),nrow=n)
    return(matrix(as.numeric(a*Z + b*ZZ + ZZZ),nrow=n)/sqrt(1+a^2+b^2))
  }

genY <- function(eta,family=c("gaussian","binomial"),sigma=1)
  {
    family=match.arg(family)
    n <- length(eta)
    if (family=="gaussian") y <- rnorm(n,mean=eta,sd=sigma)
    else if (family=="binomial")
      {
        pi. <- exp(eta)/(1+exp(eta))
        pi.[eta > log(.9999/.0001)] <- 1
        pi.[eta < log(.0001/.9999)] <- 0
        y <- rbinom(n,1,pi.)
      }
    return(y)
  }
