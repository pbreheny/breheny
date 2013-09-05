rbern <- function(N,prob)
  {
    n <- length(prob)
    X <- matrix(rbinom(N*n,1,prob),nrow=n,ncol=N)
    return(apply(X,2,sum))
  }
rnhb <- function(N,prob)
  {
    n <- length(prob)
    U <- matrix(runif(N*n),nrow=n)
    return(apply(U<prob,2,sum))
  }
