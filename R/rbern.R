rbern <- function(N,prob)
  {
    n <- length(prob)
    X <- matrix(rbinom(N*n,1,prob),nrow=n,ncol=N)
    return(apply(X,2,sum))
  }
