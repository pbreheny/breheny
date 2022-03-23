## Calculates quadratic forms
qd <- function(x,A)
  {
    crossprod(x,A)%*%x
  }
