splitleft <- function(x, split, ...) {
  spl <- strsplit(x, split=split, ...)
  n <- length(spl)
  X <- matrix(unlist(spl), nrow=n, byrow=TRUE)
  return(X[,1])
}
splitright <- function(x, split, ...) {
  spl <- strsplit(x, split=split, ...)
  n <- length(spl)
  X <- matrix(unlist(spl),nrow=n,byrow=TRUE)
  return(X[,2])
}
