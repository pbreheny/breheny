splitleft <- function(x, split, ...) {
  if (!length(x)) return(character(0))
  spl <- strsplit(x, split=split, ...)
  n <- length(spl)
  X <- matrix(unlist(spl), nrow=n, byrow=TRUE)
  return(X[,1])
}
splitright <- function(x, split, ...) {
  if (!length(x)) return(character(0))
  spl <- strsplit(x, split=split, ...)
  n <- length(spl)
  X <- matrix(unlist(spl),nrow=n,byrow=TRUE)
  return(X[,2])
}
