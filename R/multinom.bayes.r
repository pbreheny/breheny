multinom.bayes <- function(x, alpha=rep(1, length(x)), level=.95, B=10000) {
  if (is.matrix(x)) {
    if (missing(alpha)) alpha <- rep(1, ncol(x))
    return(multinom.bayes.matrix(x, alpha, level, B))
  }
  if (length(alpha)!=length(x)) stop("Dimension of alpha not consistent with data")
  pi <- rdir(B, x+alpha)
  colnames(pi) <- names(x)
  psm(pi)
}
multinom.bayes.matrix <- function(X, alpha, level, B) {
  if (nrow(X) > 2) stop("Not set up to handle more than 2 groups yet")
  if (length(alpha)!=ncol(X)) stop("Dimension of alpha not consistent with data")
  pi1 <- rdir(B, X[1,]+alpha)
  pi2 <- rdir(B, X[2,]+alpha)
  DX <- pi1-pi2
  colnames(DX) <- colnames(X)
  psm(DX)
}
