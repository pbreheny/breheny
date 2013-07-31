multinom.bayes <- function(x, alpha=rep(1, length(x)), level=.95, B=10000) {
  if (length(alpha)!=length(x)) stop("Dimension of alpha not consistent with data")
  pi <- rdirichlet(B, x+alpha)
  colnames(pi) <- names(x)
  psm(pi)
}