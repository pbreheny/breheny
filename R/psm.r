psm <- function(x, labels, level=.95) {
  if (is.null(dim(x))) x <- as.matrix(x)
  alpha <- 1-level
  probs <- c(.5, alpha/2, 1-alpha/2)
  P <- t(apply(x, 2, quantile, probs=probs))
  if (!missing(labels)) rownames(P) <- labels
  P
}
