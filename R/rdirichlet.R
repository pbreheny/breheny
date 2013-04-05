rdirichlet <- function(n, alpha) {
  l <- length(alpha)
  x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
  nn <- x %*% rep(1, l)
  x/as.numeric(nn)
}
