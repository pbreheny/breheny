dProd <- function(X,d) {
  sweep(X, 2, d, "*")
}
