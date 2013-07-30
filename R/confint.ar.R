vcov.ar <- function(fit) {
  val <- fit$asy.var.coef
  rownames(val) <- colnames(val) <- 1:fit$order
  val
}
coef.ar <- function(fit) {
  val <- fit$ar
  names(val) <- 1:fit$order
  val
}
acf.ar <- function(fit, ...) {
  acf(fit$resid[-(1:fit$order)], ...)
}
