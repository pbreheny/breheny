se <- function(fit, ...) {
  sqrt(diag(vcov(fit, ...)))
}
