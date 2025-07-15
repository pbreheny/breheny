eci <- function(fit, ind, ...) {
  if (missing(ind)) ind <- 1:length(coef(fit))
  A <- coef(fit)[ind]
  ci <- confint(fit, ind, ...)
  B <- matrix(ci, ncol = 2)
  ci.names <- if (class(ci) == "numeric") names(ci) else colnames(ci)
  val <- cbind(A, B)
  rownames(val) <- names(coef(fit)[ind])
  colnames(val) <- c("Estimate", ci.names)
  val
}
