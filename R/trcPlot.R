## Trace plot for R2jags
trcPlot <- function(fit, name, col=pal(3), lty=1, ylab, ...) {
  parnames <- dimnames(fit$BUGSoutput$sims.array)[[3]]
  vars <- grep(name, parnames)
  for (j in vars) {
    ylab <- parnames[j]
    matplot(fit$BUGSoutput$sims.array[,,j], type="l", col=col, lty=lty, ylab=ylab, xlab="Iteration", ...)
  }
}
