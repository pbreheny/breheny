textPlot <- function(X, xlab=colnames(X)[1], ylab=colnames(X)[2], ...) {
  plot(X[,1], X[,2], type="n", xlab=xlab, ylab=ylab, bty="n", las=1, ...)
  text(X[,1], X[,2], rownames(X))
}
