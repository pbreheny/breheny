Plot.BinaryTree <- function(obj, pval=FALSE, summary=FALSE, digits=1, ...) {
  if (summary) plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits), terminal_panel=panelSummary)
  else plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits))
}
Plot.rpart <- function(obj, ...) {
  require(partykit)
  fit <- as.party(obj)
  plot(fit, ip_args=list(id=FALSE), tp_args=list(id=FALSE), ...)
}
Plot.survfit <- function(obj, legend=TRUE, xlab="Time", ylab="Survival", ...) {
  n <- length(obj$strata)
  plot(obj, xlab=xlab, ylab=ylab, bty="n", las=1, col=pal(n), lwd=3, ...)
  if (legend) toplegend(legend=gsub('.*=', '', names(obj$strata)), col=pal(n), lwd=3)
}
Plot <- function(obj,...) UseMethod("Plot")
