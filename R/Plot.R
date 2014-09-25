Plot.BinaryTree <- function(obj, pval=FALSE, summary=FALSE, digits=1, ...) {
  if (summary) plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits), terminal_panel=panelSummary)
  else plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits))
}
Plot.rpart <- function(obj, ...) {
  require(partykit)
  fit <- as.party(obj)
  plot(fit, ip_args=list(id=FALSE), tp_args=list(id=FALSE))
}
Plot <- function(obj,...) UseMethod("Plot")
