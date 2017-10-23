Plot.BinaryTree <- function(obj, pval=FALSE, summary=FALSE, digits=1, ...) {
  if (summary) plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits), terminal_panel=panelSummary)
  else plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits))
}
Plot.rpart <- function(obj, ...) {
  require(partykit)
  fit <- as.party(obj)
  plot(fit, ip_args=list(id=FALSE), tp_args=list(id=FALSE), ...)
}
Plot.survfit <- function(obj, legend=c("top", "right", "none"), xlab="Time", ylab="Survival", conf.int=FALSE, col, ...) {
  legend <- match.arg(legend)
  n <- length(obj$strata)
  if (missing(col)) {
    if (n == 0) {
      col <- pal(2)[2]
    } else {
      col <- pal(n)
    }
  }    
  plot(obj, xlab=xlab, ylab=ylab, bty="n", las=1, col=col, lwd=3, conf.int=conf.int, ...)
  if (n > 0) {
    if (legend == "top") {
      toplegend(legend=gsub('.*=', '', names(obj$strata)), col=pal(n), lwd=3)
    } else if (legend == "right") {
      rightlegend(legend=gsub('.*=', '', names(obj$strata)), col=pal(n), lwd=3)
    }
  }
}
Plot <- function(obj,...) UseMethod("Plot")
