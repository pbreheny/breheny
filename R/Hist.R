Hist <- function(x, ..., col="gray", border="white", n=10, breaks=seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE), len=n), main="", las=1, xlab=deparse(substitute(x))) {
  hist(x, ..., main=main, col=col, border=border, breaks=breaks, xlab=xlab, las=las)
}
