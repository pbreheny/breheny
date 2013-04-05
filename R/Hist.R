Hist <- function(x, ..., col="gray", border="white", n=10, breaks=seq(min(x),max(x), len=n), main="", las=1, xlab=deparse(substitute(x))) {
  hist(x, ..., main=main, col=col, border=border, breaks=breaks, xlab=xlab)
}
