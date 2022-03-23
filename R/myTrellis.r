myTrellis <- function(n.col=3, lwd=3, alpha=1, cex=1) {
  col <- if (n.col==2) pal(3, alpha=alpha)[c(1,3)] else pal(n.col, alpha=alpha)
  if (n.col==2) {
    solidCol <- hcl(seq(15, 375, len = 4), l = 70, c = 100)[c(1,3)]
  } else {
    solidCol <- hcl(seq(15, 375, len = n.col+1), l = 70, c = 100)[1:n.col]
  }
  singleCol <- pal(2, alpha=alpha)[2]
  lattice::trellis.par.set(
    superpose.line=list(lty=1, lwd=3, col=col),
    strip.background=list(col="gray90"),
    superpose.symbol=list(pch=19, col=col),
    plot.symbol=list(pch=19, col=singleCol),
    plot.polygon=list(border="white", col="#8FC7FF", lwd=1),
    superpose.polygon=list(col=solidCol))
}
