myTrellis <- function(n.col=3, lwd=3, alpha=1, cex=1) {
  col <- if (n.col==2) pal(3, alpha=alpha)[c(1,3)] else pal(n.col, alpha=alpha)
  trellis.par.set(superpose.line=list(lty=1, lwd=3, col=col),
                  strip.background=list(col="gray95"),
                  superpose.symbol=list(pch=19, col=col))
}
