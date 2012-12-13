logAxis <- function(side=1, base, disp=base, n=5, style=NULL, ...) {
  if (missing(base)) base <- exp(1)
  is.x <- side%%2 == 1
  XY <- function(ch) paste0(if (is.x) "x" else "y", ch)
  r <- axp[1:2]*log(base)/log(disp)
  px <- pretty(r, n=n)
  if (is.null(style)) style <- if (isTRUE(all.equal(px, as.integer(px)))) "pow" else "dec"
  axp <- par(XY("axp"))
  if (style=="pow") {
    px <- px[px > r[1] & px < r[2]]
    at <- px*log(disp)/log(base)
    lab <- disp^px
    lab[px < 0] <- paste("1/",disp^(-px[px < 0]))    
  } else {
    at <- pretty(axp[1:2])
    lab <- formatC(base^at, digits=1, format="f")
  }
  a <- axis(side, at=at, labels=lab, las=1, ...)
}
