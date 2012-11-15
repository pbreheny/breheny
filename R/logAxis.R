logAxis <- function(side=1, base, disp=base, n=5, ...)
{
  if (missing(base)) {
    base <- exp(1)
    disp <- 2
  }
  is.x <- side%%2 == 1
  XY <- function(ch) paste0(if (is.x) "x" else "y", ch)
  axp <- par(XY("axp"))
  r <- axp[1:2]*log(base)/log(disp)
  px <- pretty(r, n=n)
  px <- px[px > r[1] & px < r[2]]
  at <- px*log(disp)/log(base)
  if (isTRUE(all.equal(px, as.integer(px)))) {
    lab <- disp^px
    lab[px < 0] <- paste("1/",disp^(-px[px < 0]))    
  } else {
    lab <- formatC(disp^px, digits=1, format="f")
  }
  a <- axis(side, at=at, labels=lab)
}