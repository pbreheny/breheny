logAxis <- function(side, base, disp=base, n=5, ...)
{
  is.x <- side%%2 == 1
  XY <- function(ch) paste0(if (is.x) "x" else "y", ch)
  axp <- par(XY("axp"))
  r <- axp[1:2]*log(base)/log(disp)
  px <- pretty(r, n=n)
  px <- px[px > r[1] & px < r[2]]
  at <- px*log(disp)/log(base)
  lab <- disp^px
  lab[px < 0] <- paste("1/",disp^(-px[px < 0]))
  a <- axis(side, at=at, labels=lab)
}