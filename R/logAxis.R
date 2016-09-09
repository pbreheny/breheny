logAxis <- function(side=1, base, disp=base, n=5, style="dec", ...) {
  if (missing(base)) base <- exp(1)
  is.x <- side%%2 == 1
  usr <- if(is.x) par("usr")[1:2] else par("usr")[3:4]
  r <- usr*log(base)/log(disp)
  px <- pretty(r, n=n)
  px <- px[px > r[1] & px < r[2]]
  if (style=="pow") {
    at <- px*log(disp)/log(base)
    lab <- sapply(px, function(x){bquote(.(disp)^.(x))})
  } else {
    at <- seq(usr[1], usr[2], len=n)
    lab <- formatC(base^at, digits=1, format="f")
  }
  a <- axis(side, at=at, labels=lab, las=1, ...)
}
