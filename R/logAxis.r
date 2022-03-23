#' Make a log-scale axis
#'
#' @param side    1=bottom, 2=left, 3=top, 4=right
#' @param base    What base is the log?  Default is base e (natural log).
#' @param disp    By default, displays in same base as how constructed, but can display in other bases too
#' @param n       Number of tick marks
#' @param style   Either "dec" for decimals such as 0.125, or "pow" for powers such as 2^(-3)
#' @param ...     Further arguments to `axis()`
#'
#' @examples
#' x <- rexp(100)
#' y <- runif(100)
#' plot(log(x), y)
#' plot(log(x), y, xaxt='n', xlab='x')
#' logAxis()
#' plot(log(x), y, xaxt='n', xlab='x')
#' logAxis(style='pow', disp=2)
#'
#' @export
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
