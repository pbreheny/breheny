#' Make an lFDR plot for an ash object
#' 
#' @param fit   Output from `ash()``
#' @param n     Number of breaks for histogram
#' @param ...   Further arguments to `Hist()`
#' 
#' @examples
#' library(ashr)
#' z <- c(rnorm(900), rnorm(200, sd=2))
#' fit <- ash(z, rep(1, length(z)), optmethod='cxxMixSquarem')
#' lfdrPlot.ash(fit)

lfdrPlot.ash <- function(fit, n=99, ...) {
  z <- fit$data$x
  h <- Hist(z, freq=FALSE, n=n)
  zz <- seq(min(z), max(z), len=299)
  lines(zz, dnorm(zz), col=pal(2, alpha=0.3)[2], lwd=2)
  f0 <- approxfun(z, get_lfdr(fit))
  y <- h$density * (1 - f0(h$mids))
  for (k in 1:length(h$mids)) {
    if (y[k] != 0) lines(rep(h$mids[k],2), c(0, y[k]), lwd = 2, col = "red")
  }
}
