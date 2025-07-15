smoothPlot <- function(x, y, n = 101, add = FALSE, spar = 0, ...) {
  xx <- seq(min(x), max(x), len = n)
  f <- predict(smooth.spline(x, y, spar = spar), xx)
  if (add) {
    lines(f$x, f$y, ...)
  } else {
    plot(f$x, f$y, type = "l", ...)
  }
}
