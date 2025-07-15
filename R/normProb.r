#' Draw a normal curve and shade area under an interval (probability)
#'
#' @param a,b       Endpoints of the interval
#' @param outside   Shade the area outside (a,b)?  Default is FALSE (i.e., shade the area inside a and b)
#' @param ...       Additional arguments to `plot()`
#'
#' @examples
#' normProb(1, 2)
#' normProb(1, 2, outside = TRUE)
#' normProb(1, 2, lwd = 5, col = "pink", main = "Nice!")
#' @export

normProb <- function(a, b, outside = FALSE, ...) {
  x <- seq(-4, 4, len = 101)
  y <- dnorm(x)
  plot(x, y, type = "l", ylab = "Density", yaxs = "i", bty = "n", ylim = c(0, 0.5), las = 1, ...)
  if (!outside) {
    x1 <- seq(a, b, len = 101)
    y1 <- dnorm(x1)
    polygon(c(x1, max(x1), min(x1)), c(y1, 0, 0), col = "gray80", border = NA)
  } else {
    x1 <- seq(-4, a, len = 101)
    y1 <- dnorm(x1)
    polygon(c(x1, max(x1), min(x1)), c(y1, 0, 0), col = "gray80", border = NA)
    x2 <- seq(b, 4, len = 101)
    y2 <- dnorm(x2)
    polygon(c(x2, max(x2), min(x2)), c(y2, 0, 0), col = "gray80", border = NA)
  }
}
