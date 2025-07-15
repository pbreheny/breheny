#' Draw a chi-squared curve and shade area to the right (probability)
#'
#' @param a         Point in question
#' @param right     Shade the area to the right?  Default is TRUE; otherwise area to the left is shaded
#' @param df        Degrees of freedom; default: 1
#' @param ...       Additional arguments to `plot()`
#'
#' @examples
#' chisqProb(2)
#' chisqProb(2, right = FALSE)
#' @export

chisqProb <- function(a, right = TRUE, df = 1, ...) {
  x <- seq(0, 5, len = 101)
  y <- dchisq(x, df = df)
  plot(x, y, type = "l", ylab = "Density", yaxs = "i", bty = "n", ylim = c(0, 1), las = 1, xlab = expression(chi^2), ...)
  if (right) {
    x1 <- seq(a, 5, len = 101)
    y1 <- dchisq(x1, df = df)
    polygon(c(x1, max(x1), min(x1)), c(y1, 0, 0), col = "gray80", border = NA)
  } else {
    x1 <- seq(0, a, len = 101)
    y1 <- dchisq(x1, df = df)
    polygon(c(x1, max(x1), min(x1)), c(y1, 0, 0), col = "gray80", border = NA)
  }
}
