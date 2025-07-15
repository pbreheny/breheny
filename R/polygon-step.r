#' Draw a filled polygon as a step function
#'
#' @param x        x coordinates; must be one longer than `y1` and `y2`
#' @param y1       lower y coordinates
#' @param y2       upper y coordinates
#' @param border   Color to draw the border; see `polygon()`.  Default: no borders.
#' @param ...      Additional arguments to be passed to `polygon()`
#'
#' @examples
#' plot(1:10, 1:10, type = "n", ylim = c(0, 12))
#' polygon.step(1:10, 0:8, 2:10, col = "gray")
#' lines(1:10, 1:10, type = "s")
#'
#' @export

polygon.step <- function(x, y1, y2, border = FALSE, ...) {
  nx <- length(x)
  ny <- length(y1)
  if (length(y2) != ny) stop("y1 and y2 must be the same length")
  if (nx != (ny + 1)) stop("x must be one longer than y")
  xx <- c(x[1], rep(x[-c(1, nx)], rep(2, nx - 2)), x[nx])
  xxx <- c(xx, rev(xx))
  yy1 <- rep(y1, rep(2, ny))
  yy2 <- rep(y2, rep(2, ny))
  yyy <- c(yy1, rev(yy2))
  polygon(xxx, yyy, border = border, ...)
}
