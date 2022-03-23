#' Scatter plot, but with text labels
#'
#' @param x      x positions (named); alternatively, a 2-column matrix with column and row names
#' @param y      y positions, if x is not a matrix
#' @param xlab   x axis label
#' @param ylab   y axis label
#' @param ...    Additional arguments to `plot()`
#'
#' @examples
#' a <- runif(10)
#' b <- runif(10)
#' names(a) <- names(b) <- LETTERS[1:10]
#' textPlot(a, b)
#'
#' X <- cbind(a=a, b=b)
#' textPlot(X)
#'
#' @export

textPlot <- function(x, y, xlab, ylab, ...) {
  if (!is.null(dim(x))) {
    if (dim(x)[2] != 2) stop('x must be a 2-column matrix')
    lab <- rownames(x)
    if (missing(xlab)) xlab <- colnames(x)[1]
    if (missing(ylab)) ylab <- colnames(x)[1]
    y <- x[,2]
    x <- x[,1]
  } else {
    if (!identical(names(x), names(y))) stop('names(x) does not match names(y)')
    lab <- names(x)
    if (missing(xlab)) xlab <- match.call()[[2]]
    if (missing(ylab)) ylab <- match.call()[[2]]
  }
  plot(x, y, type="n", xlab=xlab, ylab=ylab, bty="n", las=1, ...)
  text(x, y, lab)
}
