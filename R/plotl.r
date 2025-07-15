#' Plot likelihood function
#'
#' `plotL()` plots the likelihood, `plotl()` plots the log-likelihood
#'
#' @param x      Parameter values (on x axis)
#' @param l      Likelihood / log-likelihood (on y axis)
#' @param xlab   Default: theta
#' @param ylab   Default: L(theta)
#' @param bty    Default: no boundary box
#' @param col    Color (or vector of colors, if `l` is a matrix)
#' @param add    Add to existing plot?  Default: FALSE
#' @param ...    Further arguments to `plot()`
#'
#' @name likelihood_plot
#'
#' @encoding UTF-8
#'
#' @examples
#' x <- seq(-3, 4, length = 99)
#' L <- dnorm(1, x)
#' l <- dnorm(1, x, log = TRUE)
#' plotL(x, L)
#' plotl(x, l)
#' L <- cbind(dnorm(1, x), dnorm(2, x))
#' l <- cbind(dnorm(1, x, log = TRUE), dnorm(2, x, log = TRUE))
#' plotL(x, L)
#' plotl(x, l)
NULL

#' @rdname likelihood_plot
#'
#' @export

plotL <- function(x, l, xlab = expression(theta), ylab = expression(L(theta)), bty = "n", col, add = FALSE, ...) {
  if (is.matrix(l)) {
    if (missing(col)) col <- pal(ncol(l))
    L <- apply(l, 2, function(x) x / max(x))
    matplot(x, L, type = "l", xlab = xlab, ylab = "", bty = bty, las = 1, col = col, lwd = 3, lty = 1, add = add)
    if (!is.null(colnames(L))) toplegend(legend = colnames(L), lwd = 3, col = col)
    if (!add) mtext(ylab, 2, line = 2.5)
    return(invisible(L))
  } else {
    if (missing(col)) col <- pal(2)[2]
    l <- l / max(l)
    if (add) {
      lines(x, l, xlab = xlab, ylab = ylab, bty = bty, las = 1, col = col, lwd = 3, ...)
    } else {
      plot(x, l, type = "l", xlab = xlab, ylab = "", bty = bty, las = 1, col = col, lwd = 3, ...)
    }
    if (!add) mtext(ylab, 2, line = 2.5)
    return(invisible(l))
  }
}

#' @rdname likelihood_plot
#'
#' @export

plotl <- function(x, l, bty = "n", add = FALSE, ...) {
  dots <- list(...)
  xlab <- dots[["xlab"]] %||% expression(theta)
  ylab <- dots[["ylab"]] %||% expression("\u2113" * (theta))

  if (is.matrix(l)) {
    col <- dots[["col"]] %||% pal(ncol(l))
    L <- apply(l, 2, function(x) x - max(x))
    matplot(
      x,
      L,
      type = "l",
      xlab = xlab,
      ylab = "",
      bty = bty,
      las = 1,
      col = col,
      lwd = 3,
      lty = 1,
      ...
    )
    if (!is.null(colnames(L)))
      toplegend(legend = colnames(L), lwd = 3, col = col)
    if (!add) mtext(ylab, 2, line = 2.5)
    return(invisible(L))
  } else {
    col <- dots[["col"]] %||% pal(2)[2]
    l <- l - max(l)
    if (add) {
      lines(x, l, col = col, lwd = 3, ...)
    } else {
      plot(
        x,
        l,
        type = "l",
        xlab = xlab,
        ylab = "",
        bty = bty,
        las = 1,
        col = col,
        lwd = 3,
        ...
      )
    }
    if (!add) mtext(ylab, 2, line = 2.5)
    return(invisible(l))
  }
}
