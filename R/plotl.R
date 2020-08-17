#' Plot likelihood function
#'
#' `plotL()` plots the likelihood, `plotl()` plots the log-likelihood
#'
#' @param x      Parameter values (on x axis)
#' @param l      Likelihood / log-likelihood (on y axis)
#' @param xlab   Default: theta
#' @param ylab   Default: L(theta)
#' @param bty    Default: no boundary box
#' @param add    Add to existing plot?  Default: FALSE
#' @param ...    Further arguments to `plot()`
#'
#' @name likelihood_plot
#'
#' @encoding UTF-8
#'
#' @examples
#' x <- seq(-3, 4, length=99)
#' L <- dnorm(1, x)
#' l <- dnorm(1, x, log=TRUE)
#' plotL(x, L)
#' plotl(x, l)
NULL

#' @rdname likelihood_plot
#'
#' @export

plotL <- function(x, l, xlab=expression(theta), ylab=expression(L(theta)), bty='n', col, add=FALSE, ...) {
  if (is.matrix(l)) {
    if (missing(col)) col <- pal(ncol(L))
    L <- apply(l, 2, function(x) x/max(x))
    matplot(x, L, type='l', xlab=xlab, ylab=ylab, bty=bty, las=1, col=col, lwd=3, lty=1, add=add)
    if (!is.null(colnames(L))) toplegend(legend=colnames(L), lwd=3, col=pal(ncol(L)))
    return(invisible(L))
  } else {
    if (missing(col)) col <- pal(2)[2]
    l <- l/max(l)
    if (add) {
      lines(x, l, xlab=xlab, ylab=ylab, bty=bty, las=1, col=col, lwd=3, ...)
    } else {
      plot(x, l, type='l', xlab=xlab, ylab=ylab, bty=bty, las=1, col=col, lwd=3, ...)
    }
    return(invisible(l))
  }
}

#' @rdname likelihood_plot
#'
#' @export

plotl <- function(x, l, xlab=expression(theta), ylab=expression("\u2113"*(theta)), bty='n', ...) {
  if (is.matrix(l)) {
    L <- apply(l, 2, function(x) x - max(x))
    matplot(x, L, type='l', xlab=xlab, ylab=ylab, bty=bty,
            las=1, col=pal(ncol(L)), lwd=3, lty=1)
    if (!is.null(colnames(L))) toplegend(legend=colnames(L), lwd=3, col=pal(ncol(L)))
    return(invisible(L))
  } else {
    l <- l - max(l)
    plot(x, l, type='l', xlab=xlab, ylab=ylab, bty=bty,
         las=1, col=pal(2)[2], lwd=3, ...)
    return(invisible(l))
  }
}
