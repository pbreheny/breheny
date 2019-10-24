#' Density plot
#'
#' Primarily intended for plotting posterior densities
#'
#' @param M                               A vector or matrix of values upon which the density plot is based; if a matrix, separate densities will be estimated and plotted for each column
#' @param labs                            If supplied, a legend is printed; order of `labs` should correspond to columns of `M`
#' @param pos                             Are the densities strictly positive?  If so, a boundary correction is applied.  Default: FALSE
#' @param lwd,bty,lty,col,xlab,ylab,las   Plotting options, as in `par()`
#' @param dens.par                        Additional arguments to `density()`
#' @param ...                             Additional arguments to `plot()`
#'
#' @examples
#' M <- cbind(rgamma(1000, 2, 2), rnorm(1000))
#' dnplot(M[,1])
#' dnplot(M[,1], pos=TRUE)
#' dnplot(M)
#' dnplot(M, labs=c('Gamma', 'Normal'))
#' dnplot(M, labs=c('Gamma', 'Normal'))
#' @export

dnplot <- function(M, labs, pos=FALSE, lwd=3, bty="n", lty=1, col=pal(p), ylab="Density", xlab="", las=1, dens.par=NULL, ...) {
  if (!is.matrix(M)) M <- matrix(M, ncol=1)
  p <- ncol(M)
  X <- Y <- NULL
  dots <- list(...)
  if ("xlim" %in% names(dots)) {
    dens.par$from <- dots$xlim[1]
    dens.par$to <- dots$xlim[2]
  }
  mode <- numeric(p)
  for (i in 1:p) {
    dens.args <- list(x=M[,i])
    if (length(dens.par)) dens.args[names(dens.par)] <- dens.par
    d <- do.call("density", dens.args)
    if (pos) {
      x <- d$x
      negid <- which(x < 0)
      j0 <- length(negid) + 1
      posid <- j0:(j0 + length(negid) - 1)
      d$y[posid] <- d$y[posid] + rev(d$y[negid])
      d$x[negid] <- NA
      d$y[negid] <- NA
    }
    X <- cbind(X, d$x)
    Y <- cbind(Y, d$y)
    mode[i] <- d$x[which.max(d$y)]
  }
  matplot(X, Y, lty=lty, lwd=lwd, type="l", col=col, ylab=ylab, xlab=xlab, las=1, bty=bty, ...)
  if (!missing(labs)) toplegend(legend=labs, col=col, lwd=lwd)
  invisible(mode)
}
