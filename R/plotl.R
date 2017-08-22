plotL <- function(x, l, xlab=expression(theta), ylab=expression(L(theta)), bty='n', ...) {
  if (is.matrix(l)) {
    L <- apply(l, 2, function(x) x/max(x))
    matplot(x, L, type='l', xlab=xlab, ylab=ylab, bty=bty,
            las=1, col=pal(ncol(L)), lwd=3, lty=1)
    if (!is.null(colnames(L))) toplegend(legend=colnames(L), lwd=3, col=pal(ncol(L)))
  } else {
    l <- l/max(l)
    plot(x, l, type='l', xlab=xlab, ylab=ylab, bty=bty,
         las=1, col=pal(2)[2], lwd=3, ...)
  }
}
plotl <- function(x, l, xlab=expression(theta), ylab=expression(L(theta)), bty='n', ...) {
  if (is.matrix(l)) {
    L <- apply(l, 2, function(x) x - max(x))
    matplot(x, L, type='l', xlab=xlab, ylab=ylab, bty=bty,
            las=1, col=pal(ncol(L)), lwd=3, lty=1)
    if (!is.null(colnames(L))) toplegend(legend=colnames(L), lwd=3, col=pal(ncol(L)))
  } else {
    l <- l - max(l)
    plot(x, l, type='l', xlab=xlab, ylab=ylab, bty=bty,
         las=1, col=pal(2)[2], lwd=3, ...)
  }
}
