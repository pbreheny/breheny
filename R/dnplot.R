dnplot <- function(M, labs, lwd=3, lty=1, col=pal(p), ylab="Density", xlab="", las=1, dens.par=NULL, ...) {
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
    X <- cbind(X, d$x)
    Y <- cbind(Y, d$y)
    mode[i] <- d$x[which.max(d$y)]
  }
  matplot(X, Y, lty=lty, lwd=lwd, type="l", col=col, ylab=ylab, xlab=xlab, las=1, ...)
  if (!missing(labs)) toplegend(legend=labs, col=col, lwd=lwd, ncol=min(p, 4))
  invisible(mode)
}
