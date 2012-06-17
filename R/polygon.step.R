polygon.step <- function(x,y1,y2,border=FALSE,...)
  {
    nx <- length(x)
    ny <- length(y1)
    if (length(y2)!=ny) stop("y1 and y2 must be the same length")
    if (nx != (ny+1)) stop("x must be one longer than y")
    xx <- c(x[1],rep(x[-c(1,nx)],rep(2,nx-2)),x[nx])
    xxx <- c(xx,rev(xx))
    yy1 <- rep(y1,rep(2,ny))
    yy2 <- rep(y2,rep(2,ny))
    yyy <- c(yy1,rev(yy2))
    polygon(xxx,yyy,border=border,...)
  }
