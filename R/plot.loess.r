plot.loess <- function(x,...){ind <- order(x$x);plot(x$x[ind],x$fitted[ind],...)}
lines.loess <- function(x,...){ind <- order(x$x);lines(x$x[ind],x$fitted[ind],...)}
