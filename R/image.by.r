image.by <- function(x, col=colorRampPalette(c("blue","white","red"))(100), ...) {
  image.default(x, col=col, xaxt="n", yaxt="n", ...)
  axis(1, labels=abbreviate(dimnames(x)[[1]]), at=seq(0, 1, len=dim(x)[1]), tck=0)
  axis(2, labels=abbreviate(dimnames(x)[[2]]), at=seq(0, 1, len=dim(x)[2]), las=1, tck=0)
}
