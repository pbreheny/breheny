alphaCol <- function(col,alpha) {
  x <- t(col2rgb(col))
  return(rgb(x, alpha=alpha*255, maxColorValue=255))
}
