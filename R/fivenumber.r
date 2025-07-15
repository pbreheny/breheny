fivenumber <- function(x, by, ...) {
  if (missing(by)) {
    val <- fivenum(x, ...)
  } else {
    l <- tapply(x, by, fivenum, ...)
    val <- as.data.frame(t(sapply(l, rbind)))
    names(val) <- c("Min", "25%", "Median", "75%", "Max")
  }
  return(val)
}
