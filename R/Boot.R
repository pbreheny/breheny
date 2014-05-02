Boot <- function(data, statistic, R, ...) {
  if (is.null(dim(data))) {
    f <- function(x, ind) {statistic(x[ind])}
  } else {
    f <- function(x, ind) {statistic(x[ind,])}
  }
  boot(data, statistic=f, R, ...)
}
