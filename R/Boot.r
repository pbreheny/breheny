#' Simplified interface to boot package for basic statistics
#'
#' @param data        Data to bootstrap; can be a vector or something matrix-like
#' @param statistic   Statistic to be applied to data
#' @param R           Number of bootstrap replicates
#' @param ...         Additional arguments to `boot()`
#'
#' @examples
#' x <- rexp(100)
#' Boot(x, mean, 999)
#'
#' @export

Boot <- function(data, statistic, R, ...) {
  if (is.null(dim(data))) {
    f <- function(x, ind) {statistic(x[ind])}
  } else {
    f <- function(x, ind) {statistic(x[ind,])}
  }
  boot::boot(data, statistic=f, R, ...)
}
