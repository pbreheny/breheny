#' Apply rank transformation to new data
#'
#'
#'
#' @param x     A vector or matrix to which a rank transformation was applied
#' @param new   New data to be ranked againt the old
#' @param ...   Further arguments to `rank()`
#'
#' @examples
#' x <- rnorm(100)
#' rank_new(x, 1)
#' rank_new(x, 4)
#' X <- matrix(rnorm(100*5), 100, 5)
#' rank_new(X, rbind(1:5, -2:2))
#' @export

rank_new <- function(x, new, ...) {
  if (is.null(dim(x))) {
    if (!is.null(dim(new))) stop('x is a vector and new is a matrix', call.=FALSE)
    out <- rank_new_vector(x, new, ...)
  } else {
    if (ncol(new) != ncol(x)) stop('x and new must have same number of columns', call.=FALSE)
    out <- matrix(NA, nrow(new), ncol(new))
    for (j in 1:ncol(x)) out[,j] <- rank_new_vector(x[,j], new[,j], ...)
  }
  out
}

rank_new_vector <- function(x, new, ...) {
  r <- rank(x, ...)
  f <- approxfun(x, r, rule=2)
  f(new)
}
