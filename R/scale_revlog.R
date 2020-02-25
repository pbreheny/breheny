#' Provides a reverse log scale to ggplot; especially useful for p-values
#'
#' @name scale_revlog
#'
#' @param base   Base of log; default is natural log
#' @param ...    Additional arguments to scale_x_continuous or scale_x_continuous
#'
#' @examples
#' library(ggplot2)
#' DF <- data.frame(x=1:3, y=c(0.1, 0.01, 0.001))
#' ggplot(DF, aes(x, y)) + geom_point() + scale_y_revlog(10)
#'
#' @export

scale_x_revlog <- function(base=exp(1), ...) {
  scale_x_continuous(trans=reverselog_trans(10), ...)
}

#' @rdname scale_revlog
#' @export

scale_y_revlog <- function(base=exp(1), ...) {
  scale_y_continuous(trans=reverselog_trans(10), ...)
}

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
            scales::log_breaks(base = base),
            domain = c(1e-100, Inf))
}
