#' Boxplot of a matrix using ggplot
#'
#' @param X       Matrix of values
#' @param xlab    Horizontal axis label (default: ind)
#' @param ylab    Vertical axis label (default: values)
#'
#' @examples
#' ggbox(matrix(rnorm(50), 10, 5))
#'
#' @export

ggbox <- function(X, xlab='ind', ylab='values') {
  ggplot2::ggplot(stack(X), ggplot2::aes(ind, values)) + ggplot2::geom_boxplot() + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
}
