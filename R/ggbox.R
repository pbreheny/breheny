#' Boxplot of a data frame / matrix using ggplot
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
  if (is.null(dim(X))) remove.label <- TRUE
  if (!is.data.frame(X)) X <- as.data.frame(X)
  p <- ggplot2::ggplot(utils::stack(X), ggplot2::aes_string('ind', 'values')) + ggplot2::geom_boxplot() + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
  if (remove.label) p <- p + ggplot2::theme(axis.text.x=ggplot2::element_blank())
  p
}
