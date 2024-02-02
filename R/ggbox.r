#' Boxplot of a data frame / matrix using ggplot
#'
#' Works with labels, as from the `labelled` package. Axis labels are handled
#' somewhat differently depending on whether a single box is present or multiple.
#'
#' @param X       Matrix of values
#' @param xlab    If omitted, no space will be allocated for x axis title (ylab
#'                can be specified as usual; see examples)
#'
#' @examples
#' ggbox(matrix(rnorm(50), 10, 5))
#' ggbox(matrix(rnorm(50), 10, 5), xlab='categories')
#' ggbox(rexp(10))
#' ggbox(rexp(10), 'A plot of') + ggplot2::ylab('Something')
#'
#' # Using labels
#' X <- iris[, 1:4]
#' attr(X[[4]], 'label') <- "The petal's width"
#' ggbox(X)
#' ggbox(X[,4])
#' @export

ggbox <- function(X, xlab) {
  if (!is.data.frame(X)) X <- as.data.frame(X)
  df <- lapply(X, as.vector) |> stack()
  df$ind <- as.character(df$ind)
  for (v in names(X)) {
    if ('label' %in% names(attributes(X[[v]]))) {
      df$ind[df$ind == v] <- attr(X[[v]], 'label')
    }
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(.data$ind, .data$values)) +
    ggplot2::geom_boxplot()
  if (missing(xlab)) {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  } else {
    p <- p + ggplot2::xlab(xlab)
  }
  if (length(unique(df$ind)) == 1) {
    p <- p + ggplot2::theme(axis.text.x=ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.x=ggplot2::element_blank()) +
      ggplot2::ylab(df$ind[1])
  }
  p
}
