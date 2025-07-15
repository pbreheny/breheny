#' Boxplot of a data frame / matrix using ggplot
#'
#' Works with labels, as from the `labelled` package. Axis labels are handled
#' somewhat differently depending on whether a single box is present or multiple.
#'
#' @param X       Matrix of values
#' @param xlab    If omitted, no space will be allocated for x axis title (ylab
#'                can be specified as usual; see examples)
#' @param horiz   Make horizontal? (default: FALSE)
#'
#' @examples
#' ggbox(matrix(rnorm(50), 10, 5))
#' ggbox(matrix(rnorm(50), 10, 5), xlab = "categories")
#' ggbox(rexp(10))
#' ggbox(rexp(10), "A plot of") + ggplot2::ylab("Something")
#'
#' # Using labels
#' X <- iris[, 1:4]
#' attr(X[[4]], "label") <- "The petal's width"
#' ggbox(X)
#' ggbox(X[, 4])
#' ggbox(X[, 4], horiz = TRUE)
#' ggbox(X, horiz = TRUE)
#' @export

ggbox <- function(X, xlab, horiz = FALSE) {
  if (!is.data.frame(X)) X <- as.data.frame(X)
  df <- lapply(X, as.vector) |> stack()
  df$ind <- as.character(df$ind)
  for (v in names(X)) {
    if ("label" %in% names(attributes(X[[v]]))) {
      df$ind[df$ind == v] <- attr(X[[v]], "label")
    }
  }
  if (horiz) {
    p <- ggplot2::ggplot(df, ggplot2::aes(.data$values, .data$ind))
    if (missing(xlab)) {
      p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
    } else {
      p <- p + ggplot2::ylab(xlab)
    }
    if (length(unique(df$ind)) == 1) {
      p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
        ggplot2::theme(axis.ticks.y = ggplot2::element_blank()) +
        ggplot2::xlab(df$ind[1])
    }
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(.data$ind, .data$values))
    if (missing(xlab)) {
      p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    } else {
      p <- p + ggplot2::xlab(xlab)
    }
    if (length(unique(df$ind)) == 1) {
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
        ggplot2::ylab(df$ind[1])
    }
  }
  p <- p + ggplot2::geom_boxplot()
  p
}
