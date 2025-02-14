#' Line plot of a matrix using ggplot
#'
#' Essentially a replacement for matplot.
#'
#' @param x,y   A vector (`x`) and matrix (`y`); if only x is provided, it is
#'   taken to be the matrix of responses and `1:n` used as the horizontal axis
#'
#' @examples
#' sines <- outer(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))
#' ggmat(sines)
#' ggmat((1:20)^2, sines)
#' @export

ggmat <- function(x, y) {
  if (missing(y)) {
    y <- x
    x <- 1:nrow(x)
  } else {
    if (length(x) != nrow(y)) stop('dimensions of x and y do not match', call.=FALSE)
  }
  dat <- as.data.table(y) |>
    cbind(x) |>
    melt(id.vars = c('x'))
  p <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$value, group=.data$variable, color=.data$variable)) +
    ggplot2::geom_line()
  p
}
