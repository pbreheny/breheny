#' Wrapper to create geom_contour of a function
#'
#' @param x,y  Vectors defining the x and y axis
#' @param f    The function to plot
#'
#' @returns A `gg` object
#'
#' @examples
#' x <- y <- seq(-10, 10, 0.1)
#' f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
#' ggcontour(x, y, f)
#' @export

ggcontour <- function(x, y, f) {
  z <- outer(x, y, f)
  dimnames(z) <- list(x, y)
  dat <- data.table::as.data.table(z, keep.rownames = 'x') |>
    melt(id.vars='x', variable.name='y')
  dat[, x := as.numeric(x)]
  dat[, y := factor2num(y)]
  ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$y, z=.data$value)) +
    ggplot2::geom_contour_filled() +
    ggplot2::scale_x_continuous(expand=ggplot2::expansion(0)) +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(0))
}
