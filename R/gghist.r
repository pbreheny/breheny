#' Simple interface to ggplot2 histogram
#'
#' @param x         A vector of values to make a histogram out of
#' @param density   Should y-axis be density? (default: FALSE)
#' @param ...       Further arguments to `ggplot2::geom_histogram()`
#'
#' @examples
#' gghist(rnorm(100))
#' gghist(runif(100), breaks = seq(0, 1, 0.1))
#' gghist(rnorm(100), density = TRUE)
#' @export

gghist <- function(x, density = FALSE, ...) {
  if (density) {
    data.frame(x = as.numeric(x)) |>
      ggplot2::ggplot(ggplot2::aes(x, ggplot2::after_stat(density))) +
      ggplot2::geom_histogram(...)
  } else {
    data.frame(x = as.numeric(x)) |>
      ggplot2::ggplot(ggplot2::aes(x)) +
      ggplot2::geom_histogram(...)
  }
}
