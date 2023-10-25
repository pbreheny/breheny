#' Simple interface to ggplot2 histogram
#'
#' @param x   A vector of values to make a histogram out of
#' @param n   Number of bins
#'
#' @examples
#' Hist(rnorm(100))
#' @export

gghist <- function(x, n=10) {
  data.frame(x=as.numeric(x)) |>
    ggplot2::ggplot(ggplot2::aes(x)) + ggplot2::geom_histogram(bins=15)
}
