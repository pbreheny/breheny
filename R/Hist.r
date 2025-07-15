#' Nicer-looking histogram
#'
#' @param x        A vector of values to make a histogram out of
#' @param ...      Further arguments to `hist()`
#' @param col      Fill color for histogram bars; default: gray
#' @param border   Border (outline) of histogram bars; default: white
#' @param n        Number of bins
#' @param breaks   Specific locations at which bins are to be constructed; overrides `n`
#' @param main     Main title; default: none
#' @param las      See `par()`
#' @param xlab     Just like in every other plot
#'
#' @examples
#' Hist(rnorm(100))
#'
#' @export

Hist <- function(x, ..., col = "gray", border = "white", n = 10, breaks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), len = n), main = "", las = 1, xlab = deparse(substitute(x))) {
  hist(x, ..., main = main, col = col, border = border, breaks = breaks, xlab = xlab, las = las)
}
