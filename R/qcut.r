#' Quantile-based cut
#'
#' @param x A numeric vector
#' @param n Number of bins to divide x into (e.g., 3 for tertiles, 5 for quintiles)
#' @param ... Additional arguments to cut
#'
#' @examples
#' rnorm(9) |> qcut(3) |> table()
#' @export

qcut <- function(x, n, ...) {
  cut(x,
      breaks = quantile(x, probs = seq(0, 1, length.out = n + 1),
                        na.rm = TRUE),
      include.lowest = TRUE,
      ...)
}
