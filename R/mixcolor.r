#' Mix two colors
#'
#' @param x,y     Colors (anything accepted by `col2rgb()`)
#' @param frac    Mixing fraction, proportion of `x` (default: 0.5)
#' @param alpha   Transparency (default: 1)
#'
#' @examples
#' mixcolor("#008DFFFF", "white")
#' mixcolor("#008DFFFF", "white", frac = 0.9)
#' mixcolor("#008DFFFF", "white", alpha = 0.2)
#' @export

mixcolor <- function(x, y, frac = 0.5, alpha = 1) {
  z <- frac * col2rgb(x) + (1 - frac) * col2rgb(y)
  alphaCol(rgb(z[1], z[2], z[3], maxColorValue = 255), alpha = alpha)
}
