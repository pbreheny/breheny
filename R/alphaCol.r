#' Take a solid color and turn it semi-transparent
#'
#' @param col     Color
#' @param alpha   Semi-transparency value
#'
#' @examples
#' alphaCol('red', 0.5)
#'
#' @export

alphaCol <- function(col, alpha) {
  x <- t(col2rgb(col))
  rgb(x, alpha=alpha*255, maxColorValue=255)
}
