drawBand <- function(x, l, u, border = NA, col = "gray90", ...) {
  polygon(c(x, rev(x)), c(l, rev(u)), border = border, col = col, ...)
}
