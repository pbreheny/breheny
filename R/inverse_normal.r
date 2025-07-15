#' Apply inverse normal rank transformation
#'
#' @param x  Vector of numeric values
#'
#' @examples
#' c(-5:5, NA, NA, NA) |> inverse_normal()
#' @export
inverse_normal <- function(x) {
  na_idx <- is.na(x)
  qx <- (rank(x, na.last = "keep") - .5) / (length(x) - sum(na_idx))
  qnorm(qx)
}
