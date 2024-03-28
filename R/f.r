#' Compact and pipe-friendly wrapper to sprintf
#'
#' @param x        A floating point number
#' @param digits   Number of decimals to display
#'
#' @examples
#' runif(1) |> .f(2)
#' @export

.f <- function(x, digits) {
  sprintf(paste0('%.', digits, 'f'), x)
}
