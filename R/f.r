#' Compact and pipe-friendly wrapper to sprintf
#'
#' .p is the same, but multiplies by 100 first (for percents)
#'
#' @param x        A floating point number
#' @param digits   Number of decimals to display
#'
#' @examples
#' runif(1) |> .f(2)
#' @export

.f <- function(x, digits) {
  paste0('%.', digits, 'f') |>
    sprintf(x) |>
    as.numeric() |>
    format(big.mark = ",", nsmall=digits)
}

#' @rdname .f
#' @export
.p <- function(x, digits) {
  .f(100*x, digits)
}
