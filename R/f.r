#' Compact and pipe-friendly wrapper to sprintf
#'
#' `.p()` is the same as `.f()`, but multiplies by 100 first (for percents).
#'
#' @param x        A floating point number
#' @param digits   Number of decimals to display
#'
#' @name dot-f
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

#' @rdname dot-f
#' @export
.p <- function(x, digits) {
  .f(100*x, digits)
}
