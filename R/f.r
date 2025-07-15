#' Compact and pipe-friendly wrapper to sprintf
#'
#' `.p()` is the same as `.f()`, but multiplies by 100 first (for percents).
#' `.fm()` is the same as `.f()`, but puts braces around commas for use in
#' latex math mode.
#'
#' @param x        A floating point number
#' @param digits   Number of decimals to display
#'
#' @name dot-f
#'
#' @examples
#' runif(1) |> .f(2)
#' runif(1) |> .p(2)
#' runif(1, 100000, 200000) |> .fm(2)
#' @export

.f <- function(x, digits) {
  paste0("%.", digits, "f") |>
    sprintf(x) |>
    as.numeric() |>
    format(big.mark = ",", nsmall = digits)
}

#' @rdname dot-f
#' @export
.p <- function(x, digits) {
  .f(100 * x, digits)
}

#' @rdname dot-f
#' @export
.fm <- function(x, digits) {
  gsub(",", "{,}", .f(x, digits))
}
