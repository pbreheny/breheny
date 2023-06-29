#' Convert between min/km pace and min/mi pace
#'
#' Calculation is simple, but the minutes/seconds math somewhat cumbersome
#'
#' @param m      Minutes
#' @param s      Seconds (default: 0)
#' @param dv     Distance (default: 1)
#'
#' @examples
#' km_to_mi(3, 50)
#' @export

km_to_mi <- function(m, s=0, d=1) {
  x <- d * 1.609 * (m*60 + s)
  lubridate::seconds_to_period(round(x))
}

#' @rdname km_to_mi
#' @examples
#' mi_to_km(6)
#' @export

mi_to_km <- function(m, s=0, d=1) {
  x <- d * (m*60 + s) / 1.609
  lubridate::seconds_to_period(round(x))
}
