#' Solutions to the quadratic equation
#'
#' Some extra effort is dedicated to making the solution stable even when a is close to zero.
#'
#' @param a,b,c   Coefficients: ax^2 + bx + c = 0
#'
#' @examples
#' qe(1, 3, -4)  # -4, 1
#' qe(0, 1, 2)   # Just -2
#'
#' @export

qe <- function(a,b,c) {
  if (b >= 0) {
    out <- c((-b - sqrt(b^2-4*a*c)) / (2*a),
             (2*c) / (-b - sqrt(b^2-4*a*c)))
  } else {
    out <- c((-b + sqrt(b^2-4*a*c)) / (2*a),
             (2*c) / (-b + sqrt(b^2-4*a*c)))
  }
  out[is.finite(out)]
}
