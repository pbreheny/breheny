#' Generate random double exponential variates
#'
#' @param n      Number of random variates to generate.
#' @param mean   Mean; default: 0
#' @param var    Variance; default: 1
#'
#' @examples
#' rdex(10)
#' a <- rdex(10000, mean=3, var=4)
#' var(a)
#'
#' @export

rdex <- function(n, mean=0, var=1) {
  mean + sample(c(-1,1),n,replace=TRUE) * rexp(n, sqrt(2/var))
}
