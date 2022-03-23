#' Calculate relative risk from a 2x2 table
#'
#' @param x        Table
#' @param margin   As in `prop.table()`
#'
#' @examples
#' m <- matrix(4:1, 2)
#' rr(m, 1)
#' rr(m, 2)
#'
#' @export

rr <- function(x, margin) {
  stopifnot(margin %in% 1:2)
  p <- prop.table(x, margin)
  switch(margin, p[1,1]/p[2,1], p[1,1]/p[1,2])
}
