#' Convert factor to number
#'
#' @param f   Factor with levels like 20, 40, 60, that are simply numbers
#'
#' @export

factor2num <- function(f) {
  return(as.numeric(levels(f))[f])
}
