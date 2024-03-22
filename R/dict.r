#' Create a key-value dictionary
#'
#' Keys must be valid
#'
#' @param dots  Either supply a single, unnamed, two-column table or a list of
#'              `key=value` pairs; see examples. If a two-column table, first
#'              column has the keys.
#'
#' @examples
#' d <- dict(a=5, b=10)
#' d$b
#' d <- data.frame(letters[1:10], 1:10) |>
#'   dict()
#' d$f
#' @export

dict <- function(...) {
  dots <- list(...)
  out <- new.env()
  if (length(dots) == 1 && is.null(names(dots))) {
    tab <- dots[[1]]
    if (length(dim(tab)) != 2 || dim(tab)[2] != 2) stop('key-value table must be an array with two columns', call.=FALSE)
    for (i in 1:nrow(tab)) assign(tab[i, 1], tab[i, 2], envir=out)
  } else {
    if (any(is.null(names(dots)))) stop('all arguments to dict() must be names (that\'s the key!)', call.=FALSE)
    for (i in 1:length(dots)) assign(names(dots)[i], dots[i], envir=out)
  }
  out
}
