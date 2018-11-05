#' Detach before attaching
#'
#' @examples
#' Attach(airquality)
#' Attach(airquality)   # Ordinarily this would provoke warnings
#'
#' @export

Attach <- function(x, name=deparse(substitute(x)), ...) {
  if (is.element(name, search())) {
    pos <- match(name, search())
    detach(pos=pos)
  }
  attach(x, name=name, ...)
}
