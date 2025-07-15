#' Detach before attaching
#'
#' @param x      What to attach.
#' @param name   Name to use for attached database.  Default: name of `x`.
#' @param ...    Additional arguments to `attach()`.
#'
#' @examples
#' Attach(airquality)
#' Attach(airquality) # Ordinarily this would provoke warnings
#'
#' @export

Attach <- function(x, name = deparse(substitute(x)), ...) {
  if (is.element(name, search())) {
    detach(pos = match(name, search()))
  }
  attach(x, name = name, ...)
}
