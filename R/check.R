#' Checks whether two vectors are equal; wrapper for `all.equal`
#'
#' @param x,y                Vectors to test
#' @param check.attributes   Default: FALSE
#'
#' @examples
#' .test <- 'Example'
#' check(1,1)
#' check(1,2)
#'
#' @export

check <- function(x, y, check.attributes=FALSE, ...) {
  if (missing(y)) {
    xname <- gsub("()", "", match.call()[2])
    if (x==TRUE) return(TRUE)
    message <- paste0("Problem in ", .test, "\n", xname, " FALSE")
  }
  checkResult <- all.equal(x, y, check.attributes=check.attributes, ...)
  if (class(checkResult)[1]=="logical") return(TRUE)
  xname <- gsub("()", "", match.call()[2])
  yname <- gsub("()", "", match.call()[3])
  if (!exists(.test)) .test <- ''
  message <- paste0("Problem in ", .test, "\n", xname, " not equal to ", yname, "\n", checkResult)
  stop(message, call.=FALSE)
}
