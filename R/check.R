check <- function(x, y, ...) {
  checkResult <- all.equal(x, y, ...)
  if (class(checkResult)[1]=="logical") return(TRUE)
  xname <- gsub("()", "", match.call()[2])
  yname <- gsub("()", "", match.call()[3])
  message <- paste0("Problem in ", .test, "\n", xname, " not equal to ", yname, "\n", checkResult)
  stop(message, call.=FALSE)
}