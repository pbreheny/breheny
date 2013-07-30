List <- function (..., labels = TRUE) {
  dotlist <- list(...)
  lname <- names(dotlist)
  name <- vname <- as.character(sys.call())[-1]
  for (i in 1:length(dotlist)) {
    vname[i] <- if (length(lname) && lname[i] != "") lname[i] else name[i]
  }
  names(dotlist) <- vname[1:length(dotlist)]
  dotlist
}
