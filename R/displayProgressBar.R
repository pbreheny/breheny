displayProgressBar <- function(i,nI,increments=30)
  {
    if (nI < increments) increments <- nI
    if (i==1)
      {
        cat("Progress:\n")
        cat("|",rep(" ",increments),"|\n ",sep="")
      }
    display.iterates <- ceiling((1:increments) * (nI/increments))
    if (is.element(i,display.iterates)) cat("*")
    if (Sys.info()["sysname"]=="Windows") flush.console()
    if (i==nI) cat("\n")
  }
