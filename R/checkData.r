#' Quick inspection of columns in a data frame / table for weirdness
#'
#' Provides histograms and ranges for numeric features, and tables for categorical features, along with missing data information.
#'
#' @param Data   A data frame (or data table)
#'
#' @examples
#' checkData(airquality)
#'
#' @export

checkData <- function(Data) {
  cat("Number of instances: ", nrow(Data), "\n")
  cat("Number of features: ", ncol(Data), "\n")

  for (j in 1:ncol(Data)) {
    cat("Variable: ", names(Data)[j],"\n", sep="")
    if (sum(!is.na(Data[[j]])) == 0) {
      cat("All values NA\n")
      readline("Move on to next variable <press enter>:")
      next
    }
    if (is.numeric(Data[[j]])) {
      cat('  Range:', range(Data[[j]], na.rm=TRUE), '\n')
      Hist(Data[[j]], n=nrow(Data)/10, xlab=names(Data)[j], main=paste0("Missing: ",sum(is.na(Data[[j]]))))
    }
    if (class(Data[[j]])[1]=="POSIXlt" | class(Data[[j]])[1]=="Date") {
      print(range(Data[[j]],na.rm=TRUE))
      op <- par(fg="white")
      hist(Data[[j]], breaks=nrow(Data)/10, col="gray", las=1, freq=TRUE, xlab=names(Data)[j],
           main=paste0("Missing: ",sum(is.na(Data[[j]]))))
      par(op)
    }
    if (is.character(Data[[j]]) | is.factor(Data[[j]]) | is.logical(Data[[j]])) {
      if (!any(duplicated(Data[[j]]))) {
        cat("All values unique: ", Data[[j]][1], ', ', Data[[j]][2], ', ', Data[[j]][3], ', ...\n', sep='')
      } else {
        print(table(Data[[j]], useNA="ifany"))
      }
    }
    readline("Move on to next variable <press enter>:")
  }
}
