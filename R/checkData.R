checkData <- function(Data) {
  for (j in 1:ncol(Data)) {
    cat("Variable: ",names(Data)[j],"\n",sep="")
    if (sum(!is.na(Data[,j])) == 0) {
      cat("All values NA\n")
      readline("Move on to next variable <press enter>:")
      next
    }
    ##cat("Missing: ",sum(is.na(Data[,j])),"\n",sep="")
    if (is.numeric(Data[,j]) | class(Data[,j])[1]=="POSIXlt" | class(Data[,j])[1]=="Date")
      {
        print(range(Data[,j],na.rm=TRUE))
        hist(Data[,j],xlab=names(Data)[j],breaks=nrow(Data)/10,col="gray",main=paste("Missing: ",sum(is.na(Data[,j])),sep=""))
      }
    if (is.character(Data[,j]) | is.factor(Data[,j])) print(table(Data[,j],useNA="ifany"))
    readline("Move on to next variable <press enter>:")
  }
}
