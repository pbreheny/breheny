write.xls <- function(tab,filename,row.names=TRUE)
  {
    if (row.names)
      {
        cat("\t",file=filename)
        op <- options(warn=-1)
        write.table(tab,sep="\t",file=filename,append=TRUE)
        options(op)
      }
    else
      {
        write.table(tab,sep="\t",file=filename,row.names=FALSE)        
      }
  }
