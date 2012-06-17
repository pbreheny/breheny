## Note: filename 
trimwhite <- function(base)
  {
    system(paste("pdfcrop ",base,".pdf",sep=""))
    system(paste("mv ",base,"-crop.pdf ",base,".pdf",sep=""))
  }
