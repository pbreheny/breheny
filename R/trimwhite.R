## Note: filename 
trimwhite <- function(base, margins) {
  if (missing(margins)) system(paste("pdfcrop --luatex ",base,".pdf",sep=""))
  else system(paste("pdfcrop --luatex --margins=", margins, " ", base,".pdf",sep=""))
  system(paste("mv ",base,"-crop.pdf ",base,".pdf",sep=""))
}
