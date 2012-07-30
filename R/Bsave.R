Bsave <- function(res,suffix="")
{
  Suffix <- if (nchar(suffix) > 0) paste("-",suffix,sep="") else suffix
  NCA <- as.numeric(commandArgs(TRUE))
  if (length(NCA)) {
    save(res,file=paste("tmp",NCA[1], Suffix,".RData",sep=""))
  } else {
    save(res,file=paste("res", Suffix,".RData",sep=""))
  }
}
