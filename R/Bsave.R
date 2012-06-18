Bsave <- function(res,suffix="")
{
  NCA <- as.numeric(commandArgs(TRUE))
  if (length(NCA)) {
    save(res,file=paste("tmp",NCA[1],"-",suffix,".RData",sep=""))
  } else {
    save(res,file=paste(Sys.Date(),"-",suffix,".RData",sep=""))
  }
}
