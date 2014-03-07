Bsave <- function(res, suffix="") {
  Suffix <- if (nchar(suffix) > 0) paste("-",suffix,sep="") else suffix
  NCA <- as.numeric(commandArgs(TRUE))
  name <- as.character(match.call()[2])
  if (length(NCA)) {
    save(res, name, file=paste("tmp",NCA[1], Suffix,".RData",sep=""))
  } else {
    assign(name, res)
    save(list=name, file=paste(Sys.Date(), Suffix,".RData",sep=""))
  }
}
