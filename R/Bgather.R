Bgather <- function(suffix="",safe=FALSE)
{
  require(abind)

  ## Find files
  all.files <- list.files()
  if (suffix=="") pattern <- "tmp[[:alnum:]]+\\.RData" else {
    pattern <- paste("tmp[[:alnum:]]+-",suffix,"\\.RData",sep="")
  }
  matching.files <- all.files[grep(pattern,all.files)]
  
  ## Join objects
  res <- NULL
  types <- c("numeric","character","array","matrix")
  for (f in matching.files) {
    e <- new.env()
    load(f,envir=e)
    res.f <- get("res",envir=e)
    rc <- sapply(res.f,class)
    for (i in 1:length(rc)) {
      el <- names(rc)[i]
      if (rc[i] %in% types) {
        res[[el]] <- abind(res[[el]],res.f[[el]],along=1)
      } else res[[el]] <- res.f[[el]]
    }
  }
  
  ## Destroy
  if (!safe) {
    if (suffix=="") fn <- "res.RData" else fn <- paste(suffix,".RData",sep="")
    save(res,file=fn)
    for (f in matching.files) {
      system(paste("rm",f))
    }
  }
  invisible(res)
}
