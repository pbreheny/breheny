Bgather <- function (suffix = "", safe = FALSE) 
{
  require(abind, quietly=TRUE)
  all.files <- list.files()
  Suffix <- if (nchar(suffix) > 0) paste("-",suffix,sep="") else suffix
  pattern <- paste("tmp[[:alnum:]]+", Suffix, "\\.RData", sep = "")
  matching.files <- all.files[grep(pattern, all.files)]
  
  res <- NULL
  types <- c("numeric", "character", "array", "matrix")
  for (f in matching.files) {
    e <- new.env()
    load(f, envir = e)
    res.f <- get("res", envir = e)
    rc <- class(res)
    if (rc == "list") {
      rc <- sapply(res.f, class)
      for (i in 1:length(rc)) {
        if (rc[i] %in% types) {
          res[[i]] <- abind(res[[i]], res.f[[i]], along = 1)
        }
        else res[[i]] <- res.f[[i]]
      }
      names(res) <- names(res.f)
    }
    else {
      if (rc %in% types) {
        res <- abind(res, res.f, along = 1)
      }
      else res <- res.f
    }
  }
  if (!safe) {
    name <- get("name", envir = e)
    assign(name, res)
    save(list=name, file = paste(Sys.Date(), Suffix, ".RData", sep = ""))
    for (f in matching.files) {
      system(paste("rm", f))
    }
  }
  invisible(res)
}
