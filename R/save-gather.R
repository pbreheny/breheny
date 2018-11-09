#' Save and combine (gather) results from batched R sessions
#'
#' @param res      Results to save
#' @param suffix   By default, files are named 'tmp1.rds', 'tmp2.rds', and so on.  If `suffix='a'`, then files will be named 'tmp1a.rds', 'tmp2a.rds', etc.  Furthermore, `Bgather()` will save results to '2018-11-07a.rds'.
#' @param safe     By default, files are named 'tmp1.rds', 'tmp2.rds', and so on.  If `suffix='a'`, then files will be named 'tmp1a.rds', 'tmp2a.rds', etc.
#'
#' @name save_gather
#'
#' @examples
#' \dontrun{
#' A <- rnorm(100)
#' B <- matrix(rnorm(100), ncol=10, nrow=10)
#' C <- array(rnorm(1000), dim=c(10,10,10))
#'
#' Bsave(A)
#' Bsave(A, "A")
#'
#' Bsave(list(A=A,B=B,C=C), "B")
#' Bsave(list(A=A,B=B,C=C), "C")}
NULL

#' @rdname save_gather

Bsave <- function(res, suffix="") {
  Suffix <- if (nchar(suffix) > 0) paste("-",suffix,sep="") else suffix
  NCA <- as.numeric(commandArgs(TRUE))
  if (length(NCA)) {
    saveRDS(res, file=paste0("tmp", NCA[1], Suffix, ".rds"))
  } else {
    saveRDS(res, file=paste0(Sys.Date(), Suffix,".rds"))
  }
}


#' @rdname save_gather

Bgather <- function (suffix = "", safe = FALSE)  {
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
