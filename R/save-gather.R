#' Save and combine (gather) results from batched R sessions
#'
#' `Bsave()` is run from R; its companion function is to be run through the command line via `gather`, which is located in `Dropbox/bin`.
#'
#' @param res      Results to save
#' @param suffix   By default, files are named 'tmp1.rds', 'tmp2.rds', and so on.  If `suffix='a'`, then files will be named 'tmp1-a.rds', 'tmp2-a.rds', etc.  `gather` will then save results to '2018-11-07-a.rds'.
#'
#' @examples
#' # These examples are illustrative; for working examples, see Dropbox/bin/test
#'
#' A <- rnorm(100)
#'
#' # If run interactively
#' # Bsave(A)        # Saves to 2019-06-14.rds
#' # Bsave(A, "A")   # Saves to 2019-06-14-A.rds
#'
#' # If run in batch (with a number as first command line argument)
#' # Bsave(A)        # Saves to tmp1.rds; gathered to 2019-06-14.rds
#' # Bsave(A, "A")   # Saves to tmp1-A.rds; gathered to 2019-06-14-A.rds
#'
#' @export

Bsave <- function(res, suffix="") {
  Suffix <- if (nchar(suffix) > 0) paste("-",suffix,sep="") else suffix
  NCA <- as.numeric(commandArgs(TRUE))
  if (length(NCA)) {
    saveRDS(res, file=paste0("tmp", NCA[1], Suffix, ".rds"))
  } else {
    saveRDS(res, file=paste0(Sys.Date(), Suffix,".rds"))
  }
}
