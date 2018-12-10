#' Install bioconductor packages
#'
#' @examples
#' \dontrun{
#' bioc('RnaSeqSampleSize')}
#'
#' @export

bioc <- function (...) {
  source("https://bioconductor.org/biocLite.R")
  biocLite(...)
}
