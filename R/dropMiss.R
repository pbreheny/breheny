#' Drop missing columns from a data table
#'
#' Currently only works for data tables, and only for completely missing columns
#'
#' @param DT   The data table
#'
#' @return A data table; no columns will be entirely missing
#'
#' @examples
#' DT <- data.table::data.table(A=1:3, B=NA, C=c(runif(2), NA))
#' DropMiss(DT)
#'
#' @export

DropMiss <- function(DT) {
  n <- vapply(DT, function(x) sum(!is.na(x)), double(1))
  cols_to_delete <- names(n[which(n==0)])
  DT[, (cols_to_delete) := NULL]
  DT
}
