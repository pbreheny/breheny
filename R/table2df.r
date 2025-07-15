#' Convert a table to a (disaggregated) data frame / table
#'
#' Note that as.data.frame can do this, but you get aggregated rows with a `Freq` column
#'
#' @param Tab   The table
#'
#' @examples
#' table2dt(Titanic)
#' head(table2df(Titanic))
#' @export

table2dt <- function(Tab) {
  tmp <- data.table::as.data.table(Tab)
  tmp[rep(1:nrow(tmp), tmp$N), -ncol(tmp)]
}

#' @describeIn table2dt   Convert a table to a (disaggregated) data frame
#' @export

table2df <- function(Tab) {
  tmp <- as.data.frame(Tab)
  tmp <- tmp[rep(1:nrow(tmp), tmp$Freq), -ncol(tmp)]
  rownames(tmp) <- NULL
  tmp
}
