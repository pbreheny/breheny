#' Import from clipboard to data.table
#'
#' @param ...   Arguments to `as.data.table()`
#'
#' @export

clipdt <- function(...) {
  read.table('clipboard') |> as.data.table(...)
}
