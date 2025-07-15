#' Simple wrapper to read_excel + pipe to data.table
#'
#' @param ...   Arguments to `readxl::read_excel()`
#'
#' @export

xlsdt <- function(...) {
  data.table::as.data.table(readxl::read_excel(...))
}
