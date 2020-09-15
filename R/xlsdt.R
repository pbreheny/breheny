#' Simple wrapper to read_excel + pipe to data.table
#'
#' @export

xlsdt <- function(...) {
  data.table::as.data.table(readxl::read_excel(...))
}

