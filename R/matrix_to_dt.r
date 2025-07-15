#' Convert matrix to data.table
#'
#' @param x    The matrix
#' @param row  Name for row variable (default: 'row')
#' @param col  Name for col variable (default: 'col')
#' @param val  Name for val variable (default: 'val')
#'
#' @examples
#' x <- matrix(runif(9), 3, 3)
#' rownames(x) <- letters[1:3]
#' colnames(x) <- letters[4:6]
#' matrix_to_dt(x)
#' matrix_to_dt(x, "sample", "gene", "expression")
#' @export

matrix_to_dt <- function(x, row = "row", col = "col", val = "val") {
  out <- as.table(x) |> data.table::as.data.table(x)
  names(out) <- c(row, col, val)
  out
}
