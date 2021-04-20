#' Runs R code in testing folder
#'
#' Tests are expected to be in a directory called `tests`.  Intended to be used as
#' an Rstudio addin (which is why it doesn't accept arguments).
#'
#' @export

run_ex <- function() {
  devtools::run_examples()
}
