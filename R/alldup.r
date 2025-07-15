#' Show all duplicate entries in a data frame/table/vector
#'
#' Not just the "duplicated" ones.
#'
#' @param DF   A data frame
#' @param by   The column of `DF` you want to look at duplicates of
#'
#' @examples
#' DF <- data.frame(x = 1:3, y = c(1, 1, 2))
#' alldup(DF, "x")
#' alldup(DF, "y")
#' alldup(DF$y)
#' @export

alldup <- function(DF, by) {
  if (is.vector(DF)) {
    DF[duplicated(DF) | duplicated(DF, fromLast = TRUE)]
  } else {
    DF[duplicated(DF[[by]]) | duplicated(DF[[by]], fromLast = TRUE), ]
  }
}
