#' Show all duplicate entries in a data frame/table
#'
#' Not just the "duplicated" ones
#'
#' @param DF   A data frame
#' @param by   The column of `DF` you want to look at duplicates of
#'
#' @examples
#' DF <- data.frame(x = 1:3, y = c(1, 1, 2))
#' alldup(DF, "x")
#' alldup(DF, "y")
#' @export

alldup <- function(DF, by) {
  DF[duplicated(DF[[by]]) | duplicated(DF[[by]], fromLast=TRUE),]
}
