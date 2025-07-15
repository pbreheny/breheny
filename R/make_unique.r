#' Ensure all entries in a string are uniquely labeled
#'
#' Leaves entries untouched if they are already unique; adds a suffix like '_1', etc. if necessary.
#'
#' @param x     A character vector or something coerceable to a character vector.
#' @param sep   Separator between label and suffix.  Default: _.
#'
#' @examples
#' make_unique(LETTERS[c(1, 1, 2, 2, 2, 3, 4)])
#'
#' @export

make_unique <- function(x, sep = "_") {
  x <- as.character(x)
  tab <- table(x)
  tab <- tab[tab > 1]
  lentab <- length(tab)
  if (lentab > 0) {
    u <- names(tab)
    for (i in 1:lentab) {
      n <- tab[i]
      x[x == u[i]] <- paste0(x[x == u[i]], sep, formatC(1:n, width = 1 + floor(log10(n)), flag = "0"))
    }
  }
  x
}
