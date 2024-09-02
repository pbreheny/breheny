#' Write a tab-separated file, rounding floats
#'
#' @param object  A `data.table` (or coerceable)
#' @param precision  The number of digits after the decimal point to display (default: 8)
#' @param sep  Delimiter (default `\t`)
#' @param ...  Additional options to `data.table::fwrite()`
#'
#' @examples
#' dat <- data.table::data.table(a=rnorm(10), b=rexp(10))
#' write_tsv(dat, 4)
#' @export

write_tsv <- function(object, precision=8, sep='\t', ...) {
  if (inherits(object, 'data.table')) {
    dat <- copy(object)
  } else {
    dat <- data.table::as.data.table(object)
  }
  jj <- which(vapply(dat, function(x) class(x)[1], '') == 'numeric')
  fmt <- stringr::str_glue('%.{precision}f')
  for (j in jj) {
    set(dat, j=j, value=sprintf(fmt, dat[[j]]))
  }
  fwrite(dat, sep=sep, ...)
}
