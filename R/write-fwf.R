#' Generate fixed width file in R
#'
#' https://gist.github.com/haozhu233/28d1309b58431f4929f78243054f1f58
#'
#' @param dt           The data to be printed
#' @param width        Either a single number or a vector of widths
#' @param con          Connection, as in `writeLines()`; default is `stdout()`.
#' @param justify      "l", "r" or something like "lrl" for left, right, left.
#' @param replace_na   What to print in places of missing values; default: "NA"
#'
#' @examples
#' dt <- data.frame(a = 1:3, b = NA, c = c('a', 'b', 'c'))
#' write_fwf(dt, width = c(4, 4, 3))
#'
#' @export

write_fwf = function(dt, width, con=stdout(),
                     justify = "l", replace_na = "NA") {
  fct_col = which(sapply(dt, is.factor))
  if (length(fct_col) > 0) {
    for (i in fct_col) {
      dt[[i]] <- as.character(dt[[i]])
    }
  }
  dt[is.na(dt)] = replace_na
  n_col = ncol(dt)
  justify = unlist(strsplit(justify, ""))
  justify = as.character(factor(justify, c("l", "r"), c("-", "")))
  if (n_col != 1) {
    if (length(width) == 1) width = rep(width, n_col)
    if (length(justify) == 1) justify = rep(justify, n_col)
  }
  sptf_fmt = paste0(
    paste0("%", justify, width, "s"), collapse = ""
  )
  tbl_content = do.call(sprintf, c(fmt = sptf_fmt, dt))
  tbl_header = do.call(sprintf, c(list(sptf_fmt), names(dt)))
  out = c(tbl_header, tbl_content)
  writeLines(out, con)
}
