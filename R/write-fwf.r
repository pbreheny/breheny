#' Generate fixed width file in R
#'
#' https://gist.github.com/haozhu233/28d1309b58431f4929f78243054f1f58
#'
#' @param dt           The data to be printed
#' @param width        Either a single number or a vector of widths
#' @param con          Connection, as in `writeLines()`; default is `stdout()`.
#' @param align        "l", "r" or something like "lrl" for left, right, left.
#' @param na           What to print in places of missing values; default: "NA"
#' @param col.names    Print column names?  Default: TRUE
#'
#' @examples
#' dt <- data.frame(a = 1:3, b = NA, c = c("a", "b", "c"))
#' write_fwf(dt, width = c(4, 4, 3))
#' write_fwf(dt, 5)
#'
#' X <- matrix(LETTERS[1:9], 3, 3, dimnames = list(1:3, paste0("V", 1:3)))
#' write_fwf(X, 6)
#' @export

write_fwf <- function(dt, width, con = stdout(), align = "l", na = "NA", col.names = TRUE) {
  if (!inherits(dt, "data.frame")) dt <- as.data.frame(dt)
  fct_col <- which(sapply(dt, is.factor))
  if (length(fct_col) > 0) {
    for (i in fct_col) {
      dt[[i]] <- as.character(dt[[i]])
    }
  }
  dt[is.na(dt)] <- na
  n_col <- ncol(dt)
  align <- unlist(strsplit(align, ""))
  align <- as.character(factor(align, c("l", "r"), c("-", "")))
  if (n_col != 1) {
    if (length(width) == 1) width <- rep(width, n_col)
    if (length(align) == 1) align <- rep(align, n_col)
  }
  sptf_fmt <- paste0(
    paste0("%", align, width, "s"),
    collapse = ""
  )
  tbl_content <- do.call(sprintf, c(fmt = sptf_fmt, dt))
  tbl_header <- do.call(sprintf, c(list(sptf_fmt), names(dt)))
  out <- if (col.names) c(tbl_header, tbl_content) else tbl_content
  writeLines(out, con)
}
