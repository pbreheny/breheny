#' Share Amazon CloudFront file via presigned url
#'
#' Shares Amazon CloudFront file. Currently only set up for teaching files;
#' see `bin/signed-url`.
#'
#' @returns Either a url or R script. The latter is particularly useful for
#'   batch downloads and specifying file save paths.
#'
#' @param file         Character vector of files to share. If path is included,
#'   it will be included in the download script save location.
#' @param out          A connection object or character string, as in
#'   `[writeLines()]`, to which R commands are to be saved.  If nothing is
#'   supplied, output is to console.
#' @param expiration   How long until URLs expire (in days)? Numeric.
#'   Default: 365.
#' @param r            Output R code (as opposed to link)? By default, the
#'   function will return links if `out=stdout()` and R code if `out`
#'   otherwise. However, this can be overridden by explicitly setting r to be
#'   `TRUE` or `FALSE`.
#'
#' @examples
#' \dontrun{
#' signed_url("~/pdf/teaching/7110-f23/dk-1.pdf")
#' signed_url("~/pdf/teaching/7110-f23/dk-*.pdf", r = TRUE)
#' }
#' @export

signed_url <- function(file, out = stdout(), expiration = 365, r) {
  checkmate::assertNumeric(expiration, min.len = 1, max.len = 1)
  checkmate::assertFileExists(file)
  if (missing(r)) r <- !inherits(out, "terminal")
  checkmate::assertLogical(r, min.len = 1, max.len = 1)
  cmd <- vector("character", length(file))
  for (i in seq_along(file)) {
    bn <- basename(file[i])
    cli_cmd <- paste0("signed-url ", file[i])
    url <- system(cli_cmd, intern = TRUE)
    if (r) {
      cmd[i] <- paste0("download.file('", url, "', '", bn, "')")
    } else {
      cmd[i] <- url
    }
  }
  writeLines(cmd, con = out)
}
