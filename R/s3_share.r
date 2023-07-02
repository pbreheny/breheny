#' Share Amazon S3 file
#'
#' Shares Amazon S3 file(s) and returns either a url or R script.  The latter is particularly useful for batch downloads and specifying file save paths.
#'
#' Requires aws command line functionality.
#'
#' @param file         Character vector of files to share.If path is included, it will be included in the download script save location.
#' @param out          A connection object or character string, as in `[writeLines()]`, to which R commands are to be saved.  If nothing is supplied, output is to console.
#' @param expiration   How long until URLs expire (in hours)? Numeric.  Default: 168 (1 week).
#' @param r            Output R code (as opposed to link)? By default, the function will return links if `out=stdout()` and R code if `out` otherwise. However, this can be overridden by explicitly setting r to be `TRUE` or `FALSE`.
#'
#' @examples
#' \dontrun{
#' s3_share('~/web/pdf/BrehenyCV.pdf')
#' s3_share(c('~/web/pdf/BrehenyCV.pdf', '~/web/pdf/mnet.pdf'), r=TRUE)
#' }
#' @export

s3_share <- function(file, out=stdout(), expiration=168, r) {
  checkmate::assertNumeric(expiration, min.len=1, max.len=1)
  checkmate::assertFileExists(file)
  if (missing(r)) r <- !inherits(out, 'terminal')
  checkmate::assertLogical(r, min.len=1, max.len=1)
  cmd <- vector('character', length(file))
  ex <- expiration * 60 * 60
  for (i in seq_along(file)) {
    bn <- basename(file[i])
    dn <- dirname(file[i])
    config <- readLines(paste0(dn, '/.dat-config'))
    bucket <- stringr::str_split_fixed(config, ':', 2)[1,2]
    profile <- stringr::str_subset(config, 'profile: ') |> stringr::str_replace('profile: ', '')
    aws_cmd <- paste0('aws s3 presign s3://', bucket, '/', bn, ' --expires-in ', ex)
    if (length(profile)) aws_cmd <- paste(aws_cmd, '--profile', profile)
    url <- system(aws_cmd, intern=TRUE)
    if (r) {
      cmd[i] <- paste0("download.file('", url, "', '", dn, "/", bn, "')")
    } else {
      cmd[i] <- url
    }
  }
  writeLines(cmd, con=out)
}
