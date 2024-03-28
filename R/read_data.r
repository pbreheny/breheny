#' Download data and read into R
#'
#' These functions are downloading and reading data sets from my courses into an
#' R session. To keep the size of the package manageable, these data sets are
#' not included with the package and must be downloaded separately using
#' `download_data()`; this only needs to be done once.
#'
#' @param name  Name of data set to read/download
#'
#' @examples
#' download_data('anorexia')   # Download a specific data set
#' @export

download_data <- function(name) {
  for (id in name) {
    target <- paste0('https://raw.githubusercontent.com/IowaBiostat/data-sets/main/', id, '/', id, '.txt')
    path <- paste0(system.file("extdata", package="breheny"), '/', id, '.txt')
    download.file(target, path, mode='wb')
  }
}

#' @rdname download_data
#'
#' @examples
#' dat <- read_data(whickham)
#' head(dat)
#' @export

read_data <- function(name) {
  name <- as.character(substitute(name))
  path <- paste0(system.file("extdata", package="breheny"), '/', name, '.txt')
  if (!file.exists(path)) {
    download_data(name)
  }
  data.table::fread(path)
}
