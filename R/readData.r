#' Download data and read into R
#'
#' These functions are downloading and reading data sets from my courses into an R session.
#' To keep the size of the package manageable, these data sets are not included with the package and must be downloaded separately using `downloadData()`; this only needs to be done once.
#'
#' @param name    Name of data set to read
#'
#' @examples
#' \dontrun{
#' downloadData()         # Download all data sets
#' downloadData(bcTCGA)   # Download a specific data set}

downloadData <- function(name) {
  name <- as.character(substitute(name))
  for (id in name) {
    URL <- paste0('http://s3.amazonaws.com/pbreheny-data-sets/', id, '.txt')
    FILE <- paste0(system.file("data", package="breheny"), '/', id, '.txt')
    download.file(URL, FILE, mode='wb')
  }
}

#' @rdname downloadData
#'
#' @examples
#' \dontrun{
#' Data <- readData(whickham)
#' head(Data)}

readData <- function(name) {
  name <- as.character(substitute(name))
  FILE <- paste0(system.file("data", package="breheny"), '/', name, '.txt')
  if (!file.exists(FILE)) stop("You have to run downloadData() first; see ?downloadData", call.=FALSE)
  data.table::fread(FILE)
}
