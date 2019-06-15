#' Source all .R files in a directory
#'
#' @param directory   The directory to source
#'
#' @examples
#' \dontrun{
#' source.dir('R')
#' }
#'
#' @export

source.dir <- function(directory) {
  all.files <- list.files(directory)
  nc <- nchar(all.files)
  source.file <- all.files[substr(all.files, start=nc-1, stop=nc)==".R"]
  if (length(source.file)!=0) {
    for (i in 1:length(source.file)) {
      source(paste0(directory, "/", source.file[i]))
    }
  }
}
