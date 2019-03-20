#' Parse latex .aux files for fig/tab/etc. numbers
#'
#' @param aux   .aux files (or a vector of .aux files)
#' @param id    Function assumes labels are of form category:label; `id` is a character vector of categories to search for
#'
#' @examples
#' \dontrun{
#' parse_aux('file.aux')
#' parse_aux(c('ch1.aux', 'ch2.aux'), 'ex')}
#'
#' @export

parse_aux <- function(aux, id = c('Fig', 'Tab', 'Ex', 'ex')) {
  out <- vector('list', length(id))

  # Build key/val tables
  for (i in 1:length(id)) {
    keyTable <- NULL
    for (a in aux) {
      raw <- suppressWarnings(system(paste0("grep '", id[i], ":' ", a), intern=TRUE))
      if (length(raw)) {
        raw <- gsub("\\\\newlabel\\{", "", raw)
        key <- gsub("Fig:", "", stringr::str_split(raw, "\\}\\{\\{", simplify=TRUE)[,1])
        val <- gsub("\\}.*", "", stringr::str_split(raw, "\\}\\{\\{", simplify=TRUE)[,2])
        keyTable <- rbind(keyTable, data.frame(key=key, val=val, stringsAsFactors=FALSE))
      }
    }
    out[[i]] <- keyTable
  }
  names(out) <- id

  # Return
  if (length(id)==1) {
    return(out[[1]])
  } else {
    return(out)
  }
}
