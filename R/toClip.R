toClip <- function(x, row.names=FALSE, sep="\t", ...) {
  tmp <- tempfile()
  if (is.null(dim(x))) {
    writeLines(as.character(x), con=tmp)
  } else {
    write.table(x, file=tmp, sep=sep , row.names=row.names, ...)
  }
  system(paste0("cat ", tmp, " | xclip"))
}
