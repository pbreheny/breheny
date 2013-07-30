write.delim <- function(x, file, sep="\t", ...) {
  cat(sep, file=file)
  suppressWarnings(write.table(x, file=file, append=TRUE, sep=sep, ...))
}
