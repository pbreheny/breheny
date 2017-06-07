write.delim <- function(x, file, row.names=FALSE, ...) {
  if (row.names) {
    cat('\t', file=file)
    suppressWarnings(write.table(x, file=file, append=TRUE, sep='\t', quote=FALSE, ...))
  } else {
    write.table(x, file=file, sep='\t', row.names=FALSE, quote=FALSE, ...)
  }
}
