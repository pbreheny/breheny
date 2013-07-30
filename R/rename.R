rename <- function(Data, old, new) {
  ind <- which(names(Data)==old)
  names(Data)[ind] <- new
  assign(as.character(match.call()[2]), Data, env=.GlobalEnv)
}
revalue <- function(Data, name, old, new) {
  if (is.na(old)) {
    Data[is.na(Data[,name], name)] <- new
  } else {
    ind <- which(Data[,name]==old)
    Data[ind, name] <- new
  }
  assign(as.character(match.call()[2]), Data, env=.GlobalEnv)
}
