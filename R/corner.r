corner <- function(X, corner = 1, n = 5) {
  d <- dim(X)
  if (corner == 1) {
    return(X[1:n, 1:n])
  }
  if (corner == 2) {
    return(X[1:n, (d[2] - n + 1):d[2]])
  }
  if (corner == 3) {
    return(X[(d[1] - n + 1):d[1], 1:n])
  }
  if (corner == 4) {
    return(X[(d[1] - n + 1):d[1], (d[2] - n + 1):d[2]])
  }
}
