soft <- function(x, l) {
  v <- x
  v[x > l] <- x[x > l] - l
  v[abs(x) <= l] <- 0
  v[x < -l] <- x[x < -l] + l
  v
}
