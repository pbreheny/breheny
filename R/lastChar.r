lastChar <- function(x, n=1) {
    substr(x, nchar(x)-n+1, nchar(x))
}
