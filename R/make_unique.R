#' @export
make_unique <- function(x, sep='_') {
  x <- as.character(x)
  tab <- table(x)
  tab <- tab[tab > 1]
  lentab <- length(tab)
  if (lentab > 0) {
    u <- names(tab)
    for (i in 1:lentab) {
      n <- tab[i]
      x[x == u[i]] <- paste0(x[x == u[i]], sep, formatC(1:n, width = 1 + floor(log10(n)), flag = "0"))
    }
  }
  x
}
