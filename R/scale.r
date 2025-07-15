xscale.components.log2 <- function(lim, ...) xscale.components.log(lim, base = 2, ...)
xscale.components.log10 <- function(lim, ...) xscale.components.log(lim, base = 10, ...)
xscale.components.log <- function(base, ...) {
  ans <- lattice::xscale.components.default(base, ...)
  base <- as.numeric(strsplit(ans$bottom$labels$labels, "^", fixed = TRUE)[[1]][1])
  ans$bottom$labels$labels <- base^ans$bottom$labels$at
  return(ans)
}
yscale.components.log2 <- function(lim, ...) yscale.components.log(lim, base = 2, ...)
yscale.components.log10 <- function(lim, ...) yscale.components.log(lim, base = 10, ...)
yscale.components.log <- function(base, ...) {
  ans <- lattice::yscale.components.default(...)
  base <- as.numeric(strsplit(ans$left$labels$labels, "^", fixed = TRUE)[[1]][1])
  ans$left$labels$labels <- base^ans$left$labels$at
  return(ans)
}
