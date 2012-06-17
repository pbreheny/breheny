xscale.components.log2 <- function(lim,...) xscale.components.log(lim,base=2, ...)
xscale.components.log10 <- function(lim,...) xscale.components.log(lim,base=10, ...)
xscale.components.log <- function(base,...)
  {
    ans <- xscale.components.default(base,...)
    ##print(ans)
    base <- as.numeric(strsplit(ans$bottom$labels$labels,"^",fixed=TRUE)[[1]][1])
    ##print(base)
    ##ans$bottom$labels$labels <- parse(text=ans$bottom$labels$labels)
    ans$bottom$labels$labels <- base^ans$bottom$labels$at
    ##tick.at <- logTicks(base^lim,loc=c(1,2))
    ##ans$bottom$ticks$at <- log(tick.at, base)
    ##ans$bottom$labels$at <- log(tick.at, base)
    ##ans$bottom$labels$labels <- as.character(tick.at)
    return(ans)
  }
yscale.components.log2 <- function(lim,...) yscale.components.log(lim,base=2, ...)
yscale.components.log10 <- function(lim,...) yscale.components.log(lim,base=10, ...)
yscale.components.log <- function(base,...)
  {
    ans <- yscale.components.default(...)
    base <- as.numeric(strsplit(ans$left$labels$labels,"^",fixed=TRUE)[[1]][1])
    ans$left$labels$labels <- base^ans$left$labels$at
    return(ans)
  }
