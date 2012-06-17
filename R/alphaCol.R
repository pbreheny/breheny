alphaCol <- function(col,alpha)
  {
    x <- t(col2rgb(col))
    return(rgb(prop.table(x),alpha=alpha))
  }

