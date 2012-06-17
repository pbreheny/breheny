antitable <- function(X)
  {
    x <- as.data.frame(X)
    n <- nrow(x)
    p <- ncol(x)
    val <- NULL
    for (i in 1:n)
      {
        n.i <- x[i,p]
        if (n.i > 0)
          {
            for (j in 1:n.i)
              {
                val <- rbind(val,x[i,-p])
              }
          }
      }
    colnames(val) <- colnames(x)[-p]
    rownames(val) <- NULL
    return(val)
  }
