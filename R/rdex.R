rdex <- function(n,mean,var)
  {
    return(mean+sample(c(-1,1),n,replace=TRUE)*rexp(n,sqrt(2/var)))
  }
