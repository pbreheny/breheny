median.survfit <- function(x)
{
  require(survival)
  X <- survival:::survmean(x,rmean=max(x$time))$matrix
  m <- if (is.matrix(X)) X[,"median"] else X["median"]
  m
}
