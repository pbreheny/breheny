## Applies a vector of functions to a list of arguments
## Returns a matrix or vector, as appropriate
##
## FUN: A vector of characters naming the functions to be applied
fapply <- function(FUN,...)
{
  val <- NULL
  for (i in 1:length(FUN)) val <- cbind(val,do.call(FUN[i],list(...)))
  colnames(val) <- FUN
  drop(val)
}
