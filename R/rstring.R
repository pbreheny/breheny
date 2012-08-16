rstring <- function(n=1, length=20, include.extra=FALSE, include.num=FALSE)
{
  val <- character(n)
  pool <- c(letters,LETTERS)
  if (include.extra) pool <- c(pool, c("!","@","#","$","%","^","^","&","*","(",")"))
  if (include.num) pool <- c(pool, 0:9)
  for (i in 1:n) {
    val[i] <- paste(sample(pool, length, replace=TRUE), collapse="")
  }
  val
}
