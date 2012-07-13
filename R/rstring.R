rstring <- function(n=1, length=20)
{
  val <- character(n)
  for (i in 1:n) {
    val[i] <- paste(sample(c(letters,LETTERS), length, replace=TRUE), collapse="")
  }
  val
}
