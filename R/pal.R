pal <- function(n, alpha=1)
{
  hcl(seq(15,375,len=n+1), l=60, c=150, alpha=alpha)[1:n]
}
