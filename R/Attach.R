Attach <- function(x,name=deparse(substitute(x)),...)
  {
    if (is.element(name,search()))
      {
        pos <- match(name,search())
        detach(pos=pos)
      }
    attach(x,name=name,...)
  }
