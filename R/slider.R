slider <- function(fun,from,to,res)
  {
    require(tcltk)
    v <- from
    fun(v)
    tt <- tktoplevel()
    f<-function(...)
      {
        vv <- as.numeric(tclvalue(tv))
        if (vv != v)
          {
            v <<- vv
            fun(vv)
          }
      }
    tv <- tclVar(init=v)
    s <- tkscale(tt,command=f,from=from,to=to,variable=tv,resolution=res,orient="horiz",length=500)
    tkpack(s)
  }
