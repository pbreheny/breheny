slider <- function(fun, from, to, res) {
  v <- from
  fun(v)
  tt <- tcltk::tktoplevel()
  f <- function(...) {
    vv <- as.numeric(tcltk::tclvalue(tv))
    if (vv != v) {
      v <<- vv
      fun(vv)
    }
  }
  tv <- tcltk::tclVar(init=v)
  s <- tcltk::tkscale(tt, command=f, from=from, to=to, variable=tv, resolution=res, orient="horiz", length=500)
  tcltk::tkpack(s)
}
