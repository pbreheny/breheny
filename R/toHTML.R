toHTML <- function(X,class="ctable",...)
  {
    require(xtable)
    display <- xtable(X)
    align(display) <- c("l",rep("r",ncol(X)))
    print(display,type="html",html.table.attributes=paste("class=",class,sep=""),...)
    ##print(display2,type="html",include.rownames=FALSE,html.table.attributes="class=\"sortable ctable\" width=100%")
  }
