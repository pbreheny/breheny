Xtable <- function(X,disp.names=c("",colnames(X)),...)
  {
    require(xtable)
    xtab <- xtable(X,...)
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{@{}",paste(attr(xtab,"align"),collapse=""),"@{}}\n",sep="")
    cat("\\toprule\n")
    if (class(disp.names)=="character") disp.names <- list(disp.names)
    for (i in 1:length(disp.names))
      {
        x <- disp.names[[i]]
        for (j in 1:length(x))
          {
            if (substr(x[j],1,1)!="\\") cat("{\\bf ")
            if (substr(x[j],1,3)=="\\mr")
              {
                nr <- substr(x[j],4,4)
                cat("\\multirow{-",nr,"}*{\\begin{tabular}{@{}c@{}}\n",sep="")
                mr <- strsplit(x[j],"\\",fixed=TRUE)[[1]]
                cat(paste("{\\bf",mr[-1:-2],"}\\\\\n"))
                cat("\\end{tabular}}\n",sep="")
              }
            else cat(x[j])
            if (substr(x[j],1,1)!="\\") cat("}")
            if (j!= length(x)) cat(" & ")
          }
        if (length(x)!=1) cat("\\\\")
        cat("\n")
      }
    cat("\\midrule\n")
    print(xtab,only.contents=TRUE,hline=NULL,include.colnames=FALSE,...)
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
  }
