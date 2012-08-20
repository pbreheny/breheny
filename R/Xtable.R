Xtable <- function(X, disp.names=colnames(X), row.names, indent, align = NULL, digits = NULL, ...)
  {
    require(xtable)
    if (class(disp.names)=="character") disp.names <- list(disp.names)
    if (missing(row.names)) {
      include.rownames <- if (is.null(rownames(X))) FALSE else TRUE
    } else if (row.names==FALSE) {
      include.rownames <- FALSE
    } else {
      if (!missing(indent)) row.names[indent] <- paste("*",row.names[indent])
      X <- data.frame(ROWNAMES=row.names, X)
      include.rownames <- FALSE
      if (!is.null(align)) {
        add <- if (length(align) < ncol(X)) c("l", "l") else "l"
        align <- c(add, align)
      }
      if (!is.null(digits)) {
        add <- if (length(digits) < ncol(X)) c(0, 0) else 0
        digits <- c(add, digits)
      }
    }
    xtab <- xtable(X, align=align, digits=digits, ...)
    if (!include.rownames) align <- align[-1]
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{@{}",paste(align, collapse=""),"@{}}\n",sep="")
    cat("\\toprule\n")
    if (!missing(row.names)) cat(" & ")
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
        ## if (length(x)!=1) cat("\\\\")
        cat(" \\\\")
        cat("\n")
      }
    cat("\\midrule\n")
    if (include.rownames) {
      x <- print(xtab, only.contents=TRUE, hline=NULL, include.colnames=FALSE, file=tempfile(), ...)
    } else {
      x <- gsub("\n  \\* ", "\n \\\\phantom{000} ", print(xtab, only.contents=TRUE, hline=NULL, include.colnames=FALSE, include.rownames=FALSE, file=tempfile(), ...))
    }
    cat(x)
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
  }
