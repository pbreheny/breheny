Xtable <- function(X, disp.names=colnames(X), row.names, indent, align = NULL, digits, save=FALSE, ...)
{
  val <- capture.output({
    require(xtable)
    if (class(disp.names)=="character") disp.names <- list(disp.names)
    if (missing(digits)) digits <- 2
    if (length(digits)==1) digits <- rep(digits, ncol(X))
    digits <- c(0, digits)
    if (missing(row.names)) {
      include.rownames <- if (is.null(rownames(X))) FALSE else TRUE
      if (include.rownames & !is.null(align)) align <- c("l", align)
    } else if (identical(row.names, FALSE)) {
      include.rownames <- FALSE
      if (!is.null(align)) align <- c("l", align)
    } else {
      if (!missing(indent)) row.names[indent] <- paste("*",row.names[indent])
      X <- data.frame(ROWNAMES=row.names, X)
      include.rownames <- FALSE
      if (!is.null(align)) {
        add <- if (length(align) < ncol(X)) c("l", "l") else "l"
        align <- c(add, align)
      }
      digits <- c(0, digits)
    }
    xtab <- xtable(X, align=align, digits=digits, ...)
    align <- align(xtab)
    if (!include.rownames) align <- align[-1]
    cat("\\begin{center}\n")
    cat("\\begin{tabular}{@{}",paste(align, collapse=""),"@{}}\n",sep="")
    cat("\\toprule\n")
    if (include.rownames) cat(" & ")
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
  })
  if (save) val else cat(val, sep="\n")
}
