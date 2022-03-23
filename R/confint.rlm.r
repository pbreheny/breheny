confint.rlm <- function(fit,parm=1:length(coef(fit)),level=.95)
  {
    SE <- sqrt(diag(vcov(fit)))
    rdf <- length(fit$wresid) - length(coef(fit))
    val <- matrix(c(coef(fit)-SE*qt(1-(1-level)/2,rdf),
                    coef(fit)+SE*qt(1-(1-level)/2,rdf)),
                  nrow=length(coef(fit)),ncol=2)
    colnames(val) <- paste(100*c((1-level)/2,1-(1-level)/2),"%")
    rownames(val) <- names(coef(fit))
    return(val[parm,])
  }

