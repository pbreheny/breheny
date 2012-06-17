estimate <- function(lambda,fit,ci=FALSE,alpha=.05,t.test=(class(fit)[1]=="lm"),trans)
  {
    Estimate <- crossprod(lambda,coef(fit))
    summ <- summary(fit)
    SE <- sqrt(crossprod(lambda,vcov(fit))%*%lambda)
    z <- Estimate/SE
    if (t.test) p <- 2*pt(abs(z),fit$df.resid,low=FALSE)
    else p <- 2*pnorm(abs(z),low=FALSE)
    val <- c(Estimate,SE,z,p)
    if (t.test) names(val) <- c("Estimate","SE","t","p")
    else names(val) <- c("Estimate","SE","z","p")
    if (ci)
      {
        if (t.test) hw <- qt(1-alpha/2,fit$df.resid)*SE
        else hw <- qnorm(1-alpha/2)*SE
        lower <- Estimate - hw
        upper <- Estimate + hw
        val <- c(val,lower,upper)
        names(val)[5:6] <- c("Lower","Upper")
      }
    if (!missing(trans))
      {
        val[1] <- trans(val[1])
        val[2] <- NA
        if (ci)
          {
            val[5] <- trans(val[5])
            val[6] <- trans(val[6])
          }
      }
    return(val)
  }
