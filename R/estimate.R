estimate <- function(lambda, fit, alpha=.05, t.test=(class(fit)[1]=="lm"), trans) {
  ## Calculations
  Estimate <- crossprod(lambda,coef(fit))
  summ <- summary(fit)
  SE <- sqrt(crossprod(lambda,vcov(fit))%*%lambda)
  z <- Estimate/SE
  if (t.test) p <- 2*pt(abs(z),fit$df.resid,low=FALSE)
  else p <- 2*pnorm(abs(z),low=FALSE)
  if (t.test) hw <- qt(1-alpha/2,fit$df.resid)*SE
  else hw <- qnorm(1-alpha/2)*SE
  lower <- Estimate - hw
  upper <- Estimate + hw

  ## Format output
  val <- c(Estimate, lower, upper, SE, z, p)
  names(val) <- c("Estimate", "Lower", "Upper", "SE","z","p")
  if (t.test) names(val)[5] <- "t"
  if (!missing(trans)) {
    val[1:3] <- trans(val[1:3])
    val[4] <- NA
  }
return(val)
}
