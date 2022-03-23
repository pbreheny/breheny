#' Estimate linear combinations from a regression model
#'
#' @param fit      A model with `coef()` and `vcov()` methods
#' @param lambda   Linear combination weights (numeric vector with length equal to number of coefficients in fit)
#' @param alpha    Error rate for confidence interval (default: 0.05)
#' @param t.test   Use t distribution for inference? (default: TRUE only if model is linear)
#' @param trans    Apply a transformation function to the results? (function)
#'
#' @examples
#' # Linear regression
#' fit <- lm(Ozone ~ Wind + Temp + Solar.R, airquality)
#' estimate(fit, c(0, 1, -1, 5))
#'
#' # Logistic regression
#' DF <- as.data.frame(Titanic)
#' DF <- DF[rep(1:nrow(DF), DF$Freq),]
#' fit <- glm(Survived ~ Class + Sex + Age, DF, family=binomial)
#' estimate(fit, c(0, 1, -1, 0, 0, 0), trans=exp)
#' @export

estimate <- function(fit, lambda, alpha=0.05, t.test=inherits(fit, "lm"), trans) {
  if (length(lambda) != length(coef(fit))) stop(paste("lambda must be of length", length(coef(fit)), "(the number of coefficients in the model)"), call.=FALSE)

  # Calculations
  Estimate <- crossprod(lambda, coef(fit))
  summ <- summary(fit)
  SE <- sqrt(crossprod(lambda, vcov(fit)) %*% lambda)
  z <- Estimate/SE
  if (t.test) p <- 2*pt(abs(z),fit$df.resid, lower.tail=FALSE)
  else p <- 2*pnorm(abs(z), lower.tail=FALSE)
  if (t.test) hw <- qt(1-alpha/2,fit$df.resid)*SE
  else hw <- qnorm(1-alpha/2)*SE
  lower <- Estimate - hw
  upper <- Estimate + hw

  # Format output
  val <- c(Estimate, lower, upper, SE, z, p)
  names(val) <- c("Estimate", "Lower", "Upper", "SE","z","p")
  if (t.test) names(val)[5] <- "t"
  if (!missing(trans)) {
    val[1:3] <- trans(val[1:3])
    val[4] <- NA
  }
return(val)
}
