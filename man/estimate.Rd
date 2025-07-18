% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.r
\name{estimate}
\alias{estimate}
\title{Estimate linear combinations from a regression model}
\usage{
estimate(fit, lambda, alpha = 0.05, t.test = inherits(fit, "lm"), trans)
}
\arguments{
\item{fit}{A model with \code{coef()} and \code{vcov()} methods}

\item{lambda}{Linear combination weights (numeric vector with length equal to number of coefficients in fit)}

\item{alpha}{Error rate for confidence interval (default: 0.05)}

\item{t.test}{Use t distribution for inference? (default: TRUE only if model is linear)}

\item{trans}{Apply a transformation function to the results? (function)}
}
\description{
Estimate linear combinations from a regression model
}
\examples{
# Linear regression
fit <- lm(Ozone ~ Wind + Temp + Solar.R, airquality)
estimate(fit, c(0, 1, -1, 5))

# Logistic regression
DF <- as.data.frame(Titanic)
DF <- DF[rep(1:nrow(DF), DF$Freq), ]
fit <- glm(Survived ~ Class + Sex + Age, DF, family = binomial)
estimate(fit, c(0, 1, -1, 0, 0, 0), trans = exp)
}
