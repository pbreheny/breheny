% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotl.r
\encoding{UTF-8}
\name{likelihood_plot}
\alias{likelihood_plot}
\alias{plotL}
\alias{plotl}
\title{Plot likelihood function}
\usage{
plotL(
  x,
  l,
  xlab = expression(theta),
  ylab = expression(L(theta)),
  bty = "n",
  col,
  add = FALSE,
  ...
)

plotl(x, l, bty = "n", add = FALSE, ...)
}
\arguments{
\item{x}{Parameter values (on x axis)}

\item{l}{Likelihood / log-likelihood (on y axis)}

\item{xlab}{Default: theta}

\item{ylab}{Default: L(theta)}

\item{bty}{Default: no boundary box}

\item{col}{Color (or vector of colors, if \code{l} is a matrix)}

\item{add}{Add to existing plot?  Default: FALSE}

\item{...}{Further arguments to \code{plot()}}
}
\description{
\code{plotL()} plots the likelihood, \code{plotl()} plots the log-likelihood
}
\examples{
x <- seq(-3, 4, length = 99)
L <- dnorm(1, x)
l <- dnorm(1, x, log = TRUE)
plotL(x, L)
plotl(x, l)
L <- cbind(dnorm(1, x), dnorm(2, x))
l <- cbind(dnorm(1, x, log = TRUE), dnorm(2, x, log = TRUE))
plotL(x, L)
plotl(x, l)
}
