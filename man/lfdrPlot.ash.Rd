% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lfdrPlot-ash.r
\name{lfdrPlot.ash}
\alias{lfdrPlot.ash}
\title{Make an lFDR plot for an ash object}
\usage{
lfdrPlot.ash(fit, n = 99, ...)
}
\arguments{
\item{fit}{Output from `ash()``}

\item{n}{Number of breaks for histogram}

\item{...}{Further arguments to \code{Hist()}}
}
\description{
Make an lFDR plot for an ash object
}
\examples{
library(ashr)
z <- c(rnorm(900), rnorm(200, sd=2))
fit <- ash(z, rep(1, length(z)), optmethod='cxxMixSquarem')
lfdrPlot.ash(fit)

}
