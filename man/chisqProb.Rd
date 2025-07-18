% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chisqProb.r
\name{chisqProb}
\alias{chisqProb}
\title{Draw a chi-squared curve and shade area to the right (probability)}
\usage{
chisqProb(a, right = TRUE, df = 1, ...)
}
\arguments{
\item{a}{Point in question}

\item{right}{Shade the area to the right?  Default is TRUE; otherwise area to the left is shaded}

\item{df}{Degrees of freedom; default: 1}

\item{...}{Additional arguments to \code{plot()}}
}
\description{
Draw a chi-squared curve and shade area to the right (probability)
}
\examples{
chisqProb(2)
chisqProb(2, right = FALSE)
}
