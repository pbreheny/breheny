% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tpower.r
\name{tpower}
\alias{tpower}
\alias{tsamsize}
\title{Power for t tests, including arbitrary linear hypotheses}
\usage{
tpower(
  n,
  delta,
  lam = c(1, -1),
  b,
  sd = 1,
  alpha = 0.05,
  w = rep(1, g),
  n1,
  n2,
  verbose = (N == 1)
)

tsamsize(delta, b = c(delta, 0), w = rep(1, g), power = 0.8, upper = 5000, ...)
}
\arguments{
\item{n}{Sample size (total).}

\item{delta}{Effect size; if specified, a two-group design is assumed.  If more than two groups, use \code{b} instead.}

\item{lam}{Contrast.}

\item{b}{If a vector, the coefficient for each group.  If a matrix, must have \code{length(n)} rows.}

\item{sd}{Standard deviation of outcome.}

\item{alpha}{Type I error rate.}

\item{w}{Weights for unequal allocation (normalized to 1).}

\item{n1, n2}{Sample size for group 1, 2.}

\item{verbose}{Print details for power calculations.  Default: TRUE unless vectorized.}

\item{power}{Desired power.}

\item{upper}{Upper bound for \code{tsamsize()}; increase if tsamsize hits this bound.  Default: 5000.}

\item{...}{For \code{tsamsize()}, additional arguments to be passed to \code{tpower()}.}
}
\description{
Power for t tests, including arbitrary linear hypotheses
}
\examples{
tpower(100, 0.5)
tpower(10 * (6:9), 0.5) # Vectorize sample size
tpower(100, seq(0.25, 1, by = 0.25)) # Vectorize effect size
tpower(100, 0.5, alpha = seq(0.01, 0.05, 0.01)) # Vectorize alpha
tpower(99, b = c(0.5, 0.5, 0), lam = c(1, 1, -1)) # A multi-group example
tsamsize(0.5)
tsamsize(lam = c(-1, 1, 0, 0), b = c(0, 1, 1, 1))
}
