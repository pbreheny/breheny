% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddice.r
\name{ddice}
\alias{ddice}
\alias{pdice}
\title{Exact probabilities for sum of n dice}
\usage{
ddice(x, n, s)

pdice(x, n, s, lower.tail = TRUE)
}
\arguments{
\item{x}{Target sum}

\item{n}{Number of dice to be rolled}

\item{s}{Number of sides per die}

\item{lower.tail}{logical: if TRUE (default), probabilities are P(X <= x); otherwise P(X >= x)}
}
\description{
See https://math.stackexchange.com/questions/2290090/probability-that-the-sum-of-k-dice-is-n for derivation
}
\examples{
ddice(13, 5, 6)
pdice(13, 5, 6)
}
