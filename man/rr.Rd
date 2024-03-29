% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rr.r
\name{rr}
\alias{rr}
\title{Calculate relative risk from a 2x2 table}
\usage{
rr(x, margin)
}
\arguments{
\item{x}{Table}

\item{margin}{As in \code{prop.table()}}
}
\description{
Calculate relative risk from a 2x2 table
}
\examples{
m <- matrix(4:1, 2)
rr(m, 1)
rr(m, 2)

}
