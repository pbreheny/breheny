% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rstring.r
\name{rstring}
\alias{rstring}
\title{Generate random string of characters}
\usage{
rstring(n = 1, length = 20, include.extra = FALSE, include.num = FALSE)
}
\arguments{
\item{n}{Number of random strings to generate}

\item{length}{Number of characters in each random string}

\item{include.extra}{Include symbols like !#^*?}

\item{include.num}{Include numbers?}
}
\description{
Generate random string of characters
}
\examples{
rstring(1)
rstring(5, length = 30, include.extra = TRUE)

}
