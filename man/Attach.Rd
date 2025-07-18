% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Attach.r
\name{Attach}
\alias{Attach}
\title{Detach before attaching}
\usage{
Attach(x, name = deparse(substitute(x)), ...)
}
\arguments{
\item{x}{What to attach.}

\item{name}{Name to use for attached database.  Default: name of \code{x}.}

\item{...}{Additional arguments to \code{attach()}.}
}
\description{
Detach before attaching
}
\examples{
Attach(airquality)
Attach(airquality) # Ordinarily this would provoke warnings

}
