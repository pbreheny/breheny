% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCAplot.R
\name{PCAplot}
\alias{PCAplot}
\title{Make a PCA/tSNE plot}
\usage{
PCAplot(X, grp, txt = FALSE, tsne = FALSE, perp = 30, dims = 2, legend, ...)
}
\arguments{
\item{X}{A numeric matrix}

\item{grp}{Optional grouping factor to color instances by}

\item{txt}{Use rownames as labels instead of dots (default: false)}

\item{tsne}{Use tSNE instead of PCA?  (default: false)}

\item{perp}{Perplexity (for tSNE)}

\item{dims}{Dimensions to return (for tsne)}

\item{legend}{Include a legend (default: true)}

\item{...}{Further arguments to \code{plot()}}
}
\description{
Make a PCA/tSNE plot
}
\examples{
PCAplot(iris[,1:4])
PCAplot(iris[,1:4], grp=iris[,5])
PCAplot(iris[,1:4], grp=iris[,5], col='gray')
PCAplot(iris[,1:4], grp=iris[,5], tsne=TRUE)
PCAplot(USArrests, txt=TRUE)

}
