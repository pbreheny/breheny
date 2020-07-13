#' Make a PCA/tSNE plot
#'
#' @param X            A numeric matrix
#' @param grp          Optional grouping factor to color instances by
#' @param txt          Use rownames as labels instead of dots (default: false)
#' @param tsne         Use tSNE instead of PCA?  (default: false)
#' @param perp         Perplexity (for tSNE)
#' @param xlab,ylab    Axis labels
#' @param legend       Include a legend (default: true)
#' @param dims         Dimensions to return (for tsne)
#' @param ...          Further arguments to `plot()`
#'
#' @examples
#' PCAplot(iris[,1:4])
#' PCAplot(iris[,1:4], grp=iris[,5])
#' PCAplot(iris[,1:4], grp=iris[,5], tsne=TRUE)
#' PCAplot(USArrests, txt=TRUE)
#'
#' @export

PCAplot <- function(X, grp, txt=FALSE, tsne=FALSE, perp=30, xlab="PCA 1", ylab="PCA 2", dims=2, legend, ...) {

  if (missing(legend)) legend <- !missing(grp)

  # Remove constant columns
  const <- which(apply(X, 2, sd)==0)
  if (length(const)) X <- X[,-const]

  # Do PCA
  if (tsne) {
    P <- Rtsne::Rtsne(X, pca_scale=TRUE, perplexity=perp, theta=0, check_duplicates = FALSE, dims=dims)$Y
  } else {
    PCA <- prcomp(X, scale=TRUE)
    P <- predict(PCA)
  }

  # Plot
  if (!missing(grp)) {
    Grp <- as.factor(grp)
    col <- pal(length(levels(Grp)))[Grp]
  } else {
    col <- "black"
  }
  if (txt) {
    plot(P[,1], P[,2], type="n", xlab=xlab, ylab=ylab, bty="n", las=1, ...)
    text(P[,1], P[,2], rownames(X), col=col)
  } else if (!missing(grp)) {
    plot(P[,1], P[,2], pch=16, col=col, las=1, bty='n', xlab=xlab, ylab=ylab, ...)
  } else {
    plot(P[,1], P[,2], pch=19, xlab=xlab, ylab=ylab, ...)
  }
  if (legend) {
    toplegend(legend=levels(Grp), pch=16, col=pal(length(levels(Grp))))
  }
  return(invisible(P))
}
