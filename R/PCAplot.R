#' Make a PCA/tSNE plot
#'
#' @param X            A numeric matrix
#' @param grp          Optional grouping factor to color instances by
#' @param txt          Use rownames as labels instead of dots (default: false)
#' @param tsne         Use tSNE instead of PCA?  (default: false)
#' @param perp         Perplexity (for tSNE)
#' @param legend       Include a legend (default: true)
#' @param dims         Dimensions to return (for tsne)
#' @param ...          Further arguments to `plot()`
#'
#' @examples
#' PCAplot(iris[,1:4])
#' PCAplot(iris[,1:4], grp=iris[,5])
#' PCAplot(iris[,1:4], grp=iris[,5], col='gray')
#' PCAplot(iris[,1:4], grp=iris[,5], tsne=TRUE)
#' PCAplot(USArrests, txt=TRUE)
#'
#' @export

PCAplot <- function(X, grp, txt=FALSE, tsne=FALSE, perp=30, dims=2, legend, ...) {

  if (missing(legend)) legend <- !missing(grp)

  # Remove constant columns
  const <- which(apply(X, 2, sd)==0)
  if (length(const)) X <- X[,-const]

  # Do PCA
  if (tsne) {
    P <- Rtsne::Rtsne(X, pca_scale=TRUE, perplexity=perp, theta=0, check_duplicates = FALSE, dims=dims)$Y
    xlab <- "Dim 1"
    ylab <- "Dim 2"
  } else {
    PCA <- prcomp(X, scale=TRUE)
    P <- predict(PCA)
    pct <- round(prop.table(PCA$sdev^2) * 100, 1)
    xlab <- paste0("PCA 1 (", pct[1], "%)")
    ylab <- paste0("PCA 2 (", pct[2], "%)")
  }

  # Set up arguments
  if (!missing(grp)) {
    Grp <- as.factor(grp)
    col <- pal(length(levels(Grp)))[Grp]
  } else {
    col <- "black"
  }
  plot.args <- list(x=P[,1], y=P[,2], xlab=xlab, ylab=ylab, bty="n", las=1, pch=16, col=col)
  new.args = list(...)
  if (length(new.args)) plot.args[names(new.args)] = new.args

  if (txt) {
    plot.args$type <- "n"
    do.call("plot", plot.args)
    text(P[,1], P[,2], rownames(X), col=col)
  } else {
    do.call("plot", plot.args)
  }
  if (legend) {
    toplegend(legend=levels(Grp), pch=16, col=pal(length(levels(Grp))))
  }
  return(invisible(P))
}
