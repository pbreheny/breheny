#' Make a PCA/tSNE plot
#'
#' @param X            A numeric matrix
#' @param grp          Optional grouping factor to color instances by
#' @param txt          Use rownames as labels instead of dots (default: false)
#' @param method       How to carry out dimension reduction (pca/tsne/umap)
#' @param perp         Perplexity (for tSNE)
#' @param dims         Dimensions to return (for tsne)
#' @param gg           Use ggplot2? (default: true)
#' @param ellipse      Draw mvn-ellipses around groups? (default: false, only available for ggplot)
#' @param legend       Include a legend (default: true)
#' @param plot         Create plot? (default: true)
#' @param ...          Further arguments to `plot()`
#'
#' @return If base plot, returns PCA object invisibly. If ggplot, returns the plot object.
#'
#' @examples
#' pca_plot(iris[,1:4])
#' pca_plot(iris[,1:4], grp=iris[,5])
#' pca_plot(iris[,1:4], grp=iris[,5], ellipse=TRUE)
#' pca_plot(iris[,1:4], grp=iris[,5], method='tsne')
#' pca_plot(iris[,1:4], grp=iris[,5], method='umap')
#' pca_plot(USArrests, txt=TRUE)
#' pca_plot(USArrests, txt=TRUE, grp=rep(LETTERS[1:5], each=10))
#' pca_plot(USArrests, txt=TRUE, method='tsne', perp=5)
#' pca_plot(USArrests, txt=TRUE, method='umap')
#' @export

pca_plot <- function(X, grp, txt=FALSE, method=c('pca', 'tsne', 'umap'), perp=30, dims=2, gg=TRUE, ellipse=FALSE, legend, plot=TRUE, ...) {

  if (missing(legend)) legend <- !missing(grp)
  method <- match.arg(method)

  # Remove constant columns
  const <- which(apply(X, 2, sd)==0)
  if (length(const)) X <- X[,-const]

  # Do PCA
  if (method=='tsne') {
    P <- Rtsne::Rtsne(X, pca_scale=TRUE, perplexity=perp, theta=0, check_duplicates = FALSE, dims=dims)$Y
    xlab <- "Dim 1"
    ylab <- "Dim 2"
  } else if (method=='umap') {
    P <- umap::umap(X)$layout
    xlab <- "Dim 1"
    ylab <- "Dim 2"
  } else {
    PCA <- prcomp(X, scale=TRUE)
    P <- predict(PCA)
    pct <- prop.table(PCA$sdev^2) * 100
    xlab <- sprintf("PCA 1 (%.1f%%)", pct[1])
    ylab <- sprintf("PCA 2 (%.1f%%)", pct[2])
  }
  if (!plot) return(P)

  # Set up arguments
  if (gg) {
    colnames(P) <- paste0('V', 1:ncol(P))
    DF <- cbind(as.data.frame(P))
    if (missing(grp)) {
      if (txt) {
        DF <- cbind(DF, label=rownames(X))
        p <- ggplot2::ggplot(DF, ggplot2::aes_string('V1', 'V2', label='label')) +
          ggplot2::geom_text()
      } else {
        p <- ggplot2::ggplot(DF, ggplot2::aes_string('V1', 'V2')) +
          ggplot2::geom_point()
      }
    } else {
      DF <- cbind(DF, group=grp)
      if (txt) {
        DF <- cbind(DF, label=rownames(X))
        p <- ggplot2::ggplot(DF, ggplot2::aes_string('V1', 'V2', color='group', label='label')) +
          ggplot2::geom_text() +
          ggplot2::theme(legend.title=ggplot2::element_blank())
      } else {
        p <- ggplot2::ggplot(DF, ggplot2::aes_string('V1', 'V2', color='group')) +
          ggplot2::geom_point() +
          ggplot2::theme(legend.title=ggplot2::element_blank())
      }
    }
    if (ellipse) p <- p + ggplot2::stat_ellipse()
    p <- p + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
    return(p)
  } else {
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
}
