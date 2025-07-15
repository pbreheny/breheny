#' Make a PCA/tSNE/UMAP plot
#'
#' The function allows the passing of some parameters to Rtsne/umap:
#'   * Rtsne: perplexity
#'   * umap: nn (number of neighbors)
#'
#' @param X            A numeric matrix
#' @param grp          Optional grouping factor to color instances by
#' @param txt          Use rownames as labels instead of dots (default: false)
#' @param method       How to carry out dimension reduction (pca/tsne/umap)
#' @param dims         Dimensions to return (for tsne)
#' @param gg           Use ggplot2? (default: true)
#' @param ellipse      Draw mvn-ellipses around groups? (default: false, only available for ggplot)
#' @param legend       Include a legend (default: true)
#' @param repel        If `gg=TRUE` and `txt=TRUE`, use `ggrepel::geom_text_repel()`? (default: false)
#' @param plot         Create plot? (default: true)
#' @param ...          Further arguments to `plot()`
#'
#' @return If base plot, returns PCA object invisibly. If ggplot, returns the plot object.
#'
#' @examples
#' # Plots of the iris data
#' pca_plot(iris[, 1:4])
#' pca_plot(iris[, 1:4], grp = iris[, 5])
#' pca_plot(iris[, 1:4], grp = iris[, 5], ellipse = TRUE)
#' pca_plot(iris[, 1:4], grp = iris[, 5], method = "tsne")
#' pca_plot(iris[, 1:4], grp = iris[, 5], method = "umap")
#'
#' # What pca_plot returns if plot=FALSE
#' head(pca_plot(iris[, 1:4], method = "pca", plot = FALSE))
#' head(pca_plot(iris[, 1:4], method = "tsne", dims = 3, plot = FALSE))
#' head(pca_plot(iris[, 1:4], method = "umap", dims = 3, plot = FALSE))
#'
#' # Plots of the USArrests data
#' pca_plot(USArrests, txt = TRUE)
#' pca_plot(USArrests, txt = TRUE, repel = TRUE)
#' pca_plot(USArrests, txt = TRUE, grp = rep(LETTERS[1:5], each = 10))
#' pca_plot(USArrests, txt = TRUE, method = "tsne", perplexity = 10)
#' pca_plot(USArrests, txt = TRUE, method = "umap", nn = 6)
#' @export

pca_plot <- function(X, grp, txt = FALSE, method = c("pca", "tsne", "umap"), dims = 2,
                     gg = TRUE, ellipse = FALSE, legend, repel = FALSE, plot = TRUE, ...) {
  if (missing(legend)) legend <- !missing(grp)
  method <- match.arg(method)
  dots <- list(...)

  # Remove constant columns
  const <- which(apply(X, 2, sd) == 0)
  if (length(const)) X <- X[, -const]

  # Do PCA
  if (method == "tsne") {
    if ("perplexity" %in% names(dots)) {
      perp <- dots$perplexity
      dots$perplexity <- NULL
    } else {
      perp <- 30
    }
    P <- Rtsne::Rtsne(X, pca_scale = TRUE, perplexity = perp, theta = 0, check_duplicates = FALSE, dims = dims)$Y
    colnames(P) <- paste0("D", 1:ncol(P))
    xlab <- "Dim 1"
    ylab <- "Dim 2"
  } else if (method == "umap") {
    settings <- umap::umap.defaults
    settings$n_components <- dims
    if ("nn" %in% names(dots)) {
      settings$n_neighbors <- dots$nn
      dots$nn <- NULL
    }
    P <- umap::umap(X, settings)$layout
    colnames(P) <- paste0("D", 1:ncol(P))
    xlab <- "Dim 1"
    ylab <- "Dim 2"
  } else {
    PCA <- prcomp(X, scale = TRUE)
    P <- predict(PCA)
    pct <- prop.table(PCA$sdev^2) * 100
    xlab <- sprintf("PCA 1 (%.1f%%)", pct[1])
    ylab <- sprintf("PCA 2 (%.1f%%)", pct[2])
  }
  if (!is.null(rownames(X))) rownames(P) <- rownames(X)
  for (j in 1:ncol(P)) {
    if (range(P[, j]) |> abs() |> diff() < 0) P[, j] <- -P[, j]
  }
  attr(P, "xlab") <- xlab
  attr(P, "ylab") <- ylab
  if (!plot) {
    return(P)
  }

  # Plot
  if (gg) {
    colnames(P) <- paste0("V", 1:ncol(P))
    DF <- cbind(as.data.frame(P))
    if (missing(grp)) {
      if (txt) {
        DF <- cbind(DF, label = rownames(X))
        p <- ggplot2::ggplot(DF, ggplot2::aes(.data$V1, .data$V2, label = .data$label))
        if (repel) {
          p <- p + ggrepel::geom_text_repel() + ggplot2::geom_point()
        } else {
          p <- p + ggplot2::geom_text()
        }
      } else {
        p <- ggplot2::ggplot(DF, ggplot2::aes(.data$V1, .data$V2)) +
          ggplot2::geom_point()
      }
    } else {
      DF <- cbind(DF, group = grp)
      if (txt) {
        DF <- cbind(DF, label = rownames(X))
        p <- ggplot2::ggplot(DF, ggplot2::aes(.data$V1, .data$V2, color = .data$group, label = .data$label)) +
          ggplot2::theme(legend.title = ggplot2::element_blank())
        if (repel) {
          p <- p + ggrepel::geom_text_repel() + ggplot2::geom_point()
        } else {
          p <- p + ggplot2::geom_text()
        }
      } else {
        p <- ggplot2::ggplot(DF, ggplot2::aes(.data$V1, .data$V2, color = .data$group)) +
          ggplot2::geom_point() +
          ggplot2::theme(legend.title = ggplot2::element_blank())
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
    plot.args <- list(x = P[, 1], y = P[, 2], xlab = xlab, ylab = ylab, bty = "n", las = 1, pch = 16, col = col)
    new.args <- list(...)
    if (length(new.args)) plot.args[names(new.args)] <- new.args

    if (txt) {
      plot.args$type <- "n"
      do.call("plot", plot.args)
      text(P[, 1], P[, 2], rownames(X), col = col)
    } else {
      do.call("plot", plot.args)
    }
    if (legend) {
      toplegend(legend = levels(Grp), pch = 16, col = pal(length(levels(Grp))))
    }
    return(invisible(P))
  }
}
