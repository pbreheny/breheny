PCAplot <- function(X, grp, txt=FALSE, xlab="PCA 1", ylab="PCA 2", ...) {
  # Remove constant columns
  const <- which(apply(X, 2, sd)==0)
  if (length(const)) X <- X[,-const]

  # Do PCA
  PCA <- prcomp(X, scale=TRUE)
  P <- predict(PCA)

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
    plot(P[,1], P[,2], pch=16, col=col, las=1, bty='n', xlab=xlab, ylab=ylab, )
  } else {
    plot(P[,1], P[,2], pch=19)
  }
  if (!missing(grp)) {
    toplegend(legend=levels(Grp), pch=16, col=pal(length(levels(Grp))))
  }
}
