corImage <- function(S, col.breaks=seq(-1,1,.1), names=FALSE) {
  n <- length(col.breaks)-1
  col <- c(colorRampPalette(c("Blue","White"))(n/2),colorRampPalette(c("White","Red"))(n/2))
  if (!names) colnames(S) <- rownames(S) <- NULL
  levelplot(S, col.regions=col, at=seq(-1,1,.1), xlab="", ylab="")
}
