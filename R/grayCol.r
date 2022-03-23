#' Take a color and make it duller
#'
#' @param col        Color
#' @param grayness   A value between 0 and 1; 0 does nothing, 1 is completely gray
#' @param darkness   A value between 0 and 1, indicating the darkness of the gray color to add (0 = white, 1 = black); default 0.25
#'
#' @examples
#' grayCol('red', 0.5)
#' plot(1:10, pch=19, col='red')
#' points(2:10, pch=19, col=grayCol('red', 0.5))
#' points(3:10, pch=19, col=grayCol('red', 0.75))
#' points(4:10, pch=19, col=grayCol('red', 0.75, 0.1))
#' points(5:10, pch=19, col=grayCol('red', 0.75, 0.5))
#'
#' grayCol(c('blue', 'red', 'green'), c(0.75, 0, 0.75), darkness=0.1)
#' plot(1:3, pch=19, col=.Last.value)
#'
#' @export

grayCol <- function(col, grayness, darkness=0.25) {

  # Argument checking
  nn <- c(length(col), length(grayness), length(darkness))
  n <- max(nn)
  if (any(nn > 1 & nn < n)) stop('Arguments to grayCol() must be length 1 or same length', call.=FALSE)
  col <- rep_len(col, n)
  grayness <- rep_len(grayness, n)
  darkness <- rep_len(darkness, n)

  # Calculate new color(s)
  x <- t(col2rgb(col))
  y <- t(col2rgb(gray(1-darkness)))
  z <- (1-grayness)*x + grayness*y
  rgb(z, maxColorValue=255)
}
