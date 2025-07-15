#' Create a time-to-event chart using ggplot
#'
#' Inspired by `Hmisc::event.chart()`
#'
#' @param Data    A data frame or data.table of dates, organized in the following way (the first two columns are plotted with a line segment, the remaining columns are plotted with a symbol):
#' * Column 1: Follow-up start date
#' * Column 2: Follow-up end date
#' * Additional columns: Additional events that may occur during the follow-up period
#' @param shift   Shift time so that 0 represents start of follow-up for everyone? (default: FALSE)
#' @param scale   If shifted, what units? (default: scale=1, meaning days)
#'
#' @examples
#' df <- read.csv(system.file("extdata/cdcaids.txt", package = "breheny"))
#' event_chart(df, FALSE)
#' event_chart(df, TRUE)
#' event_chart(df, TRUE, 365.25)
#' @export

event_chart <- function(Data, shift = FALSE, scale = 1) {
  # Convert to data.table and split
  if (!is.data.table(Data)) {
    DT <- data.table(Data)
  } else {
    DT <- copy(Data)
  }

  # Check that all columns are dates or can be converted to dates
  for (j in 1:ncol(DT)) if (!inherits(DT[[j]], "Date")) DT[[j]] <- as.Date(DT[[j]])

  # Plot
  v <- names(DT)
  if (shift) {
    DT[, (v) := lapply(.SD, function(x) as.numeric(x - DT[[v[1]]]) / scale)]
    DT$y <- rank(DT[[v[2]]], ties.method = "first")
    X <- DT[, c("y", v[1:2]), with = FALSE]
    names(X) <- c("y", "x1", "x2")
    Y <- melt(DT[, c("y", v[-(1:2)]), with = FALSE], id.vars = "y")
    p <- ggplot2::ggplot(Y, ggplot2::aes(.data$value, .data$y)) +
      ggplot2::geom_point(ggplot2::aes(shape = .data$variable), na.rm = TRUE) +
      ggplot2::geom_segment(data = X, ggplot2::aes(x = .data$x1, xend = .data$x2, y = .data$y, yend = .data$y)) +
      ggplot2::xlab("Time on study") +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      )
  } else {
    DT$y <- rank(DT[[v[1]]], ties.method = "first")
    X <- DT[, c("y", v[1:2]), with = FALSE]
    names(X) <- c("y", "x1", "x2")
    Y <- melt(DT[, c("y", v[-(1:2)]), with = FALSE], id.vars = "y")
    p <- ggplot2::ggplot(Y, ggplot2::aes(.data$value, .data$y)) +
      ggplot2::geom_point(ggplot2::aes(shape = .data$variable), na.rm = TRUE) +
      ggplot2::geom_segment(data = X, ggplot2::aes(x = .data$x1, xend = .data$x2, y = .data$y, yend = .data$y)) +
      ggplot2::xlab("Date") +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank()
      )
  }
  p
}
