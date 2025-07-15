#' Save current base plot as pdf/png
#'
#' See `ggplot2::ggsave()` for saving grid plots.
#'
#' @param file   File path where the plot will be saved (string)
#' @param ...    Additional arguments to graphics device
#'
#' @examples
#' plot(1:10)
#' \dontrun{
#' # Typical journal options:
#' save_plot("test.png", width = 3.5, height = 3.5, units = "in", res = 600)
#' }
#' @export

save_plot <- function(file, ...) {
  ext <- tools::file_ext(file)
  dev_fun <- switch(ext,
    png = png,
    pdf = pdf,
    svg = svg
  )

  # Record the current plot
  p <- recordPlot()

  # Open the graphics device
  dev_fun(file, ...)

  # Replay the plot
  replayPlot(p)

  # Close the device
  dev.off()
}
