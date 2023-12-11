#' Density plot
#'
#' Like `dnplot()`, but using ggplot2.
#'
#' @param M                               A vector or matrix of values upon which the density plot is based; if a matrix, separate densities will be estimated and plotted for each column
#' @param labs                            If supplied, a legend is printed; order of `labs` should correspond to columns of `M`
#'
#' @examples
#' M <- cbind(rgamma(1000, 2, 2), rnorm(1000))
#' colnames(M) <- c('Gamma', 'Normal')
#' density_plot(M[,1])
#' density_plot(M)
#' density_plot(rexp(100, 2), labs='Exponential')
#' density_plot(data.frame(Gamma=rgamma(1000, 2, 2), Normal=rnorm(1000)))
#' @export

density_plot <- function(M, labs) {
  # Create df
  no_legend <- FALSE
  if (is.vector(M)) {
    df <- data.frame(variable='a', value=M)
    no_legend <- TRUE
  } else if (is.matrix(M)) {
    df <- as.data.frame.array(M) |>
      data.table::as.data.table() |>
      data.table::melt(measure.vars=1:ncol(M))
  } else {
    df <- data.table::as.data.table(M) |>
      data.table::melt(measure.vars=1:ncol(M))
  }

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(.data$value, group=.data$variable, color=.data$variable, fill=.data$variable)) +
    ggplot2::geom_density(alpha=0.5) +
    ggplot2::labs(color='', fill='')
  if (!missing(labs)) {
    no_legend <- FALSE
    p <- p + ggplot2::scale_fill_discrete(labels=labs) + ggplot2::scale_color_discrete(labels=labs)
  }
  if (no_legend) p <- p + ggplot2::theme(legend.position='none')
  p
}
