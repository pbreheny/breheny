#' Box plot but whiskers show quantiles instead of outliers
#'
#' @param df           A data frame
#' @param x            The continuous variable
#' @param g            The categorical (group) variable
#' @param orientation  Either `horizontal` (default) or `vertical`
#'
#' @examples
#' qbox(iris, Sepal.Length, Species)
#' qbox(iris, Sepal.Length, Species, orientation='vertical')
#' @export

qbox <- function(df, x, g, orientation=c('horizontal', 'vertical')) {
  orientation <- match.arg(orientation)
  dat <- eval(substitute(as.data.table(df)[, .(
    min = quantile(x, 0.1),
    low = quantile(x, 0.25),
    mid = quantile(x, 0.5),
    hi  = quantile(x, 0.75),
    max = quantile(x, 0.9)),
    g]))
  if (orientation == 'horizontal') {
      ggplot2::ggplot(dat, ggplot2::aes(y={{g}}, fill={{g}})) +
      ggplot2::geom_boxplot(ggplot2::aes(xmin=.data$min, xlower=.data$low, xmiddle=.data$mid, xupper=.data$hi, xmax=.data$max), stat = "identity") +
      ggplot2::guides(fill = 'none') +
      ggplot2::xlab(as.character(substitute(x)))
  } else {
    ggplot2::ggplot(dat, ggplot2::aes(x = {{g}}, fill = {{g}})) +
      ggplot2::geom_boxplot(ggplot2::aes(ymin=.data$min, lower=.data$low, middle=.data$mid, upper=.data$hi, ymax=.data$max), stat = "identity") +
      ggplot2::guides(fill = 'none') +
      ggplot2::ylab(as.character(substitute(x)))
  }
}
