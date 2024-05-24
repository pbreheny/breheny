#' Box plot but whiskers show quantiles instead of outliers
#'
#' @param df           A data frame
#' @param x            The continuous variable
#' @param g            The categorical (group) variable
#' @param probs        Quantiles at which to draw box and whiskers (default: `c(0.1, 0.25, 0.5, 0.75, 0.9)`)
#' @param orientation  Either `horizontal` (default) or `vertical`
#'
#' @examples
#' qbox(iris, Sepal.Length, Species)
#' qbox(iris, Sepal.Length, Species, orientation='vertical')
#' qbox(iris, Sepal.Length, Species, c(0, 0.25, 0.5, 0.75, 1))
#' @export

qbox <- function(df, x, g, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), orientation=c('horizontal', 'vertical')) {
  orientation <- match.arg(orientation)
  dat <- eval(substitute(as.data.table(df)[, .(
    min = quantile(x, probs[1]),
    low = quantile(x, probs[2]),
    mid = quantile(x, probs[3]),
    hi  = quantile(x, probs[4]),
    max = quantile(x, probs[5])),
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
