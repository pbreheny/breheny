#' Create forest plot of confidence intervals
#'
#' "Forest plot"-style plotting of confidence intervals from a regression model. Basic input is a matrix with columns of estimate/lower/upper, along with an optional 4th column for the p-value. Also works with a variety of models (lm/glm/coxph/etc).
#'
#' NEEDS TAU AND PROPER RHL FOR PLABEL
#'
#' @param obj   The object to be plotted; can be a matrix of raw values or a model object
#' @param ...   Not used
#'
#' @examples
#' # Supplying a matrix
#' B <- cbind(1:3, 0:2, 2:4)
#' rownames(B) <- LETTERS[1:3]
#' ci_plot(B)
#'
#' # Supplying a fitted model object
#' fit <- lm(Ozone ~ Solar.R + Wind + Temp, airquality)
#' ci_plot(fit)
#' @export

ci_plot <- function(obj, ...) UseMethod("ci_plot")

#' @rdname ci_plot
#'
#' @param sort                Sort parameters by estimate? (default: true)
#' @param diff                Include tests of difference / p-values?
#' @param null                Draw a line representing no effect at this value (default: 0)
#' @param p_label             Label p-values (p=0.02 instead of just 0.02)? (default: FALSE)
#'
#' @export

ci_plot.matrix <- function(
    obj, sort=TRUE, diff=(ncol(obj)==4), null=0, trans, p_label=FALSE) {
  # Set up data frame
  if (!missing(trans)) obj[,1:3] <- trans(obj[,1:3])
  colnames(obj)[1:3] <- c('Estimate', 'Lower', 'Upper')
  if (ncol(obj) > 3) colnames(obj)[4] <- 'p'
  df <- data.frame(name=rownames(obj), as.data.frame(obj))

  # Sort
  if (sort) {
    df$name <- factor(df$name) |>
      reorder(df$Estimate)
  } else {
    df$name <- factor(df$name, levels = rev(df$name))
  }

  # Plot
  g <- ggplot2::ggplot(df, ggplot2::aes(Estimate, name)) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(ggplot2::aes(x=Lower, xend=Upper, y=name, yend=name)) +
    ggplot2::ylab('')

  # Add null line
  if (diff) g <- g + ggplot2::geom_vline(xintercept=null, lty=2, col='gray70')

  # Add p-value
  if (ncol(obj) > 3) {
    plab <- formatP(df$p, label = p_label)
    g <- g + ggplot2::annotate("text", 4, df$name, label=plab)
  }

  g
}

#' @export

ci_plot.lm <- function(obj, intercept=FALSE, exclude=NULL, plot=TRUE, tau, ...) {
  fit <- obj
  p <- length(coef(fit))
  j <- if (intercept) 1:p else 2:p
  if (missing(tau)) tau <- 1
  B <- cbind(tau*coef(fit)[j],
             tau*confint(fit,j),
             summary(fit)$coef[j,4])
  colnames(B) <- c("Estimate","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),,drop=FALSE]
  if (plot) {
    ci_plot(B, ...)
  } else {
    invisible(B)
  }
}

#' @export

ci_plot.glm <- function(obj,...) ci_plot.lm(obj,...)

#' @export

ci_plot.mer <- function(obj, intercept=FALSE, exclude=NULL, plot=TRUE, tau, n.sim=10000, ...) {
  fit <- obj
  p <- length(fit@fixef)
  j <- if (intercept) 1:p else 2:p
  B <- cbind(fit@fixef[j], confint(fit, j, n.sim=n.sim))
  if (!missing(tau)) B[,1:3] <- B[,1:3]*tau
  colnames(B) <- c("Estimate","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) {
    ci_plot(B, ...)
  } else {
    invisible(B)
  }
}

#' @export

ci_plot.coxph <- function(obj, exclude=NULL, plot=TRUE, tau, ...) {
  fit <- obj
  p <- length(coef(fit))
  j <- 1:p
  if (missing(tau)) tau <- 1
  B <- cbind(tau*coef(fit)[j],
             tau*confint(fit,j),
             summary(fit)$coef[j,5])
  colnames(B) <- c("Estimate","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) {
    ci_plot(B, ...)
  } else {
    invisible(B)
  }
}

#' @export

ci_plot.data.frame <- function(obj, ...) {
  ci_plot.matrix(as.matrix(obj), ...)
}
