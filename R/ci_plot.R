#' Create forest plot of confidence intervals
#'
#' "Forest plot"-style plotting of confidence intervals from a regression model. Basic input is a matrix with columns of estimate/lower/upper, along with an optional 4th column for the p-value. Also works with a variety of models (lm/glm/coxph/etc).
#'
#' TO-DO:
#' * stripes (https://ggplot2-book.org/annotations#sec-custom-annotations)
#' * auto sd
#' * interactions
#'
#' @param obj    The object to be plotted; can be a matrix of raw values or a model object
#' @param ...    Not used
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
#' ci_plot(fit, tau=c(Solar.R = 100, Temp = 10, Wind = 3))
#' ci_plot(fit, tau=c(Solar.R = 100))
#' @export

ci_plot <- function(obj, ...) UseMethod("ci_plot")

#' @rdname ci_plot
#'
#' @param sort     Sort parameters by estimate? (default: true)
#' @param diff     Include tests of difference / p-values?
#' @param null     Draw a line representing no effect at this value (default: 0)
#' @param trans    Transformation to be applied (e.g., `trans=exp` for a log link)
#' @param p_label  Label p-values (p=0.02 instead of just 0.02)? (default: FALSE)
#'
#' @export

ci_plot.matrix <- function(
    obj, sort=TRUE, diff=(ncol(obj)==4), null=0, trans, p_label=FALSE, ...) {
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
  g <- ggplot2::ggplot(df, ggplot2::aes(.data$Estimate, .data$name)) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(ggplot2::aes(x=.data$Lower, xend=.data$Upper, y=.data$name, yend=.data$name)) +
    ggplot2::ylab('')

  # Add null line
  if (diff) g <- g + ggplot2::geom_vline(xintercept=null, lty=2, col='gray70')

  # Add p-value
  if (ncol(obj) > 3) {
    rng <- range(obj[,2:3]) |> diff()
    end <- max(obj[,2:3])
    plab <- formatP(df$p, label = p_label)
    g <- g + ggplot2::annotate("text", end + 0.1*rng, df$name, label=plab, hjust='inward', size=3.25)
  }

  g
}

#' @rdname ci_plot
#' @param intercept  Include a CI for the intercept? (default: FALSE)
#' @param exclude    Variables to exclude (character vector)
#' @param plot       If FALSE, just returns the matrix of estimates/CIs/p-values to be plotted but doesn't plot anything
#' @param tau        A named vector of effect sizes; CIs will be shown for tau*beta. Any coefficients not included are given tau = 1.
#' @export

ci_plot.lm <- function(obj, intercept=FALSE, exclude=NULL, plot=TRUE, tau, ...) {
  fit <- obj
  p <- length(coef(fit))
  j <- if (intercept) 1:p else 2:p
  B <- cbind(coef(fit)[j], confint(fit, j), summary(fit)$coef[j,4])
  if (!missing(tau)) B <- scale_ci(B, tau)
  colnames(B) <- c("Estimate","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),,drop=FALSE]
  if (plot) {
    ci_plot(B, ...)
  } else {
    invisible(B)
  }
}

#' @rdname ci_plot
#' @export
ci_plot.glm <- function(obj,...) ci_plot.lm(obj,...)

#' @rdname ci_plot
#' @param nsim  Number of simulations; see `confint.merMod`
#' @export
ci_plot.mer <- function(obj, intercept=FALSE, exclude=NULL, plot=TRUE, tau, nsim=500, ...) {
  fit <- obj
  p <- length(fit@fixef)
  j <- if (intercept) 1:p else 2:p
  B <- cbind(fit@fixef[j], confint(fit, j, nsim=nsim))
  if (!missing(tau)) B <- scale_ci(B, tau)
  colnames(B) <- c("Estimate","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) {
    ci_plot(B, ...)
  } else {
    invisible(B)
  }
}

#' @rdname ci_plot
#' @export
ci_plot.coxph <- function(obj, exclude=NULL, plot=TRUE, tau, ...) {
  fit <- obj
  p <- length(coef(fit))
  j <- 1:p
  if (missing(tau)) tau <- 1
  B <- cbind(coef(fit)[j], confint(fit, j), summary(fit)$coef[j,5])
  if (!missing(tau)) B <- scale_ci(B, tau)
  colnames(B) <- c("Estimate","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) {
    ci_plot(B, ...)
  } else {
    invisible(B)
  }
}

#' @rdname ci_plot
#' @export
ci_plot.data.frame <- function(obj, ...) {
  ci_plot.matrix(as.matrix(obj), ...)
}

scale_ci <- function(B, tau) {
  if (!all(names(tau) %in% rownames(B))) stop('tau must be a named list with names that correspond to coefficients in the model', call. = FALSE)
  ord <- match(names(tau), rownames(B))
  B[ord, 1:3] <- B[ord,1:3] * tau
  B
}
