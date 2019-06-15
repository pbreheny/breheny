#' Customized plotting functions
#'
#' @param obj   Object to be plotted
#' @param ...   Additional arguments to underlying plot function
#'
#' @examples
#' # KM curves
#' data(veteran, package='survival')
#' fit <- survival::survfit(survival::Surv(time, status) ~ celltype, veteran)
#' Plot(fit)
#'
#' # Trees
#' data(kyphosis, package='rpart')
#' fit <- rpart::rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
#' Plot(fit)
#' fit <- party::ctree(Kyphosis ~ Age + Number + Start, data = kyphosis)
#'
#' @export

Plot <- function(obj, ...) UseMethod("Plot")

#' @export
Plot.BinaryTree <- function(obj, pval=FALSE, summary=FALSE, digits=1, ...) {
  if (summary) plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits), terminal_panel=panelSummary)
  else plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits))
}

#' @export
Plot.rpart <- function(obj, ...) {
  fit <- partykit::as.party(obj)
  plot(fit, ip_args=list(id=FALSE), tp_args=list(id=FALSE), ...)
}

#' @export
Plot.survfit <- function(obj, legend=c("top", "right", "none"), xlab="Time", ylab="Survival", conf.int=FALSE, col, ...) {
  legend <- match.arg(legend)
  if ("pstate" %in% names(obj)) {
    n <- ncol(obj$pstate)-1
    labs <- names(obj$p0)[1:n]
  } else {
    n <- length(obj$strata)
    labs <- gsub('.*=', '', names(obj$strata))
  }
  if (missing(col)) {
    if (n == 0) {
      col <- pal(2)[2]
    } else {
      col <- pal(n)
    }
  }
  plot(obj, xlab=xlab, ylab=ylab, bty="n", las=1, col=col, lwd=3, conf.int=conf.int, ...)
  if (n > 0) {
    if (legend == "top") {
      toplegend(legend=labs, col=pal(n), lwd=3)
    } else if (legend == "right") {
      rightlegend(legend=labs, col=pal(n), lwd=3)
    }
  }
}
