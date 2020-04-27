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
#' Plot(fit)
#' Plot(fit, summary=TRUE)
#' Plot(fit, summary=TRUE, pval=TRUE)
#' Plot(fit, pval=TRUE, digits=2)
#' Data <- kyphosis
#' Data$Start <- Data$Start + runif(nrow(Data))
#' fit <- rpart::rpart(Kyphosis ~ Age + Number + Start, data = Data)
#' Plot(fit)
#' Plot(fit, digits=10)
#'
#' @export

Plot <- function(obj, ...) UseMethod("Plot")

#' @param pval      Show p-values at branching nodes?  Default=FALSE
#' @param summary   Show summary statistics at terminal nodes?  Default is FALSE; plots are drawn instead.
#' @param digits    Number of digits to show in edges.  Default: 1.
#' @rdname Plot
#' @export
Plot.BinaryTree <- function(obj, pval=FALSE, summary=FALSE, digits=1, ...) {
  if (summary) plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits), terminal_panel=panelSummary)
  else plot(obj, ip_args=list(id=FALSE, pval=pval), tp_args=list(id=FALSE), ep_args=list(digits=digits))
}

#' @rdname Plot
#' @export
Plot.rpart <- function(obj, summary=FALSE, digits=1, ...) {
  fit <- partykit::as.party(obj)
  if (summary) plot(fit, ip_args=list(id=FALSE, pval=FALSE), tp_args=list(id=FALSE), ep_args=list(digits=digits), terminal_panel=panelSummary)
  else plot(fit, ip_args=list(id=FALSE, pval=FALSE), tp_args=list(id=FALSE), ep_args=list(digits=digits))
}

#' @param legend      Where to put the legend. Either 'top', 'right', or 'none'; default: 'top'
#' @param xlab,ylab   Axis labels.
#' @param col         Vector of colors corresponding to groups.
#' @rdname Plot
#' @export
Plot.survfit <- function(obj, legend=c("top", "right", "none"), xlab="Time", ylab="Survival", col, ...) {
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
  plot(obj, xlab=xlab, ylab=ylab, bty="n", las=1, col=col, lwd=3, ...)
  if (n > 0) {
    if (legend == "top") {
      toplegend(legend=labs, col=pal(n), lwd=3)
    } else if (legend == "right") {
      rightlegend(legend=labs, col=pal(n), lwd=3)
    }
  }
}
