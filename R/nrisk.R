#' Add an axis describing number at risk to a KM curve
#'
#' @param fit   A `survfit` or `survfitms` object
#' @param cex   Character expansion parameter for axis labels, as in `par()`
#'
#' @examples
#' library(survival)
#'
#' fit <- survfit(Surv(time, status) ~ 1, veteran)
#' Plot(fit)
#' nrisk(fit)
#'
#' fit <- survfit(Surv(time, status) ~ celltype, veteran)
#' op <- par(mar=c(5,5,6,7))
#' Plot(fit, legend='right')
#' nrisk(fit)
#' par(op)
#'
#' @export

nrisk <- function(fit, cex=0.8) {
  usr <- par("usr")
  x <- axisTicks(usr[1:2], FALSE)
  xl <- tail(x, 1)
  x <- x[-length(x)]
  s <- summary(fit, x)
  if ('strata' %in% names(s)) {
    ns <- length(levels(s$strata))
    lab <- gsub(".*\\=", "", levels(s$strata))
    for (i in 1:ns) {
      ind <- s$strata == levels(s$strata)[i]
      axis(3, line=i-1, at=s$time[ind], labels=s$n.risk[ind], tick=FALSE, cex.axis=cex)
      axis(3, line=i-1, at=xl, labels=lab[i], tick=FALSE, xpd=TRUE, cex.axis=cex)
    }
  } else {
    axis(3, line=0, at=s$time, labels=s$n.risk, tick=FALSE, cex.axis=cex)
  }
}
