#' Add a confidence interval band to a KM curve
#'
#' @param obj   A `survfit` or `survfitms` object
#' @param ...   Not used
#'
#' @examples
#' library(survival)
#'
#' fit <- survfit(Surv(time, status) ~ 1, veteran)
#' Plot(fit)
#' ciband(fit)
#'
#' fit <- survfit(Surv(time, status) ~ celltype, veteran)
#' Plot(fit)
#' ciband(fit)
#'
#' @export

ciband <- function(obj, ...) UseMethod("ciband")

#' @export
ciband.survfit <- function(obj, col, fun=as.numeric, ...) {
  K <- length(obj$strata)
  if (K==0) {
    s <- c(0, length(obj$time))
    if (missing(col)) col <- rgb(0.5, 0.5, 0.5, alpha=0.3)
    K <- 1
  } else {
    s <- c(0, cumsum(obj$strata))
    if (missing(col)) col <- pal(K, alpha=0.4)
  }
  for (i in 1:K) {
    ind1 <- (s[i]+1):s[i+1]
    ind2 <- (s[i]+1):(s[i+1]-1)
    x <- c(0, obj$time[ind1])
    l <- fun(c(1, obj$lower[ind2]))
    u <- fun(c(1, obj$upper[ind2]))
    polygon.step(x, l, u, col=col[i])
  }
}

#' @export
ciband.survfitms <- function(obj, col, fun=as.numeric, ...) {
  if(length(obj$strata)) stop('Not implemented for stratified multi-state models')
  K <- ncol(obj$pstate)
  if (missing(col)) col <- pal(K-1, alpha=0.4)
  for (i in 2:K) {
    x <- c(0, obj$time)
    l <- fun(obj$lower[,i])
    u <- fun(obj$upper[,i])
    polygon.step(x, l, u, col=col[i-1])
  }
}
