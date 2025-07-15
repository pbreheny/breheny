survrate <- function(fit, t) {
  val <- NULL
  for (i in 1:length(t))
  {
    ind <- length(which(fit$time <= t[i]))
    if (ind == 0) {
      val <- rbind(val, c(1, 1, 1))
    } else {
      val <- rbind(val, c(fit$surv[ind], fit$lower[ind], fit$upper[ind]))
    }
  }
  colnames(val) <- c("Estimate", "Lower", "Upper")
  return(val)
}
