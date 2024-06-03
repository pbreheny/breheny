#' Construct confidence intervals in the presence of interactions
#'
#' @param fit    A model fit, as produced by [lm()], [glm()], [survival::coxph()], etc.
#' @param term   Term of interest
#' @param ...    Term to condition on; ideally a vector. See examples.
#'
#' @returns A `data.frame` suitable for use in [ci_plot()]
#'
#' @seealso [ci_plot()]
#'
#' @examples
#' fit <- lm(Ozone ~ Solar.R + Wind * Temp, airquality)
#' ci_int(fit, Wind, Temp=c(70, 80, 90))
#' ci_int(fit, Wind, Temp=c(70, 80, 90)) |> ci_plot()
#' airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool","Mild","Hot"))
#' fit <- lm(Ozone ~ Solar.R + Wind * Temp, airquality)
#' ci_int(fit, Wind, Temp) |> ci_plot()
#' @export

ci_int <- function(fit, term, ...) {
  # Process arguments
  m <- match.call()
  cnd <- tryCatch(
    list(str = names(list(...))[1], val = list(...)[[1]]),
    error = function(e) list(str = as.character(m[4]), val = NULL))
  x <- deparse(substitute(term))

  # Set up cnd vector if not user-specified
  if (is.null(cnd$val)) {
    z <- model.frame(fit)[,cnd$str]
    if (length(unique(z)) <= 5) {
      cnd$val <- unique(z)
    } else {
      cnd$val <- quantile(z, probs = c(0.1, 0.5, 0.9))
    }
  }

  # Set up return object
  out <- data.frame(
    Estimate = rep(NA, length(cnd$val)),
    Lower = rep(NA, length(cnd$val)),
    Upper = rep(NA, length(cnd$val)),
    p = rep(NA, length(cnd$val)),
    row.names=paste0(x, ' (', cnd$str, ' = ', cnd$val, ')'))

  # Recenter and update
  for (i in seq_along(cnd$val)) {
    dat <- model.frame(fit)
    dat[,cnd$str] <- dat[,cnd$str] - cnd$val[i]
    new_fit <- update(fit, data=dat)
    s <- summary(new_fit)$coefficients
    out$Estimate[i] <- coef(new_fit)[x]
    out[i, 2:3] <- confint(new_fit)[x,]
    out[i, 4] <- s[x, ncol(s)]
  }

  out
}
