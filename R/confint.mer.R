confint.mer <- function(object, parm, level = 0.95, n.sim = 10000, ...) {
  method <- match.arg(method)
  varnames <- names(object@fixef)[parm]
  alpha <- 1-level
  if (missing(parm)) parm <- 1:length(object@fixef)
  
  summ <- lme4::summary(object)
  df <- summ@ngrps
  CI <- cbind(summ@coefs[parm,1] - qt(1-alpha/2, df)*summ@coefs[parm,2],
              summ@coefs[parm,1] + qt(1-alpha/2, df)*summ@coefs[parm,2])
  pval <- 2*(1-pt(abs(summ@coefs[parm,3]), df))

  rownames(CI) <- varnames
  cbind(CI,pval)
}
