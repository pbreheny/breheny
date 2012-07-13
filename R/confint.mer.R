confint.mer <- function(object, parm, level = 0.95, n.sim = 10000, ...)
{
  ## Sample
  if (missing(parm)) parm <- 1:length(object@fixef)
  varnames <- names(object@fixef)[parm]
  samp <- lme4:::mcmcsamp(object, n.sim)

  ## CI
  est <- apply(samp@fixef, 1, mean)
  stderr <- apply(samp@fixef, 1, sd)
  alpha <- 1-level
  CI <- t(sapply(parm, function(i) {quantile(samp@fixef[i,], probs = c(alpha/2, 1 - alpha/2))}))
  rownames(CI) <- varnames

  ## pval
  pval <- sapply(parm, function(i) {mean(samp@fixef[i,]>0)})
  pval <- ifelse(pval <= 0.5, 2 * pval, 2 * (1 - pval))
  cbind(CI,pval)
}
