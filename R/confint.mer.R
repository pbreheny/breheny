confint.mer <- function(object, parm, level = 0.95, n.sim = 10000, method=c("simple", "mcmc"), ...)
{
  method <- match.arg(method)
  varnames <- names(object@fixef)[parm]
  alpha <- 1-level
  if (missing(parm)) parm <- 1:length(object@fixef)
  
  if (method=="mcmc") {
    samp <- lme4:::mcmcsamp(object, n.sim)
    est <- apply(samp@fixef, 1, mean)
    stderr <- apply(samp@fixef, 1, sd)
    CI <- t(sapply(parm, function(i) {quantile(samp@fixef[i,], probs = c(alpha/2, 1 - alpha/2))}))
    pval <- sapply(parm, function(i) {mean(samp@fixef[i,]>0)})
    pval <- ifelse(pval <= 0.5, 2 * pval, 2 * (1 - pval))    
  } else {
    summ <- summary(object)
    df <- summ@ngrps
    CI <- cbind(summ@coefs[parm,1] - qt(1-alpha/2, df)*summ@coefs[parm,2],
                summ@coefs[parm,1] + qt(1-alpha/2, df)*summ@coefs[parm,2])
    pval <- 2*(1-pt(abs(summ@coefs[parm,3]), df))
  }

  rownames(CI) <- varnames
  cbind(CI,pval)
}
