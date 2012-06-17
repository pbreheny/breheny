summary.rlm <- function (object, method = c("XtX", "XtWX"), correlation = FALSE, 
    ...) 
{
    method <- match.arg(method)
    s <- object$s
    coef <- object$coefficients
    ptotal <- length(coef)
    wresid <- object$wresid
    res <- object$residuals
    n <- length(wresid)
    if (any(na <- is.na(coef))) 
        coef <- coef[!na]
    cnames <- names(coef)
    p <- length(coef)
    rinv <- diag(p)
    dimnames(rinv) <- list(cnames, cnames)
    wts <- if (length(object$weights)) 
        object$weights
    else rep(1, n)
    if (length(object$call$wt.method) && object$call$wt.method == 
        "case") {
        rdf <- sum(wts) - p
        w <- object$psi(wresid/s)
        S <- sum(wts * (wresid * w)^2)/rdf
        psiprime <- object$psi(wresid/s, deriv = 1)
        m1 <- sum(wts * psiprime)
        m2 <- sum(wts * psiprime^2)
        nn <- sum(wts)
        mn <- m1/nn
        kappa <- 1 + p * (m2 - m1^2/nn)/(nn - 1)/(nn * mn^2)
        stddev <- sqrt(S) * (kappa/mn)
    }
    else {
        res <- res * sqrt(wts)
        rdf <- n - p
        w <- object$psi(wresid/s)
        S <- sum((wresid * w)^2)/rdf
        psiprime <- object$psi(wresid/s, deriv = 1)
        mn <- mean(psiprime)
        kappa <- 1 + p * var(psiprime)/(n * mn^2)
        stddev <- sqrt(S) * (kappa/mn)
    }
    X <- if (length(object$weights)) 
        object$x * sqrt(object$weights)
    else object$x
    if (method == "XtWX") {
        mn <- sum(wts * w)/sum(wts)
        X <- X * sqrt(w/mn)
    }
    R <- qr(X)$qr
    R <- R[1L:p, 1L:p, drop = FALSE]
    R[lower.tri(R)] <- 0
    rinv <- solve(R, rinv)
    dimnames(rinv) <- list(cnames, cnames)
    rowlen <- (rinv^2 %*% rep(1, p))^0.5
    names(rowlen) <- cnames
    if (correlation) {
        correl <- rinv * array(1/rowlen, c(p, p))
        correl <- correl %*% t(correl)
    }
    else correl <- NULL
    coef <- array(coef, c(p, 4L))
    dimnames(coef) <- list(cnames, c("Value", "Std. Error", "t value", "Pr(>|t|)"))
    coef[, 2] <- rowlen %o% stddev
    coef[, 3] <- coef[, 1]/coef[, 2]
    coef[, 4] <- 2*pt(-abs(coef[, 1]/coef[, 2]),rdf)
    object <- object[c("call", "na.action")]
    object$residuals <- res
    object$sigma <- s
    object$stddev <- stddev
    object$df <- c(p, rdf, ptotal)
    object$r.squared <- NA
    object$cov.unscaled <- rinv %*% t(rinv)
    object$correlation <- correl
    object$terms <- NA
    object$coefficients <- coef
    class(object) <- "summary.rlm"
    object
}
print.summary.rlm <- function (x, digits = max(3, .Options$digits - 3), ...) 
{
    cat("\nCall: ")
    dput(x$call, control = NULL)
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    cat(if (!is.null(x$weights) && diff(range(x$weights))) 
        "Weighted ", "Residuals:\n", sep = "")
    if (rdf > 5L) {
        if (length(dim(resid)) == 2L) {
            rq <- apply(t(resid), 1L, quantile)
            dimnames(rq) <- list(c("Min", "1Q", "Median", "3Q", 
                "Max"), colnames(resid))
        }
        else {
            rq <- quantile(resid)
            names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
        }
        print(rq, digits = digits, ...)
    }
    else if (rdf > 0L) {
        print(resid, digits = digits, ...)
    }
    if (nsingular <- df[3L] - df[1L]) 
        cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
            sep = "")
    else cat("\nCoefficients:\n")
    ##coef <- x$coefficients
    ##coef[,1:3] <- round(coef[,1:3], digits = digits)
    ##coef[,4] <- format.pval(coef[,4])
    ##print(format(coef), quote = FALSE, ...)
    printCoefmat(x$coefficients,digits=digits)
    cat("\nResidual standard error:", format(signif(x$sigma, 
        digits)), "on", rdf, "degrees of freedom\n")
    if (nzchar(mess <- naprint(x$na.action))) 
        cat("  (", mess, ")\n", sep = "")
    if (!is.null(correl <- x$correlation)) {
        p <- dim(correl)[2L]
        if (p > 1L) {
            cat("\nCorrelation of Coefficients:\n")
            ll <- lower.tri(correl)
            correl[ll] <- format(round(correl[ll], digits))
            correl[!ll] <- ""
            print(correl[-1L, -p, drop = FALSE], quote = FALSE, 
                digits = digits, ...)
        }
    }
    invisible(x)
}

