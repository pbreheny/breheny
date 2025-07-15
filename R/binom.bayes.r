#' Bayesian analysis of one-sample binomial data using conjugate beta priors
#'
#' @param x       Number of "successes"
#' @param n       Number of trials
#' @param a       'alpha' parameter for beta prior. Default: uniform prior
#' @param b       'beta' parameter for beta prior. Default: uniform prior
#' @param level   For posterior interval; .95 for a 95% credible interval
#' @param null    Point null to evaluate as a kind of hypothesis test
#' @param plot    Draw a plot?  Default: false
#' @param add     Add to existing plot?  Default: false
#' @param xlab    xlab for plot
#' @param ylab    ylab for plot
#' @param col     color of density line for plot
#' @param ...     Additional arguments to `plot()`
#'
#' @examples
#' binom_bayes(10, 16)
#' binom_bayes(0, 21, plot = TRUE)
#' @export

binom_bayes <- function(
    x, n, a = 1, b = 1, level = .95, null, plot = FALSE, add = FALSE,
    xlab = "p", ylab = "Posterior density", col = "blue", ...) {
  A <- a + x
  B <- n - x + b
  l_p <- qbeta((1 - level) / 2, A, B)
  u_p <- qbeta((1 + level) / 2, A, B)
  sample <- x / n
  mean_post <- A / (A + B)
  var_post <- (a + x) * (b + n - x) / ((a + b + n)^2 * (a + b + n + 1))
  ci_central <- c(l_p, u_p)
  f_central <- function(L) {
    l <- qbeta(L, a + x, n - x + b)
    u <- qbeta(L + level, a + x, n - x + b)
    u - l
  }
  if (A > 1 && B > 1) {
    mode_post <- (A - 1) / (A + B - 2)
    L <- optimize(f_central, lower = 0, upper = 1 - level, tol = 1e-10)$minimum
    l_h <- qbeta(L, a + x, n - x + b)
    if (l_h < 1e-8) l_h <- 0
    u_h <- qbeta(L + level, a + x, n - x + b)
    if (u_h > 1 - 1e-8) u_h <- 1
  } else if (A > B) {
    mode_post <- 1
    l_h <- qbeta(1 - level, A, B)
    u_h <- 1
  } else {
    mode_post <- 0
    l_h <- 0
    u_h <- qbeta(level, A, B)
  }
  ci_hpd <- c(l_h, u_h)
  if (!missing(null)) {
    d0 <- dbeta(null, A, B)
    f_hpd <- function(p) dbeta(p, A, B) - d0
    if (null > mode_post) {
      u <- null
      l <- uniroot(f_hpd, c(0, mode_post))$root
    } else {
      l <- null
      u <- uniroot(f_hpd, c(mode_post, 1))$root
    }
    p <- pbeta(l, A, B) + 1 - pbeta(u, A, B)
  }
  if (plot || add) {
    xx <- seq(0, 1, len = 399)
    if (add) {
      lines(xx, dbeta(xx, a + x, n - x + b), lwd = 3, col = col, ...)
    } else {
      plot(xx, dbeta(xx, A, B), col = col, type = "l", lwd = 3, xlab = xlab, ylab = ylab, las = 1, ...)
    }
  }
  val <- structure(list(
    sample = sample, mean.post = mean_post, mode_post = mode_post, var.post = var_post,
    ci_central = ci_central, ci_hpd = ci_hpd, level = level
  ), class = "onepar.bayes")
  if (!missing(null)) val$p <- p
  val
}

#' @export
print.onepar.bayes <- function(x, ...) {
  cat("Sample proportion:", formatC(x$sample, digits = 3, format = "f"), "\n")
  cat("Posterior mean:", formatC(x$mean, digits = 3, format = "f"), "\n")
  cat("Posterior mode:", formatC(x$mode, digits = 3, format = "f"), "\n")
  cat("Posterior SD:", formatC(sqrt(x$var), digits = 3, format = "f"), "\n")
  sprintf(
    "%1$d%% central interval: (%2$.3f, %3$.3f)\n",
    100 * x$level, x$ci_central[1], x$ci_central[2]
  ) |> cat()
  sprintf(
    "%1$d%% HPD interval: (%2$.3f, %3$.3f)\n",
    100 * x$level, x$ci_hpd[1], x$ci_hpd[2]
  ) |> cat()
  if ("p" %in% names(x)) cat("Significance: ", format_p(x$p), "\n", sep = "")
}
