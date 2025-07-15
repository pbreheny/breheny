poisson.bayes <- function(y, t = 1, a = 1 / 2, b = 0, level = .95, plot = FALSE, add = FALSE, xlab = expression(lambda), ylab = "Posterior density", col = "blue", xlim = pmax(0, mean.post + c(-4, 4) * sqrt(var.post)), ...) {
  Y <- sum(y)
  N <- t * length(y)
  l.p <- qgamma((1 - level) / 2, a + Y, N + b)
  u.p <- qgamma((1 + level) / 2, a + Y, N + b)
  sample <- Y / N
  mean.post <- (a + Y) / (N + b)
  mode.post <- (a + Y - 1) / (N + b)
  var.post <- (a + Y) / (N + b)^2
  ci.central <- c(l.p, u.p)
  f <- function(l) {
    p <- min(1, pgamma(l, a + Y, N + b) + level)
    y <- qgamma(p, a + Y, N + b)
    dgamma(y, a + Y, N + b) - dgamma(l, a + Y, N + b)
  }
  l.h <- uniroot(f, c(0, qgamma(1 - level, a + Y, N + b)))$root
  if (l.h < 2 * .Machine$double.eps) l.h <- 0
  u.h <- qgamma(pgamma(l.h, a + Y, N + b) + level, a + Y, N + b)
  ci.hpd <- c(l.h, u.h)
  if (plot | add) {
    if (add) {
      xx <- seq(par("usr")[1], par("usr")[2], len = 399)
      lines(xx, dgamma(xx, a + Y, N + b), lwd = 3, col = col, ...)
    } else {
      xx <- seq(xlim[1], xlim[2], len = 399)
      plot(xx, dgamma(xx, a + Y, N + b), col = col, type = "l", lwd = 3, xlab = xlab, ylab = ylab, las = 1, ...)
    }
  }
  structure(list(sample = sample, mean.post = mean.post, mode.post = mode.post, var.post = var.post, ci.central = ci.central, ci.hpd = ci.hpd, level = level), class = "onepar.bayes")
}
