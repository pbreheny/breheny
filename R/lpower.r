# Power functions for logistic regression (in progress)
#
# Calculate logistic regression r2 for a given beta
b2r <- function(b, type='cs') {
  n <- 10000
  x <- rep_len(c(-1, 1), n)
  pi <- binomial()$linkinv(x * b)
  y <- rbinom(n = n, size = 1, prob = pi)
  fit <- glm(y ~ x, family = binomial)
  r2(fit)[type]
}

# Root-find: logistic regression beta for a given r2
r2b <- function(r) {
  uniroot(function(x) b2r(x) - r, c(1e-4, 2))$root
}

# Logistic regression power
l_pwr <- function(n, b, N=1000) {
  x <- rep_len(c(-1, 1), n)
  pi <- binomial()$linkinv(x * b)
  p <- numeric(N)
  for (i in 1:N) {
    y <- rbinom(n = n, size = 1, prob = pi)
    smr <- glm(y ~ x, family=binomial) |> summary()
    p[i] <- smr$coefficients[2,4]
  }
  mean(p < 0.05)
}
