if (interactive()) library(tinytest)

# Single binary predictor agrees with two-sample t-test
d <- 0.5
n <- 128
df <- n - 2
ncp <- d * sqrt(n / 4)
tc <- qt(0.975, df)
pow_t <- pt(tc, df, ncp, lower.tail = FALSE) + pt(-tc, df, ncp)
rpower(d^2 / (d^2 + 4), n) |> expect_equal(pow_t)

# Sample size is the smallest n achieving desired power
n <- rsamsize(0.05, p = 10)
expect_true(rpower(0.05, n, p = 10) >= 0.8)
expect_true(rpower(0.05, n - 1, p = 10) < 0.8)

# rpower_beta round trip
rpower_beta(beta = rpower_beta(0.1)) |> expect_equal(0.1)
