# p1 is probability of a change
# p2 is conditional probability of going up, given that change occurred
mcnpower <- function(n, p1, p2, alpha = 0.05) {
  nn <- 0:n
  p.nn <- dbinom(nn, n, p1)
  CV <- qbinom(1 - alpha / 2, size = nn, prob = 0.5)
  pow <- 1 - pbinom(CV, size = nn, prob = p2)
  sum(p.nn * pow)
}
