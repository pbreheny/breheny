#' Exact probabilities for sum of n dice
#'
#' See https://math.stackexchange.com/questions/2290090/probability-that-the-sum-of-k-dice-is-n for derivation
#'
#' @param x   Target sum
#' @param n   Number of dice to be rolled
#' @param s   Number of sides per die
#'
#' @examples
#' ddice(13, 5, 6)
#' pdice(13, 5, 6)
#' @export

ddice <- function(x, n, s) {
  m <- floor((x - n) / s)
  k <- 0:m
  if (x < n) {
    num <- 0
  } else if (x > n * s) {
    num <- 0
  } else {
    num <- sum((-1)^k * choose(n, k) * choose(x - k * s - 1, n - 1))
  }
  den <- s^n
  structure(num / den, numerator = num, denominator = den, class = "ddice")
}

#' @rdname ddice
#'
#' @param lower.tail   logical: if TRUE (default), probabilities are P(X <= x); otherwise P(X >= x)
#'
#' @export

pdice <- function(x, n, s, lower.tail = TRUE) {
  den <- s^n
  num <- 0
  if (lower.tail) {
    if (x >= n) {
      for (i in n:x) {
        num <- num + attr(ddice(i, n, s), "numerator")
      }
    }
  } else {
    if (x <= n * s) {
      for (i in x:(n * s)) {
        num <- num + attr(ddice(i, n, s), "numerator")
      }
    }
  }
  structure(num / den, numerator = num, denominator = den, class = "ddice")
}

#' @export
print.ddice <- function(x, digits = 1, ...) {
  cat(attr(x, "numerator"), "/", attr(x, "denominator"), " (", round(100 * x, digits = digits), "%)\n", sep = "")
}
