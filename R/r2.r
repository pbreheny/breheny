#' Nagelkerke R2
#'
#' @param fit Output of [glm()]
#'
#' @returns A named vector:
#' * **mf:** McFadden \eqn{R^2}
#' * **cs:** Cox-Snell \eqn{R^2}
#' * **ng:** Nagelkerke \eqn{R^2}
#'
#' @examples
#' fit <- glm(am ~ hp + wt, data = mtcars, family = binomial)
#' r2(fit)
#' @export

r2 <- function(fit) {
  mf <- 1 - fit$deviance/fit$null.deviance
  n <- length(fit$y)
  cs <- 1 - exp(-(fit$null.deviance - fit$deviance) / n)
  l0 <- 1 - exp(-fit$null.deviance / n)
  ng <- cs / l0
  c(mf=mf, cs=cs, ng=ng)
}
