#' Nagelkerke R2
#'
#' @param fit Output of [glm()]
#'
#' @examples
#' fit <- glm(am ~ hp + wt, data = mtcars, family = binomial)
#' r2(fit)
#' @export

r2 <- function(fit) {
  n <- length(fit$y)
  cs <- 1 - exp(-(fit$null.deviance - fit$deviance) / n)
  l0 <- 1 - exp(-fit$null.deviance / n)
  ng <- cs / l0
  c(cs=cs, ng=ng)
}
