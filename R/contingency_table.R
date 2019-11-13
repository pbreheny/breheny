#' Format a contingency table for use with knitr
#'
#' @param Tab      Number of random strings to generate
#' @param digits   Number of characters in each random string
#'
#' @examples
#' Tab <- table(A=rnorm(100)>0, B=rnorm(100)>0)
#' contingency_table(Tab)
#'
#' @export

contingency_table <- function(Tab, digits=2) {
  dimnames(Tab) <- lapply(dimnames(Tab), gsub, pattern='FALSE', replacement='No')
  dimnames(Tab) <- lapply(dimnames(Tab), gsub, pattern='TRUE', replacement='Yes')
  dimnames(Tab)[[1]] <- paste(names(dimnames(Tab))[1], dimnames(Tab)[[1]], sep=': ')
  dimnames(Tab)[[2]] <- paste(names(dimnames(Tab))[2], dimnames(Tab)[[2]], sep=': ')
  kableExtra::kable_styling(knitr::kable(Tab, 'html', digits=digits), full_width=FALSE)
}
