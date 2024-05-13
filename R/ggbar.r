#' Quick wrapper for a gg bar plot
#'
#' @param x   A named vector
#'
#' @returns A gg object
#'
#' @examples
#' x <- rexp(10)
#' names(x) <- letters[1:10] |> sample()
#' ggbar(x)
#' @export

ggbar <- function(x) {
  data.frame(label = factor(names(x), levels=names(x)), value=x) |>
    ggplot2::ggplot(ggplot2::aes(value, label)) +
    ggplot2::geom_col() +
    ggplot2::ylab('')
}
