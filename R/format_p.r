#' Format p-values nicely
#'
#' @param p       Vector of p-values to format
#' @param digits  Digits after decimal point to display
#' @param label   If `label=TRUE`, prints `p=0.01`; otherwise, just returns `0.01`
#'
#' @examples
#' format_p(0.000003434)
#' format_p(0.12)
#' format_p(0.12, label = TRUE)
#' format_p(c(0.12, 0.000003434)) |> cat(sep = "\n")
#' format_p(c(0.12, 0.000003434), label = TRUE) |> cat(sep = "\n")
#' @export

format_p <- function(p, digits = 2, label = FALSE) {
  val <- formatC(p, digits = digits, format = "f")
  for (d in -(digits:4)) val[p < 10^d] <- paste("<", formatC(10^d))
  if (any(startsWith(val, "<")) && !label) {
    for (i in seq_along(val)) {
      if (!startsWith(val[i], "<")) {
        val[i] <- paste(" ", val[i])
      }
    }
  }
  if (label) {
    val[p >= 10^(-digits)] <- paste("p =", val[p >= 10^(-digits)])
    val[p < 10^(-digits)] <- paste("p", val[p < 10^(-digits)])
  }
  val
}
