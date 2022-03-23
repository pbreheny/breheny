#' Format p-values nicely
#'
#' @param p       Vector of p-values to format
#' @param digits  Digits after decimal point to display
#' @param label   If `label=TRUE`, prints `p=0.01`; otherwise, just returns `0.01`
#'
#' @examples
#' formatP(0.000003434)
#' formatP(0.12)
#' formatP(0.12, label=TRUE)
#'
#' @export

formatP <- function(p,digits=2,label=FALSE) {
  val <- formatC(p,digits=digits,format="f")
  for (d in -(digits:4)) val[p < 10^d] <- paste("<",formatC(10^d))
  if (any(p < .01, na.rm=TRUE) & !label) val[substr(val,1,2)=="0."] <- paste("  ",val[substr(val,1,2)=="0."])
  if (label) {
    val[p >= 10^(-digits)] <- paste("p =",val[p >= 10^(-digits)])
    val[p < 10^(-digits)] <- paste("p",val[p < 10^(-digits)])
  }
  return(val)
}
