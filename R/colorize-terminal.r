#' Setup errors and warnings to use cli coloring
#'
#' @export

colorize_terminal <- function() {
  if (!suppressMessages({
    requireNamespace("cli")})) stop("cli package is not installed", call.=FALSE)
  options(prompt=cli::combine_ansi_styles("cyan", "bold")("> "))
  options(error = quote({
    cat(cli::combine_ansi_styles("red", "bold")(geterrmessage()))
    if (!interactive()) q(status=1)
    }), show.error.messages=FALSE)
  globalCallingHandlers(
    warning = function(cnd) {
      cnd$message <- cli::col_magenta(conditionMessage(cnd))
      warning(cnd)
      tryInvokeRestart("muffleWarning")
    }
  )
}
