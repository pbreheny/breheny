#' Setup errors and warnings to use cli coloring
#'
#' @param err    Try to colorize errors? I think this works OK, so default=`TRUE`.
#' @param warn   Try to colorize warnings? This sometimes screws up the terminal, so set to `FALSE` by default.
#'
#' @export

colorize_terminal <- function(err = TRUE, warn = FALSE) {
  if (!suppressMessages({
    requireNamespace("cli")
  })) {
    stop("cli package is not installed", call. = FALSE)
  }
  options(prompt = cli::combine_ansi_styles("cyan", "bold")("> "))
  if (err) {
    options(error = quote({
      cat(cli::combine_ansi_styles("red", "bold")(geterrmessage()))
      if (!interactive()) q(status = 1)
    }), show.error.messages = FALSE)
  }
  if (warn) {
    globalCallingHandlers(
      warning = function(cnd) {
        cnd$message <- cli::col_magenta(conditionMessage(cnd))
        warning(cnd)
        tryInvokeRestart("muffleWarning")
      }
    )
  }
}
