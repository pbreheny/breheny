#' Split a file into yaml and body
#'
#' @param file   Filename (string)
#'
#' @examples
#' process_yaml(system.file("extdata/example.rmd", package = "breheny"))
#' @export

process_yaml <- function(file) {
  raw <- readLines(file)
  yl <- stringr::str_which(raw, "^---$")
  ybeg <- yl[1] + 1
  yend <- yl[2] - 1
  bbeg <- yl[2] + 1
  yaml_string <- paste(raw[ybeg:yend], collapse = "\n")
  list(
    yaml = yaml::yaml.load(yaml_string),
    body = raw[-1:-bbeg]
  )
}
