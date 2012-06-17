refresh <- function(package,character.only=FALSE)
  {
    if (!character.only) package <- as.character(substitute(package))
    loaded <- paste("package", package, sep = ":") %in% search()
    if (!loaded) require(package,character.only=TRUE)
    else
      {
        detach(pos = match(paste("package", package, sep=":"), search()),unload=TRUE)
        require(package,character.only=TRUE)
      }
  }
