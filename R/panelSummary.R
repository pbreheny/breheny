panelSummary <- function (ctreeobj, col = "black", fill = NULL, beside = NULL, ymax = NULL, ylines = NULL, widths = 1, gap = NULL, reverse = NULL, id = TRUE) {
  rval <- function(node) {
    p <- node$prediction[2]
    n <- sum(node$weights)
    f <- n*p

    top_vp <- grid::viewport(name = paste("node_barplot", node$nodeID, sep = ""))
    grid::pushViewport(top_vp)
    grid::grid.text(paste(round(100*p),"%\n(",f,"/",n,")\n\n\n", sep=""))
    grid::popViewport()
  }
  return(rval)
}
class(panelSummary) <- "grapcon_generator"
