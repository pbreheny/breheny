panelSummary <- function (ctreeobj, col = "black", fill = NULL, beside = NULL, ymax = NULL, ylines = NULL, widths = 1, gap = NULL, reverse = NULL, id = TRUE) 
{
  rval <- function(node) {
    p <- node$prediction[2]
    n <- sum(node$weights)
    f <- n*p
    
    top_vp <- viewport(name = paste("node_barplot", node$nodeID, sep = ""))
    pushViewport(top_vp)
    grid.text(paste(round(100*p),"%\n(",f,"/",n,")\n\n\n", sep=""))
    popViewport()
    ##upViewport(2)
  }
  return(rval)
}
class(panelSummary) <- "grapcon_generator"
