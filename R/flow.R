#' Create a flowchart
#'
#' `Flow` is an R6 class used for initializing, constructing and modifying the
#' information that goes into the flowchart.  The plotting is then handled via the
#' `plot()` function.
#'
#' @name flowchart
#' @aliases flow
#' @aliases Flow
#'
#' @examples
#' fl <- Flow$new("All children\n(n=935)")
#' fl$add("Returned form\n(n=511)", parent=1)
#' fl$add("Baseline: 0\n(n=447)", parent=2, x=-1)
#' fl$add("Baseline: 1 or 2\n(n=14)", parent=2)
#' fl$add("Baseline: 3\n(n=50)", parent=2, x=1)
#' fl$add("Initiation\n(n=315)", parent=3, x=-1)
#' fl$add("Completion\n(n=276)", parent=6, x=-1)
#' fl$add("Completion\n(n=12)", parent=4)
#' fl
#' plot(fl)
#' plot(fl, hpad=20, vpad=20)
#' plot(fl, xm=0.25)
NULL

#' @param x     Horizontal position (0=center)
#' @param lab   Label (node text)
#'
#' @export
#' @rdname flowchart

Flow <- R6::R6Class('Flow', public=list(
  #' @description Initialize a new flowchart at its parent node
  initialize = function(lab, x=0) {
    private$val <- data.frame(Label=lab, Parent=0, x=x)
  },

  #' @description Print the current status of the flowchart
  print = function() {
    print.data.frame(private$val)
    cat("Class: Flow")
  },

  #' @description Add a node to the flowchart
  #' @param parent   Number (i.e., row) of this node's parent
  add = function(lab, parent, x=0) {
    new <- data.frame(Label=lab, Parent=parent, x=x)
    private$val <- rbind(private$val, new)
  }
), active = list(
  #' @field n        Number of nodes
  #' @field Parent   Vector of parent nodes
  #' @field x        Vector of horizontal positions
  #' @field Label    Vector of labels
  n = function() nrow(private$val),
  Parent = function() private$val$Parent,
  x = function() private$val$x,
  Label = function() private$val$Label
), private=list(
  val = NULL
))

#' @param x         `Flow` object
#' @param xm,ym       Outer x/y margins of the plot (.2 = 20% margin on each side); default: 0.1
#' @param hpad,vpad   Horzontal/verticl padding around text in chart nodes, in mm; default: 10
#' @param ...         For S3 method compatibility
#'
#' @rdname flowchart
#' @export

plot.Flow <- function(x, xm=.1, ym=.1, hpad=10, vpad=10, ...) {
  obj <- x
  depth <- numeric(obj$n)
  for (i in 1:obj$n) {
    if (obj$Parent[i]==0) {
      depth[i] <- 1
    } else {
      depth[i] <- depth[obj$Parent[i]] + 1
    }
  }
  y <- seq(1, 0, length=max(depth))[depth]
  x <- (obj$x - min(obj$x)) / diff(range(obj$x))
  gt <- grid::gTree()
  for (i in 1:obj$n) {
    gt <- grid::addGrob(
      gt, grid::grob(lab=obj$Label[i], x=x[i], y=y[i], hpad=hpad, vpad=vpad, cl="box", name=as.character(i)))
    if (obj$Parent[i] != 0) {
      parent <- grid::getGrob(gt, as.character(obj$Parent[i]))
      child <- grid::getGrob(gt, as.character(i))
      xdiff <- x[i] - x[obj$Parent[i]]
      if (xdiff == 0) {
        inflect <- FALSE
        curv <- 0
      } else {
        inflect <- TRUE
        curv <- 2*(xdiff>0)-1
      }
      gt <- grid::addGrob(
        gt, grid::curveGrob(grid::grobX(parent, "south"),
                            grid::grobY(parent, "south"),
                            grid::grobX(child, "north"),
                            grid::grobY(child, "north"),
                            inflect=inflect, curv=curv, gp=grid::gpar(fill="black"),
                            arrow=grid::arrow(type="closed", angle=15, length=grid::unit(3, 'mm'))))
    }
  }
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(x=0.5, y=0.5, w=1-2*xm, h=1-2*ym))
  grid::grid.draw(gt)
}

#' @export
#' @method drawDetails box
drawDetails.box <- function(x, ...) {
  vp <- grid::viewport(x=x$x, y=x$y, width=grid::stringWidth(x$lab) + grid::unit(x$hpad, "mm"), height=grid::stringHeight(x$lab) + grid::unit(x$vpad, "mm"))
  grid::pushViewport(vp)
  grid::grid.roundrect(gp=grid::gpar(fill="gray90", lwd=0))
  grid::grid.text(x$lab)
  grid::popViewport()
}

#' @export
#' @method xDetails box
xDetails.box <- function(x, theta) {
  height <- grid::stringHeight(x$lab) + grid::unit(x$vpad, "mm")
  width <- grid::unit(x$hpad, "mm") + grid::stringWidth(x$lab)
  grid::grobX(grid::roundrectGrob(x=x$x, y=x$y, width=width, height=height), theta)
}

#' @export
#' @method yDetails box
yDetails.box <- function(x, theta) {
  height <- grid::stringHeight(x$lab) + grid::unit(x$vpad, "mm")
  width <- grid::unit(x$hpad, "mm") + grid::stringWidth(x$lab)
  grid::grobY(grid::rectGrob(x=x$x, y=x$y, width=width, height=height), theta)
}
