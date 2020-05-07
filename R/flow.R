#' Create a flowchart
#'
#' @name flowchart
#' @aliases flow
#'
#' @note Probably would be better with R6/RC object orientation
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
#' plot(fl, xm=.2)
NULL

#' @rdname flowchart
#'
#' @param obj         `flow` object; in addNode, a new object will be created if not supplied
#' @param node        Text for new node
#' @param parent      Parent of new node
#' @param edge        Edge text from parent node to this node; default is ''
#' @param theta,...   For S3 compatibility.
#'
#' @export

Flow <- R6::R6Class('Flow', public=list(
  val = NULL,
  initialize = function(lab, x=0) {
    self$val <- data.frame(Label=lab, Parent=0, x=x)
  },
  add = function(lab, parent, x=0) {
    new <- data.frame(Label=lab, Parent=parent, x=x)
    self$val <- rbind(self$val, new)
  },
  print = function() {
    print.data.frame(self$val)
    cat("Class: Flow")
  }
), active = list(
  n = function() nrow(self$val),
  Parent = function() self$val$Parent,
  x = function() self$val$x,
  Label = function() self$val$Label
))

#' @rdname flowchart
#' @export

drawDetails.box <- function(x, lab, hpad=20, vpad=20, ...) {
  vp <- grid::viewport(x=x$x, y=x$y, width=grid::stringWidth(x$lab) + grid::unit(hpad, "mm"), height=grid::stringHeight(x$lab) + grid::unit(vpad, "mm"))
  grid::pushViewport(vp)
  grid::grid.roundrect(gp=grid::gpar(fill="gray90", lwd=0))
  grid::grid.text(x$lab)
  grid::popViewport()
}

#' @rdname flowchart
#' @export

xDetails.box <- function(x, theta) {
  height <- grid::stringHeight(x$lab) + grid::unit(x$vpad, "mm")
  width <- grid::unit(x$hpad, "mm") + grid::stringWidth(x$lab)
  grid::grobX(grid::roundrectGrob(x=x$x, y=x$y, width=width, height=height), theta)
}

#' @rdname flowchart
#' @export

yDetails.box <- function(x, theta) {
  height <- grid::stringHeight(x$lab) + grid::unit(x$vpad, "mm")
  width <- grid::unit(x$hpad, "mm") + grid::stringWidth(x$lab)
  grid::grobY(grid::rectGrob(x=x$x, y=x$y, width=width, height=height), theta)
}

#' @rdname flowchart
#'
#' @param x    Flowchart object
#' @param y    Left/right positioning of flowchart elements
#' @param xm   x (horizontal) margin (.2 = 20% whitespace on left/right margins)
#' @param ym   y (vertical) margin (.2 = 20% whitespace on top/bottom margins)
#'
#' @export

plot.Flow <- function(obj, xm=.1, ym=.1, hpad=20, vpad=20, ...) {
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

  # for (i in 2:obj$n) {
  #   parent <- grid::getGrob(gt, as.character(obj$Parent[i]))
  #   child <- grid::getGrob(gt, as.character(i))
  #   xdiff <- x[i] - x[obj$Parent[i]]
  #   if (xdiff == 0) {
  #     inflect <- FALSE
  #     curv <- 0
  #   } else {
  #     inflect <- TRUE
  #     curv <- 2*(xdiff>0)-1
  #   }
  #   grid::grid.curve(grid::grobX(parent, "south"),
  #                    grid::grobY(parent, "south"),
  #                    grid::grobX(child, "north"),
  #                    grid::grobY(child, "north"),
  #                    inflect=inflect, curv=curv, gp=grid::gpar(fill="black"),
  #                    arrow=grid::arrow(type="closed", angle=15, length=grid::unit(3, 'mm')))
  # }
}

