#' Create a flowchart
#'
#' @name flowchart
#' @aliases flow
#'
#' @note Probably would be better with R6/RC object orientation
#'
#' @examples
#' X <- addNode("All children\n(n=935)")
#' addNode(X, "Returned form\n(n=511)", parent=1)
#' addNode(X, "Baseline: 0\n(n=447)", parent=2)
#' addNode(X, "Baseline: 1 or 2\n(n=14)", parent=2)
#' addNode(X, "Baseline: 3\n(n=50)", parent=2)
#' addNode(X, "Initiation\n(n=315)", parent=3)
#' addNode(X, "Completion\n(n=276)", parent=4)
#' addNode(X, "Completion\n(n=12)", parent=6)
#' plot(X, c(0, 0, -1, 0, 1, -1, 0, -1), xm=.2)
NULL

#' @rdname flowchart
#'
#' @param obj      `flow` object; in addNode, a new object will be created if not supplied
#' @param node     Text for new node
#' @param parent   Parent of new node
#' @param edge     Edge text from parent node to this node; default is ''
#'
#' @export

addNode <- function(obj, node, parent, edge="") {
  if (class(obj)[1] != "flow") {
    df.i <- data.frame(node=as.character(obj), parent=0, edge="", stringsAsFactors=FALSE)
    return(structure(df.i, class=c("flow", "data.frame")))
  }
  df.i <- data.frame(node=node, parent=parent, edge=edge, stringsAsFactors=FALSE)
  obj <- rbind(obj, df.i)
  assign(as.character(match.call()[2]), structure(obj, class=c("flow", "data.frame")), envir=.GlobalEnv)
}

drawBox <- function(lab, x=.5, y=.5, hpad=20, vpad=20) {
  vp <- grid::viewport(x=x, y=y, width=grid::stringWidth(lab) + grid::unit(hpad, "mm"), height=grid::stringHeight(lab) + grid::unit(vpad, "mm"))
  grid::pushViewport(vp)
  grid::grid.roundrect(gp=grid::gpar(fill="gray90", lwd=0))
  grid::grid.text(lab)
  grid::popViewport()
}

flowBox <- function(lab, x=.5, y=.5, hpad=20, vpad=20) {
  grid::grob(lab=lab, x=x, y=y, hpad=hpad, vpad=vpad, cl="box")
}

#' @rdname flowchart
#' @export

drawDetails.box <- function(x, ...) {
  drawBox(x$lab, x$x, x$y, hpad=x$hpad, vpad=x$vpad)
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
#' @param x    left/right positioning of flowchart elements
#' @param xm   x (horizontal) margin (.2 = 20\% whitespace on left/right margins)
#' @param ym   y (vertical) margin (.2 = 20\% whitespace on top/bottom margins)
#'
#' @export

plot.flow <- function(x, y, xm=.1, ym=.1, ...) {
  obj <- x; x <- y
  userX <- if (missing(x)) FALSE else TRUE
  n <- nrow(obj)
  depth <- numeric(n)
  for (i in 1:n) {
    if (obj$parent[i]==0) {
      depth[i] <- 1
    } else {
      depth[i] <- depth[obj$parent[i]] + 1
    }
  }
  yy <- seq(1-ym, ym, length=max(depth))
  y <- yy[depth]
  if (userX) {
    if (length(x) != nrow(obj)) stop("x doesn't match flow object")
    nx <- max(abs(x))
    xx <- seq(xm, 1-xm, length=nx)
    x <- 0.5 + x/nx*(0.5-xm)
  } else {
    x <- numeric(n)
    x[1] <- 0.5
  }
  box <- list(n)
  box[[1]] <- flowBox(obj$node[1], y=y[1])
  grid::grid.draw(box[[1]])
  for (i in 2:max(depth)) {
    nd <- table(depth)[i]
    if (!userX) xx <- if (nd==1) 0.5 else seq(xm, 1-xm, length=nd)
    counter <- 0
    for (j in which(depth==i)) {
      p <- obj$parent[j]
      counter <- counter + 1
      if (!userX) x[j] <- xx[counter]
      box[[j]] <- flowBox(obj$node[j], x=x[j], y=y[j])
      grid::grid.draw(box[[j]])
      if (abs(x[j]-x[p]) < 0.01) {
        inflect <- FALSE
        curv <- 1
      } else {
        inflect <- TRUE
        curv <- if (x[j] > x[p]) 1 else -1
      }
      grid::grid.curve(grid::grobX(box[[p]], "south"), grid::grobY(box[[p]], "south"),
                 grid::grobX(box[[j]], "north"), grid::grobY(box[[j]], "north"),
                 inflect=inflect, curv=curv,
                 arrow=grid::arrow(type="closed", angle=15, length=grid::unit(3, "mm")), gp=grid::gpar(fill="black"))
      if (obj$edge[j]!="") {
        just <- if (x[j] > x[p]) "right" else "left"
        grid::grid.text(obj$edge[j], x[j], (y[p]+y[j])/2, just=c(just,"bottom"))
      }
    }
  }
}

