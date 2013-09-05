drawBox <- function(lab, x=.5, y=.5, hpad=20, vpad=20) {
  vp <- viewport(x=x, y=y, width=stringWidth(lab) + unit(hpad, "mm"), height=stringHeight(lab) + unit(vpad, "mm"))
  pushViewport(vp)
  grid.roundrect(gp=gpar(fill="gray90", lwd=0))
  grid.text(lab)
  popViewport()
}

flowBox <- function(lab, x=.5, y=.5, hpad=20, vpad=20) {
  grob(lab=lab, x=x, y=y, hpad=hpad, vpad=vpad, cl="box")
}

drawDetails.box <- function(x, ...) {
  drawBox(x$lab, x$x, x$y, hpad=x$hpad, vpad=x$vpad)
}

xDetails.box <- function(x, theta) {
  height <- stringHeight(x$lab) + unit(x$vpad, "mm")
  width <- unit(x$hpad, "mm") + stringWidth(x$lab)
  grobX(roundrectGrob(x=x$x, y=x$y, width=width, height=height), theta)
}

yDetails.box <- function(x, theta) {
  height <- stringHeight(x$lab) + unit(x$vpad, "mm")
  width <- unit(x$hpad, "mm") + stringWidth(x$lab)
  grobY(rectGrob(x=x$x, y=x$y, width=width, height=height), theta)
}

plot.flow <- function(N, x) {
  userX <- if (missing(x)) FALSE else TRUE
  n <- nrow(N)
  depth <- numeric(n)
  for (i in 1:n) {
    if (N$parent[i]==0) {
      depth[i] <- 1
    } else {
      depth[i] <- depth[N$parent[i]] + 1
    }
  }
  yy <- seq(0.9, 0.1, length=max(depth))
  y <- yy[depth]
  if (userX) {
    if (length(x) != nrow(X)) stop("x doesn't match X")
    nx <- max(abs(x))
    xx <- seq(0.1, 0.9, length=nx)
    x <- 0.5 + x/nx*(0.9-0.5)
  } else {
    x <- numeric(n)
    x[1] <- 0.5
  }
  box <- list(n)
  box[[1]] <- flowBox(N$node[1], y=y[1])
  grid.draw(box[[1]])
  for (i in 2:max(depth)) {
    nd <- table(depth)[i]
    if (!userX) xx <- if (nd==1) 0.5 else seq(0.1, 0.9, length=nd)
    counter <- 0
    for (j in which(depth==i)) {
      p <- N$parent[j]
      counter <- counter + 1
      if (!userX) x[j] <- xx[counter]
      box[[j]] <- flowBox(N$node[j], x=x[j], y=y[j])
      grid.draw(box[[j]])
      if (abs(x[j]-x[p]) < 0.01) {
        inflect <- FALSE
        curv <- 1
      } else {
        inflect <- TRUE
        curv <- if (x[j] > x[p]) 1 else -1
      }
      grid.curve(grobX(box[[p]], "south"), grobY(box[[p]], "south"),
                 grobX(box[[j]], "north"), grobY(box[[j]], "north"),
                 inflect=inflect, curv=curv,
                 arrow=arrow(type="closed", angle=15, length=unit(3, "mm")), gp=gpar(fill="black"))
      if (N$edge[j]!="") {
        just <- if (x[j] > x[p]) "right" else "left"
        grid.text(N$edge[j], x[j], (y[p]+y[j])/2, just=c(just,"bottom"))
      }
    }
  }
}

addNode <- function(X, node, parent, edge="") {
  if (class(X)[1] != "flow") {
    df.i <- data.frame(node=as.character(X), parent=0, edge="", stringsAsFactors=FALSE)
    return(structure(df.i, class=c("flow", "data.frame")))
  }
  df.i <- data.frame(node=node, parent=parent, edge=edge, stringsAsFactors=FALSE)
  X <- rbind(X, df.i)
  assign(as.character(match.call()[2]), structure(X, class=c("flow", "data.frame")), env=.GlobalEnv)
}
