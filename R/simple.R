#' Failed triangle mesh algorithms
#'
#' Very simple mesh algorithms that don't work but make aRt.
#'
#' @param df two column data.frame with (x, y) coordinates (if df has more than two columns the first two will be used)
#'
#' @param i starting point (row number)
#' @param np number of points (used to generate data if )
#' @param method algorithm for choosing the vertices of the next triangle, in both cases a new point and a triangle are identified and a new triangle is drawn by connecting the new point to the triangle vertices as long as the new side doesn't intersect with the current triangle. The methods for choosing the new points and triangles vary.
#' \describe{
#'   \item{\code{closepoint}}{The new point is the closest point to third vertex of current triangle; the reference triangle is the current triangle.}
#'   \item{\code{anypair}}{The new point is the closest unused point (not in a triangle) to any used point. The triangle is the closest triangle to the point that has the used point as a vertex.}
#' }
#' @param tlab logical if \code{TRUE} triangles are numbered
#' @param plab logical if \code{TRUE} points are numbered
#' @param seed integer seed used if data is generated
#' @param col fill color for triangles
#'
#' @return List with the following components:
#'
#' \describe{
#'   \item{\code{df}}{data.frame of points}
#'   \item{\code{triangle}}{data.frame of triangle vertices}
#'   }
#'
#' @export
#'
simple <- function(df = NULL, i = 4, np = NULL, method = "anypair",
                   tlab = FALSE, plab = FALSE, seed = 8, col = NA) {
  border <- "black"
  if (is.null(df)) df <- generate_data(seed = seed)
  df <- data.frame(pts = 1:nrow(df), x = df[,1], y = df[,2])
  head(df)
  if (is.null(np)) {
    np <- nrow(df)
    unfinished <- FALSE
  } else {
    if (np >= nrow(df)) stop(paste("np must be less than", nrow(df)))
    np <- np + 1
    unfinished <- TRUE
  }
  plot(df$x, df$y, pch = 16, asp = 1, axes = FALSE,
       ann = FALSE, col = "white")
  if (plab) text(df$x+.1, df$y+.1, col = "red", cex = .7, xpd = TRUE)
  dmat <- as.matrix(dist(df[,2:3], diag = TRUE, upper = TRUE))
  used <- rep(0, nrow(df))

  # initial triangle -- same for all methods
  d <- dmat[,i]
  d[i] <- max(dmat) + 1
  i2 <- which(d == min(d))[1] # 2nd vertex
  d[i2] <- max(dmat) + 1
  i3 <- which(d == min(d))[1] # 3rd vertex
  triangle <- data.frame(i, i2, i3)
  v <- df[c(i, i2, i3),]
  used[c(i, i2, i3)] <- 1
  polygon(v$x, v$y, border = border, col = col)
  if (tlab) text(mean(v$x), mean(v$y[1:3]), sum(used)-2, cex = .7,
                 col = border)

  newp <- i3

  while(sum(used) < np) {
    # find newp and triangle to draw to (v)
    if (method == "closepoint") {
      d <- dmat[,newp]
      d[used==1] <- max(dmat) + 1
      newp <- which(d == min(d))[1]
    } else if (method == "anypair") {
      # find the used / unused point pair with shortest distance
      minidmat <- dmat[used==1, used==0, drop = FALSE]
      pts <- which(minidmat == min(minidmat), arr.ind = TRUE)[1,]
      newp <- which(used==0)[pts[2]]
      u <- which(used==1)[pts[1]]
      triopt <- triangle[u %in% triangle$i || u %in% triangle$i2 || u %in% triangle$i3]
      triopt$dist <- apply(triopt, 1, function(x) sum(dmat[newp, x]))
      verts <- as.numeric(triopt[triopt$dist == min(triopt$dist),][,1:3])
      v <- df[verts,]
    } else {
      stop("Unknown method")
    }

    used[newp] <- 1
    p <- df[newp, c("x", "y")]

    # Identify and draw new triangle
    twovert <- vector()
    if (sameside(v[3, 2:3], v[2, 2:3], v[1, 2:3], p) ||
        sameside(v[1, 2:3], p, v[3, 2:3], v[2, 2:3])) twovert <- v$pt[1]

    if (sameside(v[3, 2:3], v[1, 2:3], v[2, 2:3], p) ||
        sameside(v[2, 2:3], p, v[3, 2:3], v[1, 2:3])) twovert <- c(twovert, v$pt[2])

    if (sameside(v[1, 2:3], v[2, 2:3], v[3, 2:3], p) ||
        sameside(v[3, 2:3], p, v[1, 2:3], v[2, 2:3])) twovert <- c(twovert, v$pt[3])

    if (length(twovert) > 2) {
      vdf <- df[twovert,]
      vdf$dist <- sqrt((vdf$x-df$x[newp])^2 + (vdf$y-df$y[newp])^2)
      twovert <- vdf$pt[vdf$dist != max(vdf$dist)]
    }

    # new triangle vertices
    v <- df[c(twovert, newp),]
    triangle <- rbind(c(twovert, newp), triangle)
    if (unfinished && sum(used) == np) {
      points(p, col = "red", pch = 16)
    } else {
      if(unfinished && sum(used) == np - 1) border <- "red"
      polygon(v$x, v$y, border = border, col = col)
      if (tlab) text(mean(v$x), mean(v$y[1:3]), sum(used)-2, cex = .7,
                     col = border)
    }
  }
  invisible(list(df, triangle))
}

