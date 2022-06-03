#' Create a triangle mesh
#'
#' Uses \code{tripack::tri.mesh} to create a Delaunay triangulation.
#'
#' @param n number of grid points on the side of a square, or one quarter of the points on the circumference of a circle
#' @param sd standard deviation for jitter added (\code{block} method only)
#' @param remove logical should close points be removed?
#' @param d threshold distance for keeping points
#' @param method controls how points are chosen
#' \describe{
#'   \item{\code{block}}{Points are even spaced grid points in a square. If \code{sd} is zero, a standard half square triangle (HST) block is produced}
#'   \item{\code{random}}{Outer points are evenly spaced grid points in a square. Inner points are randomly chosen from a uniform distribution inside the square with min \code{1} and max \code{n-1}}
#'   \item{\code{circle}}{Outer points are evenly spaced on the circumference of a circle.}
#'   }
#' @param seed seed used for random number generation (optional)
#'
#' @return data.frame of randomly generated points (can be used to add to the plot)
#' @export
#'
#' @examples
#'
#' mesh()
#'
mesh <- function(df = NULL, n = 4, sd = .2, remove = FALSE, d = .5,
                       method = "circle", seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  if(is.null(df)) {
    df <- expand.grid(x = 0:n, y = 0:n)
  } else {
    df <- data.frame(x = df[,1], y = df[,2])
  }
  if (method != "circle") {
    inside <- expand.grid(x = 1:(n-1), y = 1:(n-1))
    outside <- df[which((df$x+.1*df$y) %in% setdiff(df$x+.1*df$y,
                                                    inside$x+.1*inside$y)),]
  }
 if(method == "block") {
   inside <- data.frame(x = inside$x + rnorm((n-1)^2, 0, sd),
                        y = inside$y + rnorm((n-1)^2, 0, sd))
   inside <- inside[(inside$x>=0 & inside$x <= n & inside$y >=0 & inside$y <=n),]
   } else if (method == "random") {
   inside <- data.frame(x = runif((n-1)^2, 1, n-1),
                        y = runif((n-1)^2, 1, n-1))
   inside <- inside[(inside$x>=0 & inside$x <= n & inside$y >=0 & inside$y <=n),]
   } else if (method == "circle") {
     a <- seq(0, 2*pi, length.out = n*4)
     outside <- data.frame(x = sin(a), y = cos(a))
     inside <- data.frame(x = runif(n*5, -1, 1), y = runif(n*5, -1, 1))
     inside <- inside[which(inside$x^2 + inside$y^2 < 1),]
   } else {
     stop("Unknown method")
   }
 outside$type <- "out"
 inside$type <- "in"
 df <- rbind(outside, inside)
 if (remove) df <- remove_close_points(df, d)
 plot(df$x, df$y, cex = 0, ann = FALSE, axes = FALSE, asp = 1)
 plot(suppressWarnings(tripack::tri.mesh(df$x, df$y)), add = TRUE, do.points = FALSE)
 points(df$x, df$y, cex = .7, pch = 16, col = "red")
 invisible(df)
}

remove_close_points <- function(df, d = .5) {
  m <- as.matrix(dist(df[,c("x", "y")]))
  w <- data.frame(which(m > 0 & m < d, arr.ind = TRUE))
  w$t1 <- df$type[w$row]
  w$t2 <- df$type[w$col]
  w <- w[which(w$row < w$col),]
  w <- w[w$t1 == "in" | w$t2 == "in",]
  w$remove <- ifelse(w$t1 == "out", w$col, w$row)
  if(length(w$remove) > 0) {
    return(df[-w$remove,])
  } else {
    return(df)
  }
}
