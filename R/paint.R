#' Fill Delaunay triangles
#'
#' @param df data.frame of x,y pairs
#' @param triangles list of vertices returned by \code{get_triangles()}
#' @param col vector of fill colors
#'
#' @export
#'
#' @examples
#'
#' paint()
#'
paint <- function(df = NULL, triangles = NULL, col = palette.colors(), add = FALSE) {
  if (is.null(df)) df <- generate_data()
  if (!add) plot(df, cex = 0)
  if (is.null(triangles)) triangles <- get_triangles(df)
  col <- rep(col, length(triangles))
  for(i in seq_along(triangles)) {
    polygon(df[triangles[[i]],], col = col[i])
  }
}


#' Convert list of segment endpoints to triangles
#'
#' @param df
#'
#' @return list of vertices (based on rownames of df)
#' @export
#'
#' @examples
#'
#' get_triangles(generate_data(np = 9))
#'
get_triangles <- function(df) {
  d <- deldir::deldir(df)
  segs <- d$delsgs[,c("ind1", "ind2")]
  combos <- data.frame(combn(nrow(segs), 3))
  triangles <- lapply(combos, function(x) length(unique(unlist(segs[x,]))) == 3)
  segs2 <- combos[,unlist(triangles)]
  lapply(segs2, function(x) unique(unlist(segs[x,])))
}

