remove_close_points <- function(df, d = .5) {
  m <- as.matrix(dist(df))
  w <- which(m > 0 & m < d, arr.ind = TRUE)
  remove <- unique(pmin(w[,1], w[,2]))
  df[-remove,]
}

#' Title
#'
#' @param size
#' @param sd
#' @param remove
#' @param d
#' @param method
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
quiltblock <- function(size = 4, sd = .2, remove = FALSE, d = .5,
                       method = "random", seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  df <- expand.grid(x = 0:size, y = 0:size)
  inside <- expand.grid(x = 1:(size-1), y = 1:(size-1))
  outside <- df[which((df$x+.1*df$y) %in% setdiff(df$x+.1*df$y,
                                                  inside$x+.1*inside$y)),]
 if(method == "block") {
   inside <- data.frame(x = inside$x + rnorm((size-1)^2, 0, sd),
            y = inside$y + rnorm((size-1)^2, 0, sd))
   } else {
   inside <- data.frame(x = runif((size-1)^2, size/5, size*.8),
                        y = runif((size-1)^2, size/5, size*.8))
   }
 df <- rbind(outside, inside)
 df <- df[(df$x>=0 & df$x <= size & df$y >=0 & df$y <=size),]
 if (remove) df <- remove_close_points(df, d)
 plot(df, cex = 0, ann = FALSE, axes = FALSE, asp = 1)
 plot(tripack::tri.mesh(df), add = TRUE, do.points = FALSE)
}
