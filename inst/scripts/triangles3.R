# an attempt at triangle mesh
#
# This is a big improvement over triangles2.R
#
# Rather than find the closest point to the current triangle, it finds the shortest distance between any unused (not part of any triangles) point and any used point. If a used point is in more than one triangle, it chooses the one which the shortest total distance between the three vertices of each triangle and the unused point.
#
# If it is possible to draw an edge from the new point to all three vertices of the maybe triangle, only the shortest two are maybe (this is an improvement over triangles2.R)


n <- 10
x <- rep(1:n, n) + rnorm(n^2)/10
y <- rep(1:n, each = n) + rnorm(n^2)/10
df <- data.frame(x,y)

#df <- read.csv("data/readme_example.csv")
i <- 50
dmat <- as.matrix(dist(df, diag = TRUE, upper = TRUE))

# initial plot
plot(df, pch = 16, cex = .5, asp = 1, axes = FALSE,
     ann = FALSE)
title("triangles3.R")
#text(df+.1, col = "red", cex = .7)
used <- rep(0, nrow(df))

# original triangle
d <- dmat[,i]
d[i] <- 1000
i2 <- which(d == min(d))[1]
d <- dmat[,i2]
d[c(i, i2)] <- 1000
i3 <- which(d == min(d))[1]
used[c(i, i2, i3)] <- 1
vertices <- df[c(i, i2, i3),]
polygon(vertices)
triangle <- data.frame(i, i2, i3)
text(mean(vertices$x), mean(vertices$y), nrow(triangle))


# start

while(sum(used) < length(used)) {
  # find the used / unused point pair with shortest distance
  minidmat <- dmat[used==1, used==0, drop = FALSE]
  pts <- which(minidmat == min(minidmat), arr.ind = TRUE)[1,]
  newp <- which(used==0)[pts[2]]
  u <- which(used==1)[pts[1]]
  p <- df[newp,]
  triopt <- triangle[u %in% triangle$i || u %in% triangle$i2 || u %in% triangle$i3]
  triopt$dist <- apply(triopt, 1, function(x) sum(dmat[newp, x]))
  v <- as.numeric(triopt[triopt$dist == min(triopt$dist),][,1:3])
  i <- v[1]
  i2 <- v[2]
  i3 <- v[3]

  #readline("Continue?")
  maybe <- vector()
  if (!intersect(df[i,], df[i2,], df[i3,], p)) maybe <- c(maybe, i3)
  if (!intersect(df[i,], df[i3,], df[i2,], p)) maybe <- c(maybe, i2)
  if (!intersect(df[i2,], df[i3,], df[i,], p)) maybe <- c(maybe, i)

  if (length(maybe) > 2) {
    z <- dmat[newp, maybe]
    maybe <- as.numeric(names(z[z != max(z)]))
  }
  i <- maybe[1]
  i2 <- maybe[2]
  i3 <- newp

  # new triangle vertices
  used[newp] <- 1
  vertices <- df[c(i, i2, i3),]
  polygon(vertices)
  triangle <- rbind(c(i, i2, i3), triangle)
  text(mean(vertices$x), mean(vertices$y), nrow(triangle), cex = .7)
}









