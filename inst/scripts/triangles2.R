# a second attempt at triangle mesh
#
# Next steps: change the algorithm so it goes point by point finding the closest *triangle* (based on all vertices) rather than finding the closest *point* to the current triangle. The current methods results in big jumps once the close points are used up.
#
# Same algorithm as triangles.R except only drawing two lines even if three are possible: if i2 and i3 work, i is not tested.

# create data
n <- 10
x <- rep(1:n, n) + rnorm(n^2)/10
y <- rep(1:n, each = n) + rnorm(n^2)/10
df <- data.frame(x,y)

df <- read.csv("data/readme_example.csv")

# set up
m <- matrix(0, nrow(df), nrow(df))
diag(m) <- 1
dmat <- as.matrix(dist(df, diag = TRUE, upper = TRUE))


# initial plot
plot(df, pch = 16, cex = .5, asp = 1, axes = FALSE,
     ann = FALSE)
title("triangles2.R")
text(df+.1, col = "red", cex = .7)
used <- rep(0, nrow(df))

# original triangle
i <- 8
d <- dmat[,i]
d[i] <- 1000
i2 <- which(d == min(d))[1]
d <- dmat[,i2]
d[c(i, i2)] <- 1000
i3 <- which(d == min(d))[1]
used[c(i, i2, i3)] <- 1
polygon(rbind(df[i,], df[i2,], df[i3,]))
#polygon(rbind(df[i,], df[i2,], df[i3,]), col = palette.colors(nrow(df), recycle = TRUE)[sum(used)])

m[i,i2] <- 1; m[i2,i] <- 1
m[i2, i3] <- 1; m[i3, i2] <- 1
m[i, i3] <- 1; m[i3, i] <- 1

# start
#

focus <- i3

while(sum(used) < nrow(df)) {
  d <- dmat[,focus]
  d[used==1] <- 1000
  newp <- which(d == min(d))[1]
  used[newp] <- 1
  p <- df[newp,]
  points(p, col = "green")
  readline("Continue?")

  count <- 0
  if (!intersect(df[i,], df[i2,], df[i3,], p)) {
    m[newp, i3] <- 1
    m[i3, newp] <- 1
    count <- count + 1
  }

  if (!intersect(df[i,], df[i3,], df[i2,], p)) {
    m[i2, newp] <- 1
    m[newp, i2] <- 1
    count <- count + 1
  }

  if (count < 2) {
    if (!intersect(df[i2,], df[i3,], df[i,], p)) {
    m[newp, i] <- 1
    m[i, newp] <- 1
    }
  }

  # new triangle vertices
  points(rbind(df[i,], df[i2,], df[i3,]), col = "black", pch = 16)
  v <- which(m[newp,] == 1)
  i <- v[1]
  i2 <- v[2]
  i3 <- v[3]
  focus <- newp
  polygon(rbind(df[i,], df[i2,], df[i3,]))
}












