# attempt at triangle mesh
#
# Algorithm
#
# Initial triangle:
#
# 1. Start with a point (i)
# 2. Find the closest point (i2)
# 3. Find the closest point to i2 (i3)
# 4. Draw a triangle with vertices i, i2, i3
#
# Repeat:
#
# 1. Find the closest point to i3 (newp)
# 2. Check newp-i
#
# Getting better... fixed the problem of checking whether
# line segments intersect
# (needed crosscheck(v[1,], v[2,] v[3,] p) || crosscheck(v[3,] p, v[1,], v[2,]),
# not just the first half)
#
# Next steps: change the algorithm so it goes point by point finding the closest *triangle* (based on all vertices) rather than finding the closest *point* to the current triangle. The current methods results in big jumps once the close points are used up.
#



#n <- 4
#x <- rep(1:n, n) + rnorm(n^2)/10
#y <- rep(1:n, each = n) + rnorm(n^2)/10
#df <- data.frame(x,y)

df <- read.csv("data/readme_example.csv")
numpts <- 16

plot(df$x, df$y, pch = 16, cex = .5, asp = 1, axes = FALSE,
     ann = FALSE)
title("triangles.R")
text(df$x+.1, df$y+.1, col = "red", cex = .7)
m <- matrix(0, nrow(df), nrow(df))
diag(m) <- 1
used <- rep(0, nrow(df))

# first side
i <- 4
d <- sqrt((df$x[i]-df$x)^2 + (df$y[i]-df$y)^2)
d[i] <- 1000
i2 <- which(d == min(d))[1]
m[i,i2] <- 1; m[i2,i] <- 1

# second side
d[i2] <- 1000
i3 <- which(d == min(d))[1]
m[i, i3] <- 1; m[i3, i] <- 1

# third side (close the triangle)
m[i2, i3] <- 1; m[i3, i2] <- 1
v <- df[c(i, i2, i3),]
used[c(i, i2, i3)] <- 1
polygon(v$x, v$y, border = "green")
text(mean(v$x), mean(v$y), 1)
focus <- i3

while(sum(used) < numpts) {
  d <- sqrt((df$x[focus]-df$x)^2 + (df$y[focus]-df$y)^2)
  d[used==1] <- 1000
  newp <- which(d == min(d))[1]
  used[newp] <- 1
  p <- df[newp, c("x", "y")]
  points(p, col = "green")

  maybe <- vector()

  if (crosscheck(v[3, 2:3], v[2, 2:3], v[1, 2:3], p) ||
      crosscheck(v[1, 2:3], p, v[3, 2:3], v[2, 2:3])) maybe <- v$pt[1]

  if (crosscheck(v[3, 2:3], v[1, 2:3], v[2, 2:3], p) ||
      crosscheck(v[2, 2:3], p, v[3, 2:3], v[1, 2:3])) maybe <- c(maybe, v$pt[2])

  if (crosscheck(v[1, 2:3], v[2, 2:3], v[3, 2:3], p) ||
      crosscheck(v[3, 2:3], p, v[1, 2:3], v[2, 2:3])) maybe <- c(maybe, v$pt[3])

  if (length(maybe) > 2) {
    vdf <- df[maybe,]
    vdf$dist <- sqrt((vdf$x-df$x[newp])^2 + (vdf$y-df$y[newp])^2)
    maybe <- vdf$pt[vdf$dist != max(vdf$dist)]
  }

  m[maybe, newp] <- 1
  m[newp, maybe] <- 1

  polygon(df$x[c(maybe, newp)], df$y[c(maybe, newp)], border = "green")

  # new triangle vertices
  focus <- newp
  v <- df[which(m[newp,] == 1),]
  text(mean(v$x), mean(v$y[1:3]), sum(used)-2, cex = .7)
}







