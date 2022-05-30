# an attempt at triangle mesh

sameside <- function(v1, v2, v3, p) {
  y <- c(v1$y, v2$y)
  x <- c(v1$x, v2$x)
  mod <- lm(y ~ x)
  v3sign <- sign(v3$y - predict(mod, v3))
  psign <- sign(p$y - predict(mod, p))
  ifelse(v3sign == psign, TRUE, FALSE)
}

p_in_triangle <- function(v1, v2, v3, p) {
  test1 <- sameside(v1, v2, v3, p)
  test2 <- sameside(v2, v3, v1, p)
  test3 <- sameside(v1, v3, v2, p)
  ifelse(test1 && test2 && test3, TRUE, FALSE)
}

n <- 4
x <- rep(1:n, n) + rnorm(n^2)/10
y <- rep(1:n, each = n) + rnorm(n^2)/10

i <- ceiling(n^2/2) # starting vertex

dmat <- as.matrix(dist(df, diag = TRUE, upper = TRUE))

# initial plot
plot(df, pch = 16, cex = .5, asp = 1, axes = FALSE,
     ann = FALSE)
used <- rep(0, length(x))

# original triangle
for (j in 1:100) {
v <- sample(length(x), 3)
p_in_t <- FALSE
allpts <- 1:16
otherpts <- allpts[!(allpts %in% v)]
for (i in seq_along(otherpts)) {
  if (p_in_triangle(df[v[1],], df[v[2],], df[v[3],], df[i,])) p_in_t <- TRUE
}
if(!p_in_t) {
  polygon(df[v,])
  readline("Continue?")
}
}

