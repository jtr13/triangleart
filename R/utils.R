# an attempt at triangle mesh

#' @export
generate_data <- function(np = 16, seed = 13) {
  set.seed(seed)
  n <- sqrt(np)
  x <- rep(1:n, n) + rnorm(n^2)/10
  y <- rep(1:n, each = n) + rnorm(n^2)/10
  data.frame(x,y)
}

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

intersect <- function(v1, v2, v3, p) {
  y <- c(v1$y, v2$y)
  x <- c(v1$x, v2$x)
  mod <- lm(y ~ x)
  v3sign <- sign(v3$y - predict(mod, v3))
  psign <- sign(p$y - predict(mod, p))
  test1 <- ifelse(v3sign == psign, FALSE, TRUE)
  y <- c(v3$y, p$y)
  x <- c(v3$x, p$x)
  mod <- lm(y ~ x)
  v1sign <- sign(v1$y - predict(mod, v1))
  v2sign <- sign(v2$y - predict(mod, v2))
  test2 <- ifelse(v1sign == v2sign, FALSE, TRUE)
  ifelse(test1 && test2, TRUE, FALSE)
}

