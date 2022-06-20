#' nodoubles
#'
#' Attempt at finding A(n, 4, 3) without knowing that notation. The algorithm often underestimates: 2, 4, 7, 7, 8, 10, 13, 17, 22, 28, 35 instead of the correct ones (see below)
#'
#' @param n number of items (>= 4)
#'
#' @param combos logical, if FALSE don't show combos
#'
#' @return number of combos of 3 that share no more than 1 item
#' @export
#'
#' @examples
#'
#' nodoubles(7)
#'

nodoubles <- function(n, combos = FALSE) {
  df <- combn(n, 3)
  good <- vector()
  good[1] <- TRUE
  for (i in 2:ncol(df)) {
    good[i] <- TRUE
    for (j in 1:(i-1)) {
      if (length(setdiff(df[,i], df[,j])) < 2 & good[j]) good[i] <- FALSE
    }
  }
  df <- data.frame(df)
  if (combos) {
    as.list(df[,good, drop = FALSE])
  } else {
    ncol(df[,good, drop = FALSE])
  }
}


# Bounds for Binary Codes of Length Less Than 25
# IEEE TRANSACTIONS ON INFORMATION THEORY, VOL. IT-24, NO. 1, JANUARY 1978
# p. 90 d = 4, w = 3
# numbers in table on p. 82, Table II, column 3
# (1, 2, 4, 7, 8, 12, 13, 17 20, 26, 28, 35, ...)
# A(15, 4, 3) is the Kirkman schoolgirl problem

#' Number of binary codes
#'
#' d = 4, w = 3
#'
#' @param n length of binary code
#'
#' @return number of codewords
#' @export
#'
#' @examples
#'
#' A(15)
#'
A <- function(n) {
  A <- floor(n/3 * floor((n-1)/2))
  if (n %% 6 == 5) A <- A - 1
  return(A)
}
