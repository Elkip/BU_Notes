library(epiR)

a_prime <- function(t, m1, n1, aOR) {
  interval <- sqrt(t^2 - 4*(aOR - 1)*(aOR)*m1*n1)
  ci_pos <- (t + interval) / (2*(aOR - 1))
  ci_neg <- (t - interval) / (2*(aOR - 1))
  print(paste(ci_pos, ci_neg))
}

nvrFrmSmkrs <- matrix(c(41, 192, 39, 199), ncol = 2, byrow = T)
crtSmkrs <- matrix(c(145, 205, 36, 102), ncol=2, byrow = T)
