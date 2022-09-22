library(epiR)

a_prime <- function(t, m1, n1, aOR) {
  interval <- sqrt(t^2 - 4*(aOR - 1)*(aOR)*m1*n1)
  ci_pos <- (t + interval) / (2*(aOR - 1))
  ci_neg <- (t - interval) / (2*(aOR - 1))
  print(paste(ci_pos, ci_neg))
}

bd.var <- function(mtrxList) {
    t_size = length(mtrxList)
    a <- vector(length = t_size)
    b <- vector(length = t_size)
    c <- vector(length = t_size)
    d <- vector(length = t_size)
    n_total <- vector(length = t_size)
    n0 <-  vector(length = t_size)
    n1 <- vector(length = t_size)
    m0 <- vector(length = t_size)
    m1 <- vector(length = t_size)
    
    res = 0
    
    for(i in 1:t_size) {
      mat = mtrxList[[i]]
      # print(mat)
      a[i] = mat[1,1]
      b[i] = mat[1,2]
      c[i] = mat[2,1]
      d[i] = mat[2,2]
      
      n_total[i] <- a[i] + b[i] + c[i] + d[i]
      n1[i] <- a[i] + c[i]
      n0[i] <- b[i] + d[i]
      m1[i] = a[i] + b[i]
      m0[i] = c[i] + d[i]
      # Fix me
      res = res + ((a[i] - a_prime(a[i])^2) / (1/a[i] + 1/b[i] + 1/c[i] + 1/d[i]))
    } 
}

nvrFrmSmkrs <- matrix(c(41, 192, 39, 199), ncol = 2, byrow = T)
crtSmkrs <- matrix(c(145, 205, 36, 102), ncol=2, byrow = T)
