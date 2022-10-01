library(epitools)
# 1

mat1 <- matrix(c(55, 25, 35, 20), ncol = 2)
mat2 <- matrix(c(38, 15, 35, 20), ncol = 2)
mat3 <- matrix(c(43, 11, 35, 20), ncol = 2)
coffee_mat <- array(c(mat1, mat2, mat3), c(2,2,3))
rownames(coffee_mat) <- c("Coffee", "No Coffee")
colnames(coffee_mat) <- c("MI Cases", "Controls")

# Single frame of all coffee drinkers
coffee_full <- matrix(c(matrix(coffee_mat[,2,1], ncol = 1), coffee_mat[,1,]), ncol = 4)
rownames(coffee_full) <- c("MI Cases", "Controls")
colnames(coffee_full) <- c("0, <1", "1,2", "3,4", "5+")
m <- colSums(coffee_full)

prop.trend.test(coffee_full[1,], n=m, score = c(1, 2, 3, 4))

for (i in 1:dim(cofee_mat)[3]) {
  OR <- (coffee_mat[1,1,i] * coffee_mat[2,2,i]) / (coffee_mat[2,1,i] * coffee_mat[1,2,i])
  print(paste0("Odds Ratio ", i, " ", OR))
}

# 2
mat1 <- matrix(c(2, 10, 5, 50), ncol = 2)
mat2 <- matrix(c(9, 13, 17, 57), ncol = 2)
mat3 <- matrix(c(12, 2, 9, 6), ncol = 2)
cancer_mat <- array(c(mat1, mat2, mat3), c(2,2,3))
colnames(cancer_mat) <- c("Receptor Low", "Recptor High")
rownames(cancer_mat) <- c("Dead", "Alive")

mor_num = 0
mor_den = 0
mh_num = 0
mh_den = 0

for (i in 1:dim(cancer_mat)[3]) {
  OR <- (cancer_mat[1,1,i] * cancer_mat[2,2,i]) / (cancer_mat[2,1,i] * cancer_mat[1,2,i])
  print(paste0("Odds Ratio ", i, " ", OR))
  n_total <- sum(cancer_mat[,,i])
  mor_num <- mor_num + (cancer_mat[1,1,i]*cancer_mat[2,2,i]/n_total)
  mor_den <- mor_den + (cancer_mat[1,2,i]*cancer_mat[2,1,i]/n_total)
}

mor_cancer = mor_num/mor_den

#prop.trend.test(x=c(5,9,12), n=c(12, 22, 14), score = c(1,2,3)) 
#prop.trend.test(x=c(7,26,21), n=c(77, 96, 29), score = c(1,2,3)) 

get.odds <- function(mtrxList) {
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
  mor_num = 0
  mor_den = 0
  mh_num = 0
  mh_den = 0
  bd_num = 0
  bd_den_1 = 0
  bd_den_2_1 = 0
  bd_den_2_2 = 0
  
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
    
    mor_num <- mor_num + (a[i]*d[i]/n_total[i])
    mor_den <- mor_den + (b[i]*c[i]/n_total[i])
    mh_num <- mh_num + ((a[i]*d[i] - b[i]*c[i]) / n_total[i])
    mh_den <- mh_den + ((n0[i]*n1[i]*m0[i]*m1[i])/(n_total[i]^2*(n_total[i] - 1)))
    
    print(paste("Odds Ratio: ", (((a[i]/c[i])/(b[i]/d[i])))))
    
    a_prime <- 
    print(paste0("Expected value of a: "))
  } 
  
  mh <- mh_num^2 / mh_den 
  print(mh)
  print(pchisq(mh,t_size-1,lower.tail = F))
  
  mor <- mor_num / mor_den
  print(paste("mOR", mor))
  
  for(i in 1:t_size) {
    # print(paste0(mor, "*(", m1[i], "+", n1[i], ") + ", m0[i], " - ", n1[i]))
    t <- mor*(m1[i] + n1[i]) + m0[i] - n1[i]
    print(paste("t: ", t))
    print(paste0("(", t, " + sqrt(", t^2, " - ", 4, " * (", mor, " - ", 1, ") *", mor, " * ", m1[i], " * ", n1[i], "))/(", 2, " * (", mor, " - ", 1, "))"))
    a_prime_high <- ((t + sqrt(t^2 - 4 * (mor - 1) * mor * m1[i] * n1[i]))/(2 * (mor - 1)))
    a_prime_low <- ((t - sqrt(t^2 - 4 * (mor - 1) * mor * m1[i] * n1[i]))/(2 * (mor - 1)))
    print(paste0("Expected value of a: ", a_prime_low, " or ", a_prime_high))
    var_a_high <- ((1/a_prime_high) + (1/(m1[i] - a_prime_high)) + (1 / (n1[i] - a_prime_high)) + (1 / (m0[i] - n1[i] + a_prime_high)))^-1
    var_a_low <- ((1/a_prime_low) + (1/(m1[i] - a_prime_low)) + (1 / (n1[i] - a_prime_low)) + (1 / (m0[i] - n1[i] + a_prime_low)))^-1
    print(paste0("Variance of a: ", var_a_low, " or ", var_a_high))
    
    # Chi using LOW estimates
    bd_num <- bd_num + i * (a[i] - a_prime_low)
    bd_den_1 <- bd_den_1 + (i^2)*var_a_low
    bd_den_2_1 <- bd_den_2_1 + (i*var_a_low)
    bd_den_2_2 <- bd_den_2_2 + var_a_low
  }
  
  mor_ci_top <- mor^(1+1.96/sqrt(mh))
  mor_ci_btm <- mor*(1-1.96/sqrt(mh))
  print("Confidence interval:")
  print(paste(mor_ci_btm, mor_ci_top))
  
  bd_chi <- bd_num^2 / (bd_den_1 - (bd_den_2_1^2 / bd_den_2_2))
  print(paste("BD chi value: ", bd_chi))
  print(pchisq(bd_chi, 1, lower.tail = F))
}

get.odds(list(mat1,mat2,mat3))


(((2 - 2.301) + 2*(9 - 9.341) + 3*(12 - 11.438))^2) /
  ((1.298 + 4*3.795 + 9*1.305) - (((1.298 + 2*3.795 + 3*1.305)^2) / (1.298 + 3.795 + 1.305)))
