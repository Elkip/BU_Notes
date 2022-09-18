library(epitools)
# 1

a1 <- 51
b1 <- 177
c1 <- 20
d1 <- 141
or1 <- (a1*d1) / (b1*c1)
mat_ht <- matrix(c(a1, b1, c1, d1),ncol=2,byrow=TRUE)
colnames(mat_ht) <- c("5+ Cups", "<5 Cups")
rownames(mat_ht) <- c("MI Case", "Control")
View(mat_ht)
n1_total <- a1 + b1 + c1 + d1
n1_1 <- a1 + c1
n1_0 <- b1 + d1
m1_1 = a1 + b1
m1_0 = c1 + d1
chisq.test(mat_ht, correct = F)

oddsratio.wald(mat_ht)


a2 <- 93
b2 <- 166
c2 <- 41
d2 <- 247
mat_noHt <- matrix(c(a2, b2, c2, d2),ncol=2,byrow=TRUE)
colnames(mat_noHt) <- c("5+ Cups", "<5 Cups")
rownames(mat_noHt) <- c("MI Case", "Control")
or2 <- (a2*d2) / (b2*c2)
n2_total <- a2 + b2 + c2 + d2
n2_1 <- a2 + c2
n2_0 <- b2 + d2
m2_1 <- a2 + b2
m2_0 <- c2 + d2

mat_both <- array(c(mat_ht, mat_noHt), dim = c(2,2,2))
mor <- (a1*d1/n1_total + a2*d2/n2_total)/(b1*c1/n1_total + b2*c2/n2_total)
mh <- (((a1*d1 - b1*c1) / n1_total) + ((a2*d2 - b2*c2) / n2_total))^2 / (((n1_0*n1_1*m1_0*m1_1)/(n1_total^2*(n1_total - 1))) + ((n2_0*n2_1*m2_0*m2_1)/(n2_total^2*(n2_total - 1))))
pchisq(mh, 1, lower.tail = F)
crude_or = ((a1+a2)*(d1+d2))/((b1+b2)*(c1+c2))

# 2
female <- matrix(c(24,139,21,325), ncol=2, byrow=T)
colnames(female) <- c("Homeless", "Not Homeless")
row.names(female) <- c("MDR TB", "non_MDR TB")
oddsratio.wald(female)
chisq.test(female)
f_or <- (female[1,1]*female[2,2])/(female[1,2]*female[2.1])

male <- matrix(c(95,732,67,1337), ncol=2, byrow=T)
colnames(male) <- c("Homeless", "Not Homeless")
row.names(male) <- c("MDR TB", "non_MDR TB")
oddsratio.wald(male)
chisq.test(male)
m_or <- (male[1,1]*male[2,2])/(male[1,2]*male[2.1])

total <- matrix(c(119, 871, 88, 1662), ncol=2, byrow = T)
colnames(total) <- c("Homeless", "Not Homeless")
row.names(total) <- c("MDR TB", "non_MDR TB")
oddsratio.wald(total)
chisq.test(total)
total_or <- (total[1,1]*total[2,2])/(total[1,2]*total[2.1])


# INPUT: A list of 2x2 matrices
# OUTPUT: Averaged Odds Ratio, OR Confidence interval and MH chi-squared value
get.mor <- function(mtrxList) {
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
  } 
  
  
  mh <- mh_num^2 / mh_den 
  print(mh)
  print(pchisq(mh,1,lower.tail = F))
  
  mor <- mor_num / mor_den
  print(paste("mOR", mor))
  mor_ci_top <- mor^(1+1.96/sqrt(mh))
  mor_ci_btm <- mor*(1-1.96/sqrt(mh))
  print("Confidence interval:")
  print(paste(mor_ci_btm, mor_ci_top))

}

get.mor(list(mat_ht, mat_noHt))
get.mor(list(female, male))
