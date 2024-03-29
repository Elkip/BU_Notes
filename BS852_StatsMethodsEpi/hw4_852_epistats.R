# 1
mat1 <- matrix(c(55, 19, 128, 164), ncol = 2)
((366 - 1)*(55*164 - 128*19)^2) / ((55+128)*(55+19)*(128+164)*(19+164))

chisq.test(mat1, correct = F)

mat2 <- matrix(c(12, 7, 43, 121), ncol = 2)
chi <- (43 - 7)^2 / (43+7)

6.14*exp(-1.96*sqrt(1/43 + 1/7))
6.14*exp(1.96*sqrt(1/43 + 1/7))

# 2
(9 - 3)^2 / (9 + 3)

((27 - 1)*(4*11 - 3*9)^2) / ((4+9)*(4+3)*(9+11)*(3+11))

1.629^(1+1.96/sqrt(0.294898))
1.629^(1-1.96/sqrt(0.294898))

((30)*(9*6 - 12*3)^2) / ((9+12)*(9+3)*(12+6)*(3+6))
pchisq(.23, 1, lower.tail = F)
