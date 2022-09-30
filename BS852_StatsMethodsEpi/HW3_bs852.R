# 1

mat1 <- matrix(c(55, 25, 35, 20), ncol = 2)
mat2 <- matrix(c(38, 15, 35, 20), ncol = 2)
mat3 <- matrix(c(43, 11, 35, 20), ncol = 2)
mat = mat1
mat <- matrix(c(mat1, mat2, mat3))
mat[1]
m = vector(length = 4)
n = vector(length = 2)

m[1] = mat1[1,1] + mat1[2,1]
m[2] = mat1[1,2] + mat1[2,2]
m[3] = mat1[1,3] + mat1[2,3]
m[4] = mat1[1,4] + mat1[2,4]
prop.trend.test(mat1[1,], n=m, score = c(1, 2, 3, 4))

OR1 = (55 * 20) / (35 * 25) # 1.26
OR2 = (38 * 20) / (35 * 15) # 1.44
OR3 = (43 * 20) / (35 * 11) # 2.23
