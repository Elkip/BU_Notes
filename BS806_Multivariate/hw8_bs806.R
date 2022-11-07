hem <- read.csv("/home/elkip/Datasets/sca.csv")

# 2a
hem2 <- hem[,8:12]
plot(hem2)

par(mfrow=c(1,4))
hist(hem2$F12LDH)
hist(hem2$F17RETIC)
hist(hem2$F12SGOT)
hist(hem2$F12BLRBN)

# 2b
pca <- prcomp(hem2[,2:5])
pca

# 2d
pca <- prcomp(hem2[,2:5], scale. = T)
pca
