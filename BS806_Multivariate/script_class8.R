### Example 1
head(USArrests)
dim(USArrests)
sqrt(apply(USArrests,2,var))

plot(USArrests)
# compute principal components
pca1 <- prcomp(USArrests, scale=T)
pca1
(13.2 -mean(USArrests$Murder))/sqrt(var(USArrests$Murder))*( -0.5358995) +
  (236-mean(USArrests$Assault))/sqrt(var(USArrests$Assault))*(-0.5831836) +
  (58-mean(USArrests$UrbanPop))/sqrt(var(USArrests$UrbanPop))*(-0.2781909) +
  (21.2-mean(USArrests$Rape))/sqrt(var(USArrests$Rape))*(-0.5434321)

sum(((USArrests[1,]-pca1$center)/pca1$scale)*pca1$rotation[,1])

## generate summary of loadings
summary(pca1)
plot(pca1)

# extract principal components
pca1$x[1:5,]

# plot PCs
plot(pca1$x[,1:2])
biplot(pca1)