## example to calculate TotSS and WSS
set.seed(1)
data.ex <- rnorm(20,0,1)
dim(data.ex) <- c(5,4)
data.ex

mu <- apply(data.ex, 2, mean);mu
TotSS <- sum((t(data.ex)- mu)^2);TotSS

Cluster.1 <- data.ex[1:3,]
Cluster.2 <- data.ex[4:5,]
mu.1 <- apply(Cluster.1, 2, mean); mu.1
TotSS.1 <- sum((t(Cluster.1)- mu.1)^2); TotSS.1

mu.2 <- apply(Cluster.2, 2, mean); mu.2
TotSS.2 <- sum((t(Cluster.2)- mu.2)^2); TotSS.2

## K-means
km <- kmeans(data.ex, 2)
km
km$withinss
km$totss

## USArrest data
km <- kmeans(USArrests, 2)
km

## how to choose k
Wss <- c()
for(i in 1:20){
  km <- kmeans(USArrests, i)
  Wss <- c(Wss, km$tot.withinss )}
plot(c(1:20), Wss, type="l")

# visualization using PCA
km <- kmeans(USArrests, 3)
km$size
km$centers
pca1 <- prcomp(USArrests, scale=T)
plot(pca1$x[,1], pca1$x[, 2])
for(i in 1:3){
  points(pca1$x[which(km$cluster == i),1],
         pca1$x[which(km$cluster == i),2],col=i)}

# standardize variable
mu <- apply(USArrests,2,mean)
sigma <- sqrt(apply(USArrests,2,var))
mu
sigma

data <- c()
for(i in 1:ncol(USArrests)){
  data <- cbind( data, (USArrests[,i]-mu[i])/sigma[i])
}
USA.data <- as.data.frame(data)
names(USA.data) <- names(USArrests)
row.names(USA.data) <- row.names(USArrests)

Wss <- c()
for(i in 1:20){
  km <- kmeans(USA.data, i)
  Wss <- c(Wss, km$tot.withinss )}
plot(c(1:20), Wss, type="l")

# visualization using PCA
km <- kmeans(USA.data, 3)
km

pca1 <- prcomp(USA.data, scale=T)
plot(pca1$x[,1], pca1$x[, 2])
for(i in 1:3){
  points(pca1$x[which(km$cluster == i),1],pca1$x[which(km$cluster == i),2],col=i)}



###### Hierarchical clustering of USA Arrests Data
hc <- hclust(dist(USA.data) )
plot(hc)
rect.hclust(hc,k=2)

clu.lab <- cutree(hc, k=2)
km <- kmeans(USA.data, 2)
table(km$cluster, clu.lab)

### checking how similar between kmeans and hierachical clustering
pca1 <- prcomp(USA.data, scale=T)
plot(pca1$x[,1], pca1$x[, 2])
for(i in 1:2){
  points(pca1$x[which(clu.lab == i),1],
		pca1$x[which(clu.lab == i),2],col=i)
  points(pca1$x[which(km$cluster == i),1],
		pca1$x[which(km$cluster== i),2],col=i, pch="*")}

### use average linkage
hc <- hclust(dist(USA.data), method="average" )
plot(hc)
rect.hclust(hc,k=2)

clu.lab <- cutree(hc, k=2)
km <- kmeans(USA.data, 2)
table(km$cluster, clu.lab)


#### resampling to detect significant cluster
p.length <- c(); rand.tree.h <- c()
data <- USA.data;  
for(i in 1:10){
  ## permute data within each column/variable, resample without replacement
  test <-  sample(data[,1])
  for(ind.c in 2:ncol(data)){
    test <- cbind(test, sample((data[,ind.c])))}

  ## using permuted data to construct hierachical clustering
  ## then save its $height, i.e. [[2]]
  ## rand.tree.h is matrix with $height, each column corresponds to each permutation
  rand.tree <- hclust(dist(test))
  rand.tree.h <- cbind(rand.tree.h, rand.tree[[2]])

  ## p.length: quantiles value of $height based on permuted/ref data
  ## each row correponds to each permutation
  ## each column corresponds to different quantiles, here 0.95
  ## e.g. probs=c(0.925, 0.95, 0.975, 0.99 )
p.length <- rbind(p.length,quantile(rand.tree[[2]],probs=c( 0.95)))
}
head(p.length)
## on average, what's the $height value and different quantile value
## then compare with real data's $height value
rand.tree.ref <- apply(rand.tree.h,1,mean)
quantile(rand.tree.ref, prob=c(0.95))
threshold <- apply(p.length,2,mean)
threshold

## compare QQ-plot of real data clustering vs reference data clustering
real.tree <- hclust(dist((data)))
plot((rand.tree.ref),(real.tree[[2]]), 
	xlab="Expected", ylab="Observed"); 
abline(0,1)

## Cut dendrogram to find clusters with threshold based on reference 
hc <- hclust(dist(USA.data))		## i.e. real.tree
plot(hc)
threshold  #
rect.hclust(hc,h=threshold); title(sub="5% significance", line=2)

#### if we replace prob=c(0.95) with prob=c(0.99) in earlier step
threshold <- quantile(rand.tree.ref, prob=c(0.99))
plot(hc)
threshold
rect.hclust(hc,h=threshold); title(sub="1% significance", line=2)



######## Heatmap
#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#
#BiocManager::install("Heatplus")

library(Heatplus)
map1<-  annHeatmap2(t(USA.data), 
			annotation = NULL, legend =2)
plot(map1, cex=0.75, cex.lab=0.75, cex.axis=0.75) 

### showing the cuting tree
threshold <- quantile(rand.tree.ref, prob=c(0.95))
map1<-  annHeatmap2(t(USA.data),cluster=list(cuth= threshold),
			legend =2)
plot(map1, cex.lab=0.75, cex.axis=0.75) 

## using different function to generate heatmap
heatmap_plus(t(USA.data), scale="row",
              col = RGBColVec(64),reorder = c(FALSE, TRUE))
heatmap_plus(t(USA.data), scale="row",
             col = RGBColVec(64),reorder = c(TRUE, TRUE))

### summary the clustering analysis
hc <- hclust(dist(USA.data))
threshold
clu.lab = cutree(hc, h=threshold)

cluster.summary <- 
  as.data.frame(
    rbind(tapply(USA.data$Murder, clu.lab,mean),
          tapply(USA.data$Assault, clu.lab,mean),
          tapply(USA.data$Rape, clu.lab,mean),
          tapply(USA.data$UrbanPop, clu.lab,mean)))
row.names(cluster.summary) <- c("Murder", "Assault","Rape","UrbPop")

cluster.summary
