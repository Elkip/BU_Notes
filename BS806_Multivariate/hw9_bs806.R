# 1.
# Simulate 300 observations with 4 variables from 3 clusters
set.seed(1)
# Cluster 1
c1_x1 <- rnorm(100, mean = 0, sd = 1)
c1_x2 <- rnorm(100, mean = 2.5, sd = 1)
c1_x3 <- rnorm(100, mean = -2.5, sd = 1)
c1_x4 <- rnorm(100, mean = 1, sd = 1)
c1 <- data.frame(x1 = c1_x1, x2 = c1_x2, x3 = c1_x3, x4 = c1_x4)

# Cluster 2
c2_x1 <- rnorm(100, mean = -2.5, sd = 1)
c2_x2 <- rnorm(100, mean = 0, sd = 1)
c2_x3 <- rnorm(100, mean = 1, sd = 1)
c2_x4 <- rnorm(100, mean = 2.5, sd = 1)
c2 <- data.frame(x1 = c2_x1, x2 = c2_x2, x3 = c2_x3, x4 = c2_x4)

# Cluster 3
c3_x1 <- rnorm(100, mean = 2.5, sd = 1)
c3_x2 <- rnorm(100, mean = -2.5, sd = 1)
c3_x3 <- rnorm(100, mean = 1, sd = 1)
c3_x4 <- rnorm(100, mean = 2.5, sd = 1)
c3 <- data.frame(x1 = c3_x1, x2 = c3_x2, x3 = c3_x3, x4 = c3_x4)

# Combined clusters
c <- rbind(c1, c2, c3)
# Standardized
c_scaled <- scale(c)

# 2 Generate PCA of the variables
pca <- prcomp(c, scale. = T)
pca

plot(pca, type = "l")
plot(pca$x[,1], pca$x[, 2])

total_var_pca <- 1.449281^2 + 1.062586^2 + 0.728718^2 + 0.489353^2
first2_var_pca <- 1.449281^2 + 1.062586^2
per2_var_pca <- first2_var_pca / total_var_pca
per2_var_pca 
first3_var_pca <- 1.449281^2 + 1.062586^2 + 0.728718^2
per3_var_pca <- first3_var_pca / total_var_pca
per3_var_pca

# 3 
# a. Analyze WSS for different numbers of clusters
wss <- c()
for (i in 1:3) {
  km <- kmeans(c, i)
  wss <- c(wss, km$tot.withinss)
}
plot(c(1:3), wss, type="l")
# b. Generate cluster centroids
km$centers

# c. Visualize the cluster with the first 2 PCs and color code clusters
plot(pca$x[,1], pca$x[, 2])
for(i in 1:3) {
  points(pca$x[which(km$cluster == i),1],pca$x[which(km$cluster == i),2],col=i)
}

# 4
# Use hierarchical clustering with complete linkage to generate a dendrogram
hc <- hclust(dist(c_scaled), method = "complete")
plot(hc)

# 5
# Use the resampling technique to decide whether there are evident clusters
c.length <- c(); rand.tree.c <- c()
for(i in 1:10) {
  test <-  sample(c_scaled[,1])
  for(ind.c in 2:ncol(c_scaled)){
    test <- cbind(test, sample((c_scaled[,ind.c])))
  }
  
  ## using permuted data to construct hierarchical clustering
  ## then save its $height, i.e. [[2]]
  ## rand.tree.h is matrix with $height, each column corresponds to each permutation
  rand.tree <- hclust(dist(test))
  rand.tree.c <- cbind(rand.tree.c, rand.tree[[2]])
  
  ## c.length: quantiles value of $height based on permuted/ref data
  ## each row corresponds to each permutation
  ## each column corresponds to different quarantines, here 0.95
  c.length <- rbind(c.length,quantile(rand.tree[[2]],probs=c( 0.99)))
}
head(c.length)

rand.tree.ref <- apply(rand.tree.c, 1, mean)
quantile(rand.tree.ref, prob = c(0.99))
threshold <- apply(c.length, 2, mean)
threshold

# QQ Plot
real.tree.c <- hclust(dist(c_scaled))
plot(rand.tree.c[0:299], real.tree.c[[2]])
abline(0,1)

# 99% Tile Detection
plot(hc)
rect.hclust(hc, h=threshold)

# 6
# Generate a heatmeap of the data

