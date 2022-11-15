---
title: "BS806 in class exercise 10"
author: "Jingzhe Han"
date: "2022-11-11"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

install.packages("flexclust")
library(flexclust)
```

# reading data
```{r}
nutrient <- read.csv("nutrient.csv")
head(nutrient)
row.names(nutrient) <- tolower(row.names(nutrient))

rownames(nutrient) <- nutrient[,1]
nutrient <- nutrient[,-1]
```
# 1.
```{r}
scale_nutrient <- apply(nutrient,2,scale)
scale_nutrient <- data.frame(scale_nutrient)
head(scale_nutrient)
```
# 2.
## a.
```{r}
hc <- hclust(dist(scale_nutrient), method = "average")
#km <- kmeans(scale_nutrient, 2)

plot(hc)
#rect.hclust(hc,k = 2)

#clu.lab <- cutree(hc, k = 2)
#table(km$cluster, clu.lab)
```

## b.
```{r}
p.length <- c()
rand.tree.h <- c()
for(i in 1:27){
  test <- sample(scale_nutrient[,1])
  for(ind.c in 2:ncol(scale_nutrient)){
    test <- cbind(test, sample((scale_nutrient[,ind.c])))
  }
  rand.tree <- hclust(dist(test))
  rand.tree.h <- cbind(rand.tree.h, rand.tree[[2]])
p.length <- rbind(p.length, quantile(rand.tree[[2]], probs = c(0.95)))
}

head(p.length)
```
```{r}
rand.tree.ref <- apply(rand.tree.h, 1, mean)
quantile(rand.tree.ref, prob = c(0.95))
threshold <- apply(p.length, 2, mean)
threshold
```
```{r}
real.tree <- hclust(dist((scale_nutrient)))
plot((rand.tree.ref),(real.tree[[2]]), xlab = "Expected", ylab = "Observed")
abline(0,1)
```

## c
```{r}
hc <- hclust(dist(scale_nutrient))
threshold
clu.lab = cutree(hc, h=threshold)

cluster.summary <- as.data.frame(
    rbind(tapply(scale_nutrient$energy, clu.lab,mean),
          tapply(scale_nutrient$protein, clu.lab,mean),
          tapply(scale_nutrient$fat, clu.lab,mean),
          tapply(scale_nutrient$calcium, clu.lab,mean),
          tapply(scale_nutrient$iron, clu.lab,mean)))
row.names(cluster.summary) <- c("energy", "protein","fat","calcium","iron")

cluster.summary
```

# 3.
```{r}
km <- kmeans(scale_nutrient, 3)
km
```
```{r}
km$withinss
km$totss
```



















































