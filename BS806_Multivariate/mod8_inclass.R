a <- read.csv("/home/elkip/Datasets/heptathlon.csv")
hep <- a[,-1]
rownames(hep) <- a[,1]
head(hep)

# 1
hep$hurdles = max(hep$hurdles) - hep$hurdles
hep$run200m = max(hep$run200m) - hep$run200m
hep$run800m = max(hep$run800m) - hep$run800m

# 2
summary(hep)
plot(hep)
round(cor(hep),2)

# 3
hep = hep[!(row.names(hep) %in% "Launa (PNG)"),]
plot(hep)
round(cor(hep),2)

# 4
# a
pca1 <- prcomp(hep, scale = T)
pca1$x[,1]
summary(pca1)

# b 

