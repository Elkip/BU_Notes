library(tree)

fhs <- read.csv("/home/elkip/Datasets/FHS_data.csv")
fhs <- na.omit(fhs)

# 1
# 1.2 Var = Residual mean deviance
var = 15690000 / 2533
sd.est = sqrt(var)
mean.node1 = 574.8
mean.node2 = 512.3
mean.node3 = 444.9
mean.node4 = 366.6

n.node1 = fhs[fhs$SEX == 1 & fhs$AGE < 48.5,]
n.node2 = fhs[fhs$SEX == 1 & fhs$AGE >= 48.5,]
n.node3 = fhs[fhs$SEX == 2 & fhs$AGE < 51.5,]
n.node4 = fhs[fhs$SEX == 2 & fhs$AGE >= 51.5,]

set.seed(1)
rand.node1 <- rnorm(n = nrow(n.node1), mean = mean.node1, sd=sd.est)
rand.node2 <- rnorm(n = nrow(n.node2), mean = mean.node2, sd=sd.est)
rand.node3 <- rnorm(n = nrow(n.node3), mean = mean.node3, sd=sd.est)
rand.node4 <- rnorm(n = nrow(n.node4), mean = mean.node4, sd=sd.est)
rand.total <- c(rand.node1, rand.node2, rand.node3, rand.node4)

hist(rand.total)
hist(fhs$FVC)

hist(n.node4$FVC)
hist(rand.node4)

# 2 
fhs.tree <- tree(dth ~ ., data = fhs)
summary(fhs.tree)
plot(fhs.tree)
text(fhs.tree, pretty = 0)
