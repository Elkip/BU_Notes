library(tree)

fhs <- read.csv("/home/elkip/Datasets/FHS_data.csv")
fhs <- na.omit(fhs)
# 1
# 1.2 Var = Residual mean deviance
var = 15690000 / 2533
sd.est = sqrt(var)

# 1.3 Simulating data
node1 = fhs[fhs$SEX == 1 & fhs$AGE < 48.5,]
node2 = fhs[fhs$SEX == 1 & fhs$AGE >= 48.5,]
node3 = fhs[fhs$SEX == 2 & fhs$AGE < 51.5,]
node4 = fhs[fhs$SEX == 2 & fhs$AGE >= 51.5,]
mean.node1 = mean(node1$FVC)
mean.node2 = mean(node2$FVC)
mean.node3 = mean(node3$FVC)
mean.node4 = mean(node4$FVC)

set.seed(1)
rand.node1 <- rnorm(n = nrow(node1), mean = mean.node1, sd=sd.est)
rand.node2 <- rnorm(n = nrow(node2), mean = mean.node2, sd=sd.est)
rand.node3 <- rnorm(n = nrow(node3), mean = mean.node3, sd=sd.est)
rand.node4 <- rnorm(n = nrow(node4), mean = mean.node4, sd=sd.est)
rand.total <- c(rand.node1, rand.node2, rand.node3, rand.node4)

# 1.4 Comparing the distributions of FVC Scores real vs simulated
par(mfrow=c(1,2))
hist(fhs$FVC, xlab = "FVC Score", main = "Real Data")
hist(rand.total, xlab = "FVC Score", main = "Simulated Data")

# 1.5 Simulated FVC 
par(mfrow=c(1,1))
sim.fvc = data.frame(FVC = rand.total, fhs$SEX, fhs$AGE, fhs$Smoke, fhs$SPF, fhs$T2D)
sim.tree = tree(FVC ~ ., data = sim.fvc, control=tree.control(nobs=nrow(sim.fvc), mindev = 0.001))
set.seed(1)
sim.list <- cv.tree(sim.tree, FUN = prune.misclass, K = 3)
plot(sim.tree)
text(sim.tree, pretty = 0, cex = .8)

# 2 
fhs.tree <- tree(dth ~ ., data = fhs)
summary(fhs.tree)
plot(fhs.tree)
text(fhs.tree, pretty = 0)
