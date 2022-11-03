library(tree)

fhs <- read.csv("/home/elkip/Datasets/FHS_data.csv")
fhs <- na.omit(fhs)
# 1
# 1.2 Var = Residual mean deviance
var = 15690000 / 2533
sd.est = sqrt(var)

# 1.3 Simulating data
node1 = fhs[fhs$SEX < 1.5 & fhs$AGE < 48.5,]
node2 = fhs[fhs$SEX < 1.5 & fhs$AGE >= 48.5,]
node3 = fhs[fhs$SEX > 1.5 & fhs$AGE < 51.5,]
node4 = fhs[fhs$SEX > 1.5 & fhs$AGE >= 51.5,]
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
mean(fhs$FVC)
mean(rand.total)
sd(fhs$FVC)
sd(rand.total)
mean(rand.node1)
mean.node1
mean(rand.node2)
mean.node2
mean(rand.node3)
mean.node3
mean(rand.node4)
mean.node4

# 1.5 Simulated FVC 
par(mfrow=c(1,1))
sim.fvc1 = data.frame(FVC = rand.node1, SEX = node1$SEX, AGE = node1$AGE, 
                      Smoke = node1$Smoke, SPF = node1$SPF, T2D = node1$T2D)
sim.fvc2 = data.frame(FVC = rand.node2, SEX = node2$SEX, AGE = node2$AGE, 
                      Smoke = node2$Smoke, SPF = node2$SPF, T2D = node2$T2D)
sim.fvc3 = data.frame(FVC = rand.node3, SEX = node3$SEX, AGE = node3$AGE, 
                      Smoke = node3$Smoke, SPF = node3$SPF, T2D = node3$T2D)
sim.fvc4 = data.frame(FVC = rand.node4, SEX = node4$SEX, AGE = node4$AGE, 
                      Smoke = node4$Smoke, SPF = node4$SPF, T2D = node4$T2D)
sim.fvc = rbind(sim.fvc1, sim.fvc2, sim.fvc3, sim.fvc4)

sim.tree = tree(FVC ~ ., data = sim.fvc, control=tree.control(nobs=nrow(sim.fvc), mindev = 0.001))
set.seed(1)
sim.tree <- cv.tree(sim.tree, best = 4)
plot(sim.tree)
text(sim.tree, pretty = 0, cex = .8)

# 2a
fhs.tree <- tree(as.factor(dth) ~ ., data = fhs)
fhs.tree
summary(fhs.tree)
plot(fhs.tree)
text(fhs.tree, pretty = 0)

# 2d
set.seed(1)
train <- sample(c(1:nrow(fhs)), 2*nrow(fhs)/3)
training <- fhs[train,]
train_tree <- tree(as.factor(dth) ~ ., data = training)
train_tree = prune.tree(train_tree, method = c("misclass"), best = 4)
test <- fhs[-train,]
preds <- predict(train_tree, newdata = test, type = "class")
plot(train_tree)
text(train_tree)
table(preds, test$dth)

# 3 Bonus Q
library(rpart)
library(rpart.plot)
tree.0 <- rpart( as.factor(dth) ~ SEX+AGE+Smoke+FVC+SPF+T2D, method="class", data=fhs)
rpart.plot(tree.0)