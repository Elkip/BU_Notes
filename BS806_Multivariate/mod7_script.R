
install.packages("tree")
library(tree)

### load the packages for the dataset Hitters
install.packages("ISLR")
library(ISLR)


## Data Exploration Before Analysis
hist(Hitters$Salary)
plot(Hitters$Hits, log(Hitters$Salary))
plot(Hitters$Years, log(Hitters$Salary))


## to predict 1987 salary based on Hits and Years
mod <- lm(log(Salary) ~ Hits + Years, data = Hitters)
summary(mod)

# model diagnostic
plot(mod)

install.packages("alr4")
library(alr4)
residualPlots(mod)

##### classification Tree
tree.1 <- tree(log(Salary) ~ Years + Hits, data = Hitters)
tree.1
summary(tree.1)
plot(tree.1)
text(tree.1, pretty= 0, cex =1.0)

## pruning
tree.2 <- prune.tree(tree.1, best = 3)
plot(tree.2)
text(tree.2, pretty = 0, cex = 1.5)
summary(tree.2)

## cross-validation
set.seed(1)
tree.list <- cv.tree(tree.1, FUN=prune.tree, K=5)
tree.list
plot(tree.list$size, tree.list$dev)

### select the best tree based on deviance with 4 leaves
tree.3 <- prune.tree(tree.1, best = 4)
summary(tree.3)

## Comparison between regression tree and linear model
### tree
tree.1 <- tree(log(Salary) ~ Years + Hits, data = Hitters)
tree.2 <- prune.tree(tree.1, best = 4)
plot(tree.2)
text(tree.2, pretty = 0, cex = 1.5)
tree.2
plot(na.omit(Hitters)$Salary,predict(tree.2))

### linear regression
## define dummy variable
Hitters$branch.2 <- 0; 
Hitters$branch.2[Hitters$Years >=3.5 &Hitters$Years<4.5] <- 1
Hitters$branch.3 <- 0; 
Hitters$branch.3[Hitters$Years >=4.5 &Hitters$Hits<117.5] <- 1
Hitters$branch.4 <- 0; 
Hitters$branch.4[Hitters$Years >=4.5 &Hitters$Hits>=117.5] <- 1
mod <- lm(log(Salary) ~ branch.2 + branch.3 + branch.4, data = Hitters)
summary(mod)
anova(mod)
table(mod$fitted.values)

## use CART for Prediction
set.seed(2)
train <- sample(c(1:nrow(Hitters)), 2*nrow(Hitters)/3)
tree.1 <- tree(log(Salary) ~ Years + Hits, data = Hitters[train,])
summary(tree.1)
tree.2 <- prune.tree(tree.1, best =4)

pred.salary <- predict(tree.2, newdata = Hitters[-train,])
plot(log(Hitters$Salary[-train]), pred.salary)


### example with heart data set
Heart <- read.csv("Heart.csv")
tree.1 <- tree(AHD ~., data = Heart)
summary(tree.1)

plot(tree.1)
text(tree.1, pretty = 0, cex=0.8)

## pruning by cross validation set.seed(1)
set.seed(1)
tree.list <- cv.tree(tree.1, FUN= prune.misclass, K=3)
tree.list
plot(tree.list$k, tree.list$dev)

tree.2 <- prune.tree(tree.1, method = c("misclass"), best = 8)
summary(tree.2)
plot(tree.2)
text(tree.2, pretty=0, cex= 0.8) # no abbreviation





