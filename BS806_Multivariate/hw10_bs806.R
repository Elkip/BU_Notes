hap <- read.csv("/home/elkip/Datasets/Somerville_Happiness.csv")

# 1. Randomly Select 100 observations for training and the remaining for testing
set.seed(2)
trn <- sample(1:nrow(hap), 100)
hap_trn <- hap[trn,]
hap_tst <- hap[-trn,]

# 2. Conduce classification Analysis then identify the best classifier on AUC
# a. Logistic Regression
hap.log <- glm(D ~., data = hap_trn, family=binomial)
summary(hap.log)
pred.log <- predict(hap.log, hap_tst, type = "response")

library(pROC)
auc(hap_tst$D, pred.log)

# b. Classification Tree
library(tree)
hap.tree <- tree(as.factor(D) ~ ., data = hap_trn, 
                 control = tree.control(nobs = nrow(hap_trn), mindev = .001))
summary(hap.tree)
plot(hap.tree)
text(hap.tree, pretty = 0, cex = 1)

hap.tree_list <- cv.tree(hap.tree, FUN = prune.misclass, K = 3)
hap.tree1 <- prune.tree(hap.tree, method = c("misclass"), best = 3)

# pred.tree <- predict(hap.tree2, newdata = hap_tst, type = "class")
#table(hap_tst$D, pred.tree)
#table(hap_tst$D,pred.tree)/
#  apply(table(hap_tst$D,pred.tree),1,sum)

pred.tree <- predict(hap.tree1, newdata = hap_tst)
tree.roc <- roc(hap_tst$D, pred.tree[,2])
plot(tree.roc)
tree.roc

# c. Linear Discriminant Analysis
library(MASS)
hap.lda <- lda(D ~ ., data=hap_trn)
hap.lda
plot(hap.lda)

pred.lda <- predict(hap.lda, hap_tst)
table(hap_tst$D, pred.lda[[1]])

lda.roc <- roc(hap_tst$D, pred.lda[[2]][,2])
plot(lda.roc)
lda.roc

# d. Quadratic Discriminant Analysis
hap.qda <- qda(D ~ ., hap_trn)
hap.qda

pred.qda <- predict(hap.qda, hap_tst)
table(hap_tst$D, pred.qda[[1]])

qda.roc <- roc(hap_tst$D, pred.qda[[2]][,2])
plot(qda.roc)
qda.roc

# 3. Perform analysis with the best method using the whole dataset 
hap.best <- glm(D ~ ., data = hap, family = binomial)
pred <- predict(hap.best, newdata = hap, type = "response")
tbl <- table(hap$D, pred > .5)
# a. Construct a confusion matrix
tbl
# accuracy
acc = (tbl[1,1]+tbl[2,2])/sum(tbl)
mis = 1 - acc
# b. Specificity and sensitivity
sens = tbl[2,2]/sum(tbl[,2])
spec = tbl[1,1]/sum(tbl[,1])
# c. pos and neg pred value
pos = tbl[2,2] / sum(tbl[2,])
neg = tbl[1,1] / sum(tbl[1,])
