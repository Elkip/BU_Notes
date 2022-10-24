dat <- read.csv("/home/elkip/Datasets/Heart.csv")
dat <- na.omit(dat)

set.seed(1)
train <- sample(c(1:nrow(dat)), 2*nrow(dat)/3)
training <- dat[train,]

tree1 <- tree::tree(AHD ~ Thal + ChestPain +  Ca + MaxHR , data = training)

plot(tree1)
text(tree1)

# 11 terminal nodes

set.seed(1)

tree.list <- cv.tree(tree1, FUN= prune.misclass, K=3)
plot(tree.list$k, tree.list$dev)
tree.list

tree2 <- prune.tree(tree1, method = c("misclass"), best = 7)
plot(tree2)
text(tree2)

test <- dat[-train,]
preds <- predict(tree2, newdata = test, type = "class")

table(preds, test$AHD)

false_pos <- 9/(44+9)
false_neg <- 8/(38+8)