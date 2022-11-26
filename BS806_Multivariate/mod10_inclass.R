wine <- read.csv("/home/elkip/Datasets/Wine.csv")


inclass$Cultivar = as.factor(inclass$Cultivar)
is.factor(inclass$Cultivar)
head(inclass)

set.seed(1)
train <- sample (1:nrow(inclass), 120)
inclass.train <- inclass[train,]
summary(inclass.train)
inclass.train

inclass.test <- inclass[-train,]
(inclass.test)

set.seed(1)
z.train = data.frame( cbind(Cultivar = inclass.train[,1], scale(inclass.train[,2:9])))
z.train

z.test= data.frame( cbind(Cultivar = inclass.test[,1], scale(inclass.test[,2:9])))
z.test


#4a

library(class)
predict.KNN <- knn(z.train, z.test, inclass.train[, c("Cultivar")], k=3)
predict.KNN
a=table(inclass.test$Cultivar,predict.KNN)
sum(diag(a))/sum(a)



#4b
prop.table(table(inclass.test$Cultivar,predict.KNN), margin = 1)
#4c
predict.KNN1 <- knn(z.train, z.test, inclass.train[, c("Cultivar")], k=5)

c=table(inclass.test$Cultivar,predict.KNN1)
c
sum(diag(c))/sum(c)

prop.table(table(inclass.test$Cultivar,predict.KNN1), margin = 1)