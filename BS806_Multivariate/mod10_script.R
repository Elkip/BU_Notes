library(tree)
library(ISLR) 
library(alr4)
library(pROC)

#### read in the default dataset
Default<- read.csv("Default.csv", header=T)
#summary(Default)

#### data exploration.
attach(Default)
plot(balance,income)
points(balance[which(default=="No")],income[which(default=="No")], col="cyan")
points(balance[which(default=="Yes")],income[which(default=="Yes")], col="orange")

### Generate training and testing datasets to be used
### randomly select 2/3 of sample to be training
set.seed(2)
  train <- sample( c(1:nrow(Default)), 2*nrow(Default)/3)
  Default.train <- Default[train,]
  summary(Default.train)
### the remaining 1/3 of sample to be testing dataset
  Default.test <- Default[-train,]
  summary(Default.test)

#### logistic regression
mod.f <- glm(default ~.,data = Default.train, family=binomial)
summary(mod.f)

# Model selection using stepwise selection with BIC
best.model <- step(mod.f, k=log(nrow(Default.train)))

### prediction and classification
### prediction with testing dataset
pred.logistic <- predict(best.model, newdata=Default.test, type="response" )
plot(Default.test$balance, pred.logistic, type="n")
points(Default.test$balance[which(Default.test$student == "Yes")], 
         pred.logistic[which(Default.test$student == "Yes")],col=2)
points(Default.test$balance[which(Default.test$student == "No")], 
         pred.logistic[which(Default.test$student == "No")],col=1)
legend("topleft",c("Students","Non-students"),col=c(1,2), pch=1)

## classification based probability > 0.5
cbind(Default.test, pred.logistic)[1:5,]
table(Default.test$default,pred.logistic>0.5)
table(Default.test$default,pred.logistic>0.5)/apply(table(Default.test$default,pred.logistic>0.5),1,sum)

### ROC Analysis
install.packages("pROC")
library(pROC)
logistic.roc <- roc(Default.test$default,pred.logistic)
plot(logistic.roc)	## to plot ROC curve
logistic.roc	## to get summary

roc.analysis <- ci.thresholds(logistic.roc) 
names(roc.analysis)	# sensitivity and specificity
plot(logistic.roc)
plot(roc.analysis)

table(Default.test$default,pred.logistic> 0.05)
table(Default.test$default,pred.logistic>0.05)/apply(table(Default.test$default,pred.logistic>0.05),1,sum)


############### Classification Tree
## construct the classification tree
install.packages("tree")
library(tree)
tree.1 <- tree(default ~ ., data = Default.train, 
		control=tree.control(nobs=nrow(Default.train), mindev = 0.001) )
summary (tree.1)
#plot (tree.1)
#text(tree.1 ,pretty =0, cex=1)
## use k-fold cross-validation to determine the best tree
set.seed(1)
tree.list <- cv.tree(tree.1, FUN=prune.misclass, K=3)
tree.list

tree.2 <- prune.tree(tree.1, method = c("misclass"), best=3)
summary(tree.2)
plot (tree.2)
text(tree.2 ,pretty =0, cex=1)
tree.2

pred.tree <- predict(tree.2, newdata=Default.test, type="class")
table(Default.test$default,pred.tree)
table(Default.test$default,pred.tree)/
		apply(table(Default.test$default,pred.tree),1,sum)

### without type="class" argument to get probability
pred.tree <- predict(tree.2, newdata=Default.test)
plot(Default.test$balance, pred.tree[,2])

### can we do better with ROC
#install.packages("pROC")
library(pROC)
tree.roc <- roc(Default.test$default,pred.tree[,2])
plot(tree.roc)
tree.roc

## calculate the confidence interval using bootstrap
roc.analysis <- ci.thresholds(tree.roc) 
roc.analysis 
plot(tree.roc)		# ROC cuve
plot(roc.analysis)	# ROC curve with confidence interval

pred.tree <- predict(tree.2, newdata=Default.test)
table(Default.test$default,pred.tree[,2]> 0.10)
table(Default.test$default,pred.tree[,2]> 0.10)/
	apply(table(Default.test$default,pred.tree[,2]> 0.10),1,sum)


##### KNN (k nearest neighbor)
install.packages("class")
library(class)

## all data must be numeric
levels(Default.test$student) <- c(0,1)
levels(Default.train$student) <- c(0,1)
new.train <- cbind(as.numeric(as.character(Default.train$student)),
				Default.train[, c( "balance","income")])
new.test <- cbind(as.numeric(as.character(Default.test$student)),
				Default.test[, c( "balance","income")])

## apply KNN   
predict.knn <- knn( new.train, new.test, 
			Default.train[, c( "default" )], k=3)
table(Default.test$default,predict.knn)


## choose k by ROC analysis
roc.knn <- c()
for(k in 2:10){
	predict.knn <- knn(new.train, new.test, 
					Default.train[,c( "default" )], k=k)
     	tab <- table(Default.test$default,predict.knn)
   	roc.knn <- rbind(roc.knn, c(k, tab[1,1]/table(Default.test$default)[1],
                                   tab[2,2]/table(Default.test$default)[2]))}
roc.nn <- as.data.frame(roc.knn)
names(roc.knn) <- c("K", "Specificity", "Sensitivity")
roc.knn

### it's important to standardize the variables
z.train <- scale(cbind(as.numeric(as.character(Default.train$student)),
				Default.train[, c( "balance","income")]))
z.test <- scale(cbind(as.numeric(as.character(Default.test$student)),
				Default.test[, c( "balance","income")]))   
predict.knn <- knn( z.train,z.test, Default.train[, c( "default" )], k=3)
table(Default.test$default,predict.knn)

table(Default.test$default,predict.knn)/
		apply(table(Default.test$default,predict.knn),1,sum)

## choose k by ROC analysis
roc.knn <- c()
for(k in 2:10){
    	predict.knn <- knn(z.train,z.test, Default.train[,c("default")], k=k)
   	table(Default.test$default,predict.knn)
   	roc.knn <- rbind(roc.knn, c(k, 
			table(Default.test$default,predict.knn)[1,1]/
				table(Default.test$default)[1],
            	table(Default.test$default,predict.knn)[2,2]/
				table(Default.test$default)[2]))}
roc.knn <- data.frame(roc.knn)
names(roc.knn) <- c("K", "Specificity", "Sensitivity")
roc.knn


################### discrminat analysis
#### linear discriminant analysis (equal variances)
### LDA with Single variables (balance)
library(MASS)
Bayes.rule <- lda(default~balance,data=Default.train)
Bayes.rule
plot(Bayes.rule)

# predict the outcome in the test set using the lda rule
predict.lda <- predict(Bayes.rule, Default.test)

## predict.lda[[1]] is $class and [[2]] is $posterior
table(Default.test$default,predict.lda[[1]])
#### if you like to get the proportion, remove the # sign below
#table(Default.test$default,predict.lda[[1]])/
#		apply(table(Default.test$default,predict.lda[[1]]),1,sum)

lda.roc <- roc(Default.test$default,predict.lda[[2]][,2])
#plot(lda.roc)
lda.roc
## if you like to get the confidence interval, remove # sign
#roc.analysis <- ci.thresholds(lda.roc) 
#roc.analysis 


### Quadratic Discriminant Analysis
## QDA with single variable, balance
Bayes.rule <- qda(default~balance,data=Default.train)
Bayes.rule

### prediction with test data
predict.qda <- predict(Bayes.rule, Default.test)
names(predict.qda)
predict.qda[[1]][1:3]
predict.qda[[2]][1:3,]
table(Default.test$default,predict.qda[[1]])
# if like to get proportion, remove the # sign
#table(Default.test$default,predict.qda[[1]])/
#		apply(table(Default.test$default,predict.qda[[1]]),1,sum)

## ROC analysis
qda.roc <- roc(Default.test$default,predict.qda[[2]][,2])
qda.roc
# plot(qda.roc)
## if need confidence interval in ROC, remove # sign
#roc.analysis <- ci.thresholds(qda.roc) 
#roc.analysis

########## multiple features
################ Naive Bayes Rule 
#### naive bayes with Multiple variables
#install.packages("naivebayes")
library(naivebayes) 
Bayes.rule <- naive_bayes(default~.,data=Default.train)
Bayes.rule

## prediction 
predict.nb <- predict(Bayes.rule, Default.test, type="class")
table(Default.test$default,predict.nb)
table(Default.test$default,predict.nb)/
		apply(table(Default.test$default,predict.nb),1,sum)

predict.nb <- predict(Bayes.rule, Default.test, type="prob")
nb.roc <- roc(Default.test$default,predict.nb[,2])
#plot(nb.roc)
nb.roc
roc.analysis <- ci.thresholds(nb.roc) 
roc.analysis 

### LDA with Multiple variables
Bayes.rule <- lda(default~.,data=Default.train)
Bayes.rule

predict.lda <- predict(Bayes.rule,  Default.test )
table(Default.test$default,predict.lda[[1]])
table(Default.test$default,predict.lda[[1]])/apply(table(Default.test$default,predict.lda[[1]]),1,sum)

lda.roc <- roc(Default.test$default,predict.lda[[2]][,2])
plot(lda.roc)
lda.roc
roc.analysis <- ci.thresholds(lda.roc) 
roc.analysis 



## QDA with Multiple variables
Bayes.rule <- qda(default~.,data=Default.train)
Bayes.rule

predict.qda <- predict(Bayes.rule,  Default.test )
table(Default.test$default,predict.qda[[1]])
table(Default.test$default,predict.qda[[1]])/apply(table(Default.test$default,predict.qda[[1]]),1,sum)

qda.roc <- roc(Default.test$default,predict.qda[[2]][,2])
plot(qda.roc)

qda.roc
roc.analysis <- ci.thresholds(qda.roc) 
roc.analysis
