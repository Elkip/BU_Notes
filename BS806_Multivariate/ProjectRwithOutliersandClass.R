#project 806
library(tidyverse)
library(corrplot)
library(leaps)
library(car)
library(tree)
library(pROC)


#import red and white
WhiteWine <- read_delim("/home/elkip/Datasets/winequality-white.csv")
RedWine   <- read_delim("/home/elkip/Datasets/winequality-red.csv")

#rename vars
names <- c("fix","vol","citric","suger","chlor",
           "freeSD","totSD","density","ph","sulfate","abv","quality")
colnames(WhiteWine) <- names
colnames(RedWine) <- names

#adds variable for red/white
WhiteWine$color <- 0
RedWine$color <- 1
orgWine <- rbind(RedWine,WhiteWine) #combine them 
orgWine <- na.omit(orgWine) #empty var handeling
attach(orgWine) #attachement issues #bigg mood

#tea test stratified by color
t.test(abv~color)

#prints plots for each var vs abv
par(mfrow=c(3,4))
# for(i in namess){ #this makes plots vs abv for all variables
#   plot(abv ~ log(get(i)), data = wine, xlab = i)}
for(i in names){ # this makes histograms for each var
  hist(get(i), xlab = i, main = "No logs")
  hist(log(get(i)), xlab = i, main = "with logs")}
detach(orgWine)
#fix+vol+sulfate+suger+chlor could use log transformation

logWine <- orgWine
logWine$fix<-log(logWine$fix)
logWine$vol<-log(logWine$vol)
logWine$suger<-log(logWine$suger)
logWine$chlor<-log(logWine$chlor)
attach(logWine)

#is there is significant dif between red and white
mod    <- lm(abv ~ ., logWine)
modOne <- lm(abv ~ 1)
anova(mod, modOne) # at least one predictor is associated 
summary(mod) # almost all the vars except color, totSD are significant

#linear regression for each and check if it is insignificant little bitch
names1 <- c(names,"color")
for(i in names){
  baseMod <- summary(lm(abv~get(i)))
  if(baseMod$coefficients[,4][2] >= 0.05/length(names1)){
    print(list(i,baseMod$coefficients[,4][2]))}
} #the vars citric sulfate vol and color are insignificant
#color is VERY close tho @ 0.0079

#correlations plot
par(mfrow=c(1,1))
corrplot(cor(logWine), method="color", order="hclust")

# Finding the most significant predictors using FWD aic
predct <- ~fix+vol+suger+chlor+freeSD+density+ph+abv+quality+citric+sulfate+color #model w/o the insig vars
baseMod <- lm(abv~1)
aicFwd <- step(baseMod, scope = predct,direction = "forward", k=2, trace = F)
summary(aicFwd)$coefficients #the model

# fwd bic model
n <- nrow(logWine)
bicFwd <- step(baseMod,scope = predct, direction = "forward", k=log(n), trace = F)
summary(bicFwd)$coefficients #the model

#visual inpection not finished
leaps <- regsubsets(abv~fix+vol+suger+chlor+freeSD+density+ph+abv+quality+citric+sulfate+color, logWine)
re <- summary(leaps)
plot(re$cp)
abline(0,1)
plot(re$adjr2)
re$adjr2[5]
re

#pca stuff
#this doesnt explain much so lets phugettaboutitt
par(mfrow=c(1,1))
mod <- prcomp(logWine, scale. = T)
pc1<-mod$x[,1]
pc2<-mod$x[,2]
pc3<-mod$x[,3]
pc4<-mod$x[,4]
summary(mod)
summary(lm(logWine$abv~pc1 + pc2 + pc3 + pc4))


#this looks for outliers
regres <- lm(abv~fix+vol+suger+chlor+freeSD+density+ph+quality+citric+sulfate+color, logWine)
summary(regres)
#pretty pictures ....
par(mfrow = c(1,1))
plot(fitted(regres),residuals(regres))
qqnorm(residuals(regres))
qqline(residuals(regres))
hist(residuals(regres))
#qqplot(residuals(regres)) #for some reason this isnt working

# 12 cols
#studentized residuals
pprim <- 12 #this is from the model
studRez <- rstudent(regres)
roes <- length(studRez)
outliers <- abs(studRez) > abs(qt(0.5/((roes+1)*2),df = roes - pprim- 1, lower.tail = F))
summary(outliers)

# cooks distance sponsored by @gordonramsey
cooksD <- cooks.distance(regres)
roes <- nrow(logWine)
looksy <- cooksD[cooksD > 4/n]
looksy

#hat values sponsored by @newera
hatVals <- hatvalues(regres)
threshold <- 2*pprim/roes
leveragePTS <- hatVals[hatVals > threshold]
leveragePTS #these are the potential leverage pts

#influence pts
influenceIndexPlot(regres)

#checking for change, but there is possibly too many???
myList <- c()
testges <- logWine
for (i in myList) {
  testges <- testges[-c(1),]
  regres1 <- lm(abv~fix+vol+suger+chlor+freeSD+density+ph+quality+citric+sulfate+color, testges)
  regres1 <- summary(regres1)$fstatistic
  pt <- c(i, regres1[1],regres1[2], regres1[3],pf(regres1[1],regres1[2],regres1[3],lower.tail = F))
  print(pt)
}

# classification analysis

logWine$qual <- as.factor(as.numeric(logWine$quality>=6))
logWine$qual <- as.factor(logWine$qual)
logWine$quality <- as.factor(logWine$quality)
classdata <- logWine[,colnames(logWine)!="quality"]

# partition data into training and testing
set.seed(2)
training <- sample(c(1:nrow(classdata)), 2*nrow(classdata)/3)
train <- classdata[training,]
test <- classdata[-training,]

# determine classification method

# logistic
mod.log <- glm(qual ~ ., data = train, family=binomial)
pred.log <- predict(mod.log, newdata=test, type="response" )
roc.log <- roc(test$qual,pred.log)

# classification tree
tree.1 <- tree(qual ~ ., data = train, 
               control=tree.control(nobs=nrow(train), mindev = 0.001) )

set.seed(1)
tree.list <- cv.tree(tree.1, FUN=prune.misclass, K=5)
tree.list

tree.2 <- prune.tree(tree.1, method = c("misclass"), best=3)
plot(tree.2)
text(tree.2)
pred.tree <- predict(tree.2, newdata=test)

roc.tree <- roc(test$qual,pred.tree[,2])

# linear discriminant analysis
mod.lda <- lda(qual~.,data=train)
pred.lda <- predict(mod.lda, test)
roc.lda <- roc(test$qual,pred.lda[[2]][,2])

# quadratic discriminant analysis
mod.qda <- qda(qual~.,data=train)
pred.qda <- predict(mod.qda, test)
roc.qda <- roc(test$qual,pred.qda[[2]][,2])

#compare
table<-matrix(c(roc.log$auc, roc.tree$auc, roc.lda$auc, roc.qda$auc),
              nrow = 1,
              dimnames=list(c("AUC"), c("Logistic", "Tree", "LDA", "QDA")))
table

# use logistic regression to classify wines
classmod <- glm(qual ~ ., data = classdata, family = "binomial")

classmod1 <- step(classmod, k = log(nrow(classdata)), trace = F)
summary(classmod1)

# use testing data set to assess accuracy
pred <- predict(classmod1, newdata = test, type = "response")

log.roc<-roc(test$qual, pred)
plot(log.roc)

cm <- table(test$qual, pred > 0.75)

misclass <- (cm[1,2]+cm[2,1])/sum(cm)
misclass

accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
accuracy

sens <- cm[2,2]/sum(cm[,2])
sens

spec <- cm[1,1]/sum(cm[,1])
spec

ppd <- cm[2,2]/sum(cm[2,])
ppd

npd <- cm[1,1]/sum(cm[1,])
npd
