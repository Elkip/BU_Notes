#project 806
library(tidyverse)
library(corrplot)
library(leaps)
library(car)
library(tree)
library(pROC)
library(lsmeans)
library(MASS)

#import red and white
WhiteWine <- read_delim("C:/Users/julia/OneDrive/Desktop/BU/BS806/Final Project BS806/winequality-white.csv")
RedWine   <- read_delim("C:/Users/julia/OneDrive/Desktop/BU/BS806/Final Project BS806/winequality-red.csv")

#rename vars
names <- c("fix","vol","citric","sugar","chlor",
           "freeSD","totSD","density","ph","sulfate","abv","quality")
colnames(WhiteWine) <- names
colnames(RedWine) <- names

#adds variable for red/white
WhiteWine$color <- 0
RedWine$color <- 1
orgWine <- rbind(RedWine,WhiteWine) #combine them 
orgWine <- na.omit(orgWine) #no missing data
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
#fix+vol+sulfate+sugar+chlor could use log transformation

logWine <- orgWine
logWine$fix<-log(logWine$fix)
logWine$vol<-log(logWine$vol)
logWine$sugar<-log(logWine$sugar)
logWine$chlor<-log(logWine$chlor)
attach(logWine)

#is there is significant dif between red and white
mod    <- lm(abv ~ ., logWine)
modOne <- lm(abv ~ 1)
anova(mod, modOne) # at least one predictor is associated 
summary(mod) # all vars are significant

# simple linear regression for each variable
# names1 <- c(names,"color")
# for(i in names){
#   baseMod <- summary(lm(abv~get(i)))
#   if(baseMod$coefficients[,4][2] >= 0.05/length(names1)){
#     print(list(i,baseMod$coefficients[,4][2]))}
# } #the vars citric sulfate vol and color are insignificant
# #color is VERY close tho @ 0.0079

#correlations plot
par(mfrow=c(1,1))
corrplot(cor(logWine), method="color", order="hclust")

# Finding the most significant predictors using stepwise aic
predct <- ~fix+vol+sugar+chlor+freeSD+density+ph+abv+quality+citric+sulfate+color #model w/o the insig vars
baseMod <- lm(abv~1)
aicFwd <- step(baseMod, scope = predct,direction = "forward", k=2, trace = F)
summary(aicFwd)$coefficients #the model

# fwd bic model
n <- nrow(logWine)
bicFwd <- step(baseMod,scope = predct, direction = "forward", k=log(n), trace = F)
summary(bicFwd)$coefficients #the model

#visual inpection not finished
leaps <- regsubsets(abv~fix+vol+sugar+chlor+freeSD+density+ph+abv+quality+citric+sulfate+color, logWine)
re <- summary(leaps)
plot(re$cp)
abline(0,1)
plot(re$adjr2, main = "R-squared adjusted by Number of Predictors", 
     ylab = "R-squared Adjusted", 
     xlab = "Number of Predictors")
re$adjr2[5]
re

# final model
finalmod<-lm(abv ~ fix + sugar + density + ph + color, data = logWine)
summary(finalmod)

#this looks for outliers
#pretty pictures ....
par(mfrow = c(1,1))
plot(fitted(finalmod),residuals(finalmod))
qqnorm(residuals(finalmod))
qqline(residuals(finalmod))
hist(residuals(finalmod))
#qqplot(residuals(finalmod)) #for some reason this isnt working

# 12 cols
#studentized residuals
pprim <- 5 #this is from the model
studRez <- rstudent(finalmod)
roes <- length(studRez)
outliers <- abs(studRez) > abs(qt(0.5/((roes+1)*2),df = roes - pprim- 1, lower.tail = F))
outliers[outliers==TRUE]

# cooks distance sponsored by @gordonramsey
cooksD <- cooks.distance(finalmod)
roes <- nrow(logWine)
looksy <- cooksD[cooksD > 4/n]
looksy

#hat values sponsored by @newera
hatVals <- hatvalues(finalmod)
threshold <- 2*pprim/roes
leveragePTS <- hatVals[hatVals > threshold]
leveragePTS #these are the potential leverage pts

#influence pts
influenceIndexPlot(finalmod)

# residual plots
par(mfrow=c(2,2))
plot(finalmod)

# remove outliers until residuals are under control
logWine <- logWine[-4381,]
logWine <- logWine[-3263,]
logWine <- logWine[-3253,]
# logWine <- logWine[-5498,]
# logWine <- logWine[-5220,]
# logWine <- logWine[-5216,]

finalmod<-lm(abv ~ fix + sugar + density + ph + color, data = logWine)
summary(finalmod$residuals)
finalmod$residuals[finalmod$residuals>4]

pprim <- 5 #this is from the model
studRez <- rstudent(finalmod)
roes <- length(studRez)
outliers <- abs(studRez) > abs(qt(0.5/((roes+1)*2),df = roes - pprim- 1, lower.tail = F))
outliers[outliers==TRUE]
summary(outliers)

# cooks distance sponsored by @gordonramsey
cooksD <- cooks.distance(finalmod)
roes <- nrow(logWine)
looksy <- cooksD[cooksD > 4/n]
length(looksy)

#hat values sponsored by @newera
hatVals <- hatvalues(finalmod)
threshold <- 2*pprim/roes
leveragePTS <- hatVals[hatVals > threshold]
length(leveragePTS) #these are the potential leverage pts

# accuracy of model
plot(finalmod)
summary(finalmod)
par(mfrow=c(1,1))
hist(summary(finalmod)$residuals,
     main = "Histogram of Residuals", 
     xlab = "Residual Value",
     ylab = "Frequency")

# revisit difference between red and white ABV
lsmeans(finalmod,~color)

#pca stuff
#this doesnt explain much
mod <- prcomp(logWine, scale. = T)
summary(mod)
pc1<-mod$x[,1]
pc2<-mod$x[,2]
pc3<-mod$x[,3]
pc4<-mod$x[,4]
pc5<-mod$x[,5]
pc6<-mod$x[,6]
summary(lm(logWine$abv~pc1+pc2+pc3+pc4+pc5))

# classification analysis
logWine <- orgWine
logWine$fix<-log(logWine$fix)
logWine$vol<-log(logWine$vol)
logWine$sugar<-log(logWine$sugar)
logWine$chlor<-log(logWine$chlor)

logWine$qual <- as.numeric(logWine$quality>=6)
classdata <- logWine[,colnames(logWine)!="quality"]

# two proportion z test
wqual <- classdata[classdata$color==0,]
rqual <- classdata[classdata$color==1,]

prop.test(x = c(sum(wqual$qual), sum(rqual$qual)), n = c(nrow(wqual), nrow(rqual)))

# partition data into training and testing
classdata$qual <- as.factor(classdata$qual)
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

# assess accuracy
pred <- predict(classmod1, newdata = classdata, type = "response")

log.roc<-roc(classdata$qual, pred)
plot(log.roc)

cm <- table(classdata$qual, pred > 0.75)
cm

misclass <- (cm[1,2]+cm[2,1])/sum(cm)
misclass

accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
accuracy

sens <- cm[2,2]/sum(cm[2,])
sens

spec <- cm[1,1]/sum(cm[1,])
spec

ppv <- cm[2,2]/sum(cm[,2])
ppv

npv <- cm[1,1]/sum(cm[,1])
npv


ggplot(orgWine, aes(x=quality, fill = as.factor(color)), xlim=c(0, 10))+
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')

# data characteristics
sum(as.numeric(is.na(WhiteWine))) # no missing values
sum(as.numeric(is.na(RedWine))) # no missing values

sum(as.numeric(logWine$quality>=8))/nrow(logWine)
sum(as.numeric(logWine$quality<=3))/nrow(logWine)

