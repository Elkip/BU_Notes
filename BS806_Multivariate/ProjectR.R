#project 806
library(tidyverse)
library(corrplot)
library(leaps)

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

# # Finding the most significant predictors using FWD aic
# predct <- ~fix+vol+suger+chlor+freeSD+density+ph+abv+quality+citric+sulfate+color #model w/o the insig vars
# baseMod <- lm(abv~1)
# aicFwd <- step(baseMod, scope = predct,direction = "forward", k=2, trace = F)
# summary(aicFwd)$coefficients #the model

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








