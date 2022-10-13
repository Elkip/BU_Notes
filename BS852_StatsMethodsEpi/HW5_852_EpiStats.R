lung <- read.table("/home/elkip/Datasets/Lungca.txt",header=T, na.strings=c("."))

# 1. Basic Logistic Regression 
log.crude <- glm(Y ~ X1, family=binomial, data = lung)
summary(log.crude)

# b Odds ratio  = b1
exp(0.6875063)

# c Odds ratio CI
exp(.6875 + 1.96 * .3486)
exp(.6875 - 1.96 * .3486)