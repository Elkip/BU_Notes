lung <- read.table("/home/elkip/Datasets/Lungca.txt",header=T, na.strings=c("."))

# 1
# a. Basic Logistic Regression 
log.crude <- glm(Y ~ X1, family=binomial, data = lung)
summary(log.crude)

# b. Odds ratio  = b1
exp(0.6875063)

# c. Odds ratio CI
exp(.6875 + 1.96 * .3486)
exp(.6875 - 1.96 * .3486)

# 2 
# a. Logistic Regression with columns 1:7
log.adjusted_full <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, family=binomial, data = lung)
log.adjusted <- glm(Y ~ X2 + X3 + X4 + X5 + X6 + X7, family=binomial, data = lung)
summary(log.adjusted_full)
anova(log.adjusted, log.adjusted_full)
pchisq(5.281,1,lower.tail = F)

# b. OR and CI
exp(1.068)
exp(1.068 - 1.96 * .47896)
exp(1.068 + 1.96 * .47896)