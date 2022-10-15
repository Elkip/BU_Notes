library(aod) # inlcudes function wald.test()
library(lmtest)
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
log.full <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, family=binomial, data = lung)
log.adjusted <- glm(Y ~ X2 + X3 + X4 + X5 + X6 + X7, family=binomial, data = lung)
summary(log.adjusted_full)
anova(log.adjusted, log.adjusted_full)
pchisq(5.281,1,lower.tail = F)

# b. OR and CI
exp(1.068)
exp(1.068 - 1.96 * .47896)
exp(1.068 + 1.96 * .47896)

# 3
# Crude analysis
wald.test(b = coef(log.crude), Sigma = vcov(log.crude), Terms = 2)
# Adjusted analysis
confint.default(log.adjusted_full)
exp(cbind(OR = coef(log.adjusted_full), confint.default(log.adjusted_full)))
wald.test(b = coef(log.adjusted_full), Sigma = vcov(log.adjusted_full), Terms = 2)
# Confounding analysis
exp(coef(log.crude)["X1"])/exp(coef(log.adjusted_full)["X1"])

# 4
# Association between cell type and odds of death
log.full_no_celltype <- glm(Y ~ X1 + X5 + X6 + X7, family=binomial, data = lung)
log.adjusted_no_celltype <-  glm(Y ~ X5 + X6 + X7, family=binomial, data = lung)
lrtest(log.adjusted_no_celltype, log.adjusted)
anova(log.adjusted_no_celltype, log.adjusted)
lrtest(log.full_no_celltype, log.full)
anova(log.full_no_celltype, log.full)

# 5 
# Logistic Model X1-X8
log.full_x8 <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, family=binomial, data = lung)

exp(-0.079387*(60-45))
exp(-0.079387-1.96 * 0.015648 *(60-45))
exp(-0.079387+1.96 * 0.015648 *(60-45))