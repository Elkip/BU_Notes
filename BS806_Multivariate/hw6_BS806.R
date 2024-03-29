usa <-  read.csv("/home/elkip/Datasets/divusa.csv", header = T)

# 1a
# Backwards Elimination
reg_full <- lm(divorce ~ ., data = usa)
summary(reg_full)

# model - unemployment
reg_adj1 <- lm(divorce ~ . - unemployed, data = usa)
summary(reg_adj1)

# 1b
# AIC

# first perform forward selection
forward <- ~ year +  unemployed + femlab + marriage + birth + military
m0 <- lm(divorce ~ 1, data = usa)
reg.forward.AIC <- step(m0, scope = forward, direction = "forward", k = 2)
n <- nrow(usa)

# AIC = n*log(RSS/n) + 2p'
aov(lm(divorce ~ femlab + birth + marriage + year + military, data=usa))
n*log(162.1228/n)+2*6            # 69.33025
extractAIC(reg.forward.AIC, k=2) # 69.33027

## final model using AIC
summary(reg.forward.AIC)$coefficients

# 1c
# BIC
reg.forward.BIC <- step(m0, scope = forward, direction = "forward", k = log(n))
extractAIC(reg.forward,k=log(n))		# 83.3931
# BIC = n*log(RSS/n) + p'*log*n)
n*log(162.1228/n)+6*log(n)          # 83.39308
summary(reg.forward.BIC)$coefficients

# 1d
# Adjusted R^2
summary(reg_full)$adj.r.squared
summary(reg_adj1)$adj.r.squared

# 1e
# Mallows C_p
library(leaps)
leaps <- regsubsets(divorce ~ ., data = usa)
rs <- summary(leaps)
par(mfrow=c(1,2))
plot(2:7, rs$cp, xlab="No. of parameters",
     ylab="Cp Statistic")
abline(0,1)

plot(2:7, rs$adjr2, xlab="No. of parameters",
     ylab="Adjusted R-Squared")
abline(0,1)

rs