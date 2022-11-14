library(survival)
lung <- read.table("/home/elkip/Datasets/Lungca.txt",header=T, na.strings=c("."))

# 1. Plot the Kaplan-Meier curves for treatment groups
# X9 - time to event, Y - censoring variable, X1 - treatment variable
fit.1 <- survfit(Surv(X9, Y) ~ X1, data=lung)
summary(fit.1)
plot(fit.1, col=c(1,2), lwd=2, ylim=c(0,1),
     xlab="Time (Days)", ylab="Disease free survival", cex.axis=1.5, cex.lab=1.5)
legend(x=1, y=0.40, legend=c("Treatment 0","Treatment 1"),
       col=c(1,2), lwd =2, cex=1.2)

# Log-Rank Test
survdiff(Surv(X9, Y) ~ X1, data=lung)

# 2. Use the Cox Proportional Hazard Model for the following
# a. Fit model adjusting for X1-X8
### Adjusted Model
fit.2 <- coxph(Surv(X9, Y) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=lung)
summary(fit.2)
scho <- cox.zph(fit.2)
scho
par(mfrow = c(4,2))
plot(scho)

# c. Confounding in X2-X8
abs(1.4760 - 1.5675) / 1.4760

# d. Which predictors are significantly associated with survival
summary(fit.2)
fit.3 <- coxph(Surv(X9, Y) ~ X1 + X5 + X6 + X7 + X8, data=lung)
summary(fit.3)

# 3
# b
fit.4 <- coxph(Surv(X10, X11) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=lung)
summary(fit.4)

# c 
fit.5 <- coxph(Surv(X10, X11) ~ X1, data=lung)
summary(fit.5)

# d
fit.6 <- coxph(Surv(X10, X11) ~ X1 + X5 + X6 + X7 + X8, data=lung)
summary(fit.6)
