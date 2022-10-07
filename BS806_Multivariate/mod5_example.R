# 1
trans <- read.table("/home/elkip/Datasets/transformations.csv", header=T, sep = ",")
head(trans)
reg <- lm(sbp ~ cfpwv, trans)
summary(reg)

# 2
par(mfrow=c(2,2))
plot(fitted(reg), residuals(reg), xlab="Fitted", ylab="Residuals")
qqnorm(residuals(reg), ylab="Residuals")
qqline(residuals(reg), ylab="resids")
library(car)
qqPlot(residuals(reg))
hist(reg$residuals)

# 3
shapiro.test(reg$residuals)
dev.off()

# 4
hist(trans$cfpwv)
hist(1/trans$cfpwv)

# 5 
reg2 <- lm(1/cfpwv ~ sbp , trans)
summary(reg2)

par(mfrow=c(2,2))
plot(fitted(reg2), residuals(reg2), xlab="Fitted", ylab="Residuals")
qqnorm(residuals(reg2), ylab="Residuals")
qqline(residuals(reg2), ylab="resids")
qqPlot(residuals(reg2))
hist(reg2$residuals)

# 6
n <- nrow(trans)
p <- 2

jake <- rstudent(reg2)
jake <- jake[which.max(abs(jake))]

qt(0.05/(n*2), df = n-p-1 , lower.tail=TRUE)  
yn.outlier <- abs(jake) > abs(qt(0.05/(n*2), df = n-p-1 , lower.tail=TRUE))
as.numeric(yn.outlier)
which(as.numeric(yn.outlier)==1)

# 7
cook <- cooks.distance(reg2)
n <- nrow(trans)

pprime <- 2

check <- cook[cook > 4/n]  # rule of thumb
sort(check, decreasing=TRUE) [1:5]  # list first five max

cook[cook>0.5]    # check Di>0.5
cook[(pf(cook, pprime, n-pprime)>0.5)] # use F-dist

influenceIndexPlot(reg2)
