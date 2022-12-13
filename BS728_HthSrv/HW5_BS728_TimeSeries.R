########################
# Nov 6, 2012          # 
# Code for HW #5       #
######################## 
# Data comes from the monthly number of accidental deaths in the US between 1973 and 1978

# read in the data
deaths <- read.csv("/home/elkip/Datasets/deaths.csv",header=T)
num.months <- length(deaths[,1])
# 1. How many months of data are there?
num.months

attach(deaths)

# 2. Plot the series
# Explain the parts of this code and what each piece is doing.
plot(deaths$num,type='b',pch=20,ylab="Deaths",xlab="Time",xaxt='n')
axis(1,at=c(0,12,24,36,48,60),labels=c("1972","1973","1974","1975","1976","1977"))

# 3. Investigate seasonality
plot(num~as.factor(num.month),xlab="Month")

# Remove seasonal components
period1 <- 12
period2 <- 6

# create sinusoid terms for the two periods you just created.
s1 <- sin(c(1:num.months)*2*pi/period1)
c1 <- cos(c(1:num.months)*2*pi/period1)
s2 <- sin(c(1:num.months)*2*pi/period2)
c2 <- cos(c(1:num.months)*2*pi/period2)

# Determine which sinusoids to keep. Justify your decision with graphics.
par(mfrow=c(1,2))
plot(s1, type='b')
plot(deaths$num,type='b',pch=20,ylab="Deaths",xlab="Time")
plot(s2, type = 'b')
plot(deaths$num,type='b',pch=20,ylab="Deaths",xlab="Time")
plot(c1, type = 'b')
plot(deaths$num,type='b',pch=20,ylab="Deaths",xlab="Time")
plot(c2, type = 'b')
plot(deaths$num,type='b',pch=20,ylab="Deaths",xlab="Time")

# Plot the original data with the fitted values from the chosen model overlaid in a 
#   different color.
par(mfrow=c(1,1))
fit1 <- lm(num~s1+c1)
plot(fit1$residuals,type='l')
lines(lin.reg2$fitted.values,col="blue",lwd=3)


# 4. FIT THE TREND
# Look at the residuals from the model that you just fit.  
resids <- fit1$residuals
plot(fit1$residuals)

#  Now, try to fit a linear trend using the time variable as the predictor. 
lin.reg <- lm(resids~time)
summary(lin.reg)
plot(lin.reg$residuals,type='l') 

lin.reg2 <- lm(resids~s1+c1)
plot(lin.reg2$residuals, type='l')
#  Now investigate if the trend is quadratic. Fit the appropriate model.
time2 <- time^2
qaud.reg <- lm(resids~time2)
summary(qaud.reg)
plot(qaud.reg$residuals, type='l')

quad.reg2 <- lm(resids~s1^2+c1^2)
plot(quad.reg2$residuals, type='l')
# Plot the residuals from the seasonally adjusted model. Then overlay a plot of the 
#  fitted values from the model you just chose in a different color. 
plot(fit1$residuals)
lines(lin.reg2$fitted.values,col="blue",lwd=3)

# Plot the acf of resids. How does it look? What type of model do you think 
#   that you should use 
acf(resids, main="base")

# try a few different models-
fit.ma6 <- arima(resids,order=c(0,0,6))
fit.ma12 <- arima(resids,order=c(0,0,12))

# use the AIC criteria to decide between the two models.
fit.ma6$aic
fit.ma12$aic
# indicates that the ma(12) is better (lower aic is indicative of better model)

# Also consider the acfs of the residuals from these models.
par(mfrow=c(2,1))
acf(fit.ma6$residuals, main = "order 6")
acf(fit.ma12$residuals, main = "order 12")
