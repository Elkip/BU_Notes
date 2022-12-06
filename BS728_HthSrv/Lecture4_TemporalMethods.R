########################
# Time Series          # 
# IN CLASS EXERCISE 1  #
######################## 
# Data is a modification of the weekly number of cases of respiratory illness 
# on the Cape

# Read in the data
resp.data <- read.csv("c:/Users/lfwhite/Documents/Work/weeklyRespData.csv",header=T)

names(resp.data)

attach(resp.data)

plot(resp.week,type='l',xlab="week",ylab="Number of cases")

## Overall decomposition of the series-assume an annual cycle ##
flu <- ts(resp.week,freq=52)
plot(stl(flu,s.window="periodic"))

# alternative plot #
plot(decompose(flu))

######################
# Modeling the Trend #
######################
# look for a trend in the data
fit.lin <- lm(resp.week ~ week.num)
plot(fit.lin$residuals,type='l')

# get the residuals after fitting the linear trend
resids.lin <- fit.lin$residuals

############################
# Modeling the seasonality #
############################
# possible periods to consider
# recall that sin(ax) has a period of 2*pi/a
period1 <- 52 # annual period
period2 <- 26 # semi annual
period3 <- 13 # quarterly
period4 <- 7 # monthlyish

# create the sinusoid terms
num.weeks <- length(resp.week)
s1 <- sin(c(1:num.weeks)*2*pi/period1)
c1 <- cos(c(1:num.weeks)*2*pi/period1)
s2 <- sin(c(1:num.weeks)*2*pi/period2)
c2 <- cos(c(1:num.weeks)*2*pi/period2)
s3 <- sin(c(1:num.weeks)*2*pi/period3)
c3 <- cos(c(1:num.weeks)*2*pi/period3)
s4 <- sin(c(1:num.weeks)*2*pi/period4)
c4 <- cos(c(1:num.weeks)*2*pi/period4)

# first fit an annual period
fit1 <- lm(resids.lin ~ s1 + c1)

# one annual sinusoid - plot the data versus the fitted values #
plot(resids.lin,type='l')
lines(fit1$fitted.values,col="red",lwd=3)

# plot residuals from model with one annual period 
plot(fit1$residuals,type='l')

# add a semi annual sinusoid and plot #
fit2 <- lm(resids.lin ~ s1 + c1 + s2 + c2)
plot(resids.lin,type='l')
lines(fit2$fitted.values,col="red",lwd=3)

# plot residuals
plot(fit2$residuals,type='l')
acf(fit2$residuals)

# with annual, semi-annual, and quarterly #
fit3 <- lm(resids.lin ~ s1 + c1 + +s2 + c2 + s3 + c3)

plot(resids.lin,type='l')
lines(fit3$fitted.values,col="red",lwd=3)

# plot residuals
plot(fit3$residuals,type='l')
abline(h=0)
acf(fit3$residuals)
# did not seem to help at all

# with annual, semi-annual, and monthly #
fit4 <- lm(resids.lin ~ s1 + c1 + +s2 + c2 + s4 + c4)

plot(resids.lin,type='l')
lines(fit4$fitted.values,col="red",lwd=3)

# plot residuals
plot(fit4$residuals,type='l')
abline(h=0)

# look at acf
acf(fit4$residuals)
# again no improvement here

resids.no.season <- fit2$residuals

# acf seems to indicate that an ar model might be best
# function to determine the best order of an ar model
ar(resids.no.season)

# now fit the model with a more general function
fit.ar3 <- arima(resids.no.season,order=c(3,0,0))
acf(fit.ar3$residuals)

fit.ar3$coef

# one unified model with everything in it
fit.all <- arima(resp.week,order=c(3,0,0),xreg=cbind(week.num,s1,c1,s2,c2))

# plot the residuals to confirm that they are the same
plot(fit.all$residuals)
lines(fit.ar3$residuals,col="red")

# method to get z statistics to test for significance 
#  This is not the ideal way to do this, but it is a start!
z.stats <- fit.all$coef/sqrt(diag(fit.all$var.coef))
z.stats

fit.all.2 <- arima(resp.week,order=c(1,0,0),xreg=cbind(week.num,s1,c1,s2,c2))

z.stats2 <- fit.all.2$coef/sqrt(diag(fit.all.2$var.coef))
z.stats2
plot(fit.all.2$residuals)
acf(fit.all.2$residuals)

# compare the aic values for each model. The lower one is better.
fit.all$aic
fit.all.2$aic
