# Alternative example: annual cancer cases in the US #
######################################################
# data downloaded from https://wonder.cdc.gov/cancer-v2015.html
#  on 12/6/21
################################
cancer <- read.csv("G:\\My Drive\\Work\\Teaching\\BS728\\Lecture 7 8 Time Series\\datasets\\CDC cancer data.csv")

# just look at invasive cancers
invasive <- subset(cancer,cancer$Cancer.Sites=="All Invasive Cancer Sites Combined")

# set up time series
invase.ts <- ts(invasive$Count,freq=6)
plot(stl(invase.ts,s.window="periodic"))

# plot the data #
#################
plot(decompose(invase.ts))
plot(invasive$Count)
# clear trend over time with increasing cases

# look at ACF #
acf(invasive$Count)
## note there is definite correlation in the data

# Fit the trend #
#################
year <- invasive$Year-min(invasive$Year)
year2 <- year^2

fit.lin <- lm(Count~year,data=invasive)
summary(fit.lin)
plot(fit.lin$residuals)
abline(h=0)
## there are some patterns in the the residuals

# try a quadratic term
fit.quad <- lm(Count~year+year2,data=invasive)
summary(fit.quad)

plot(fit.quad$residuals)
## looks better than linear

# look at model fit values versus actual data for both linear and quadratic
plot(year,invasive$Count)
lines(year,fit.lin$fitted.values,col="red")
lines(year,fit.quad$fitted.values,col="blue")

# try a likelihood ratio test
library(lmtest)
lrtest(fit.lin,fit.quad)
## quadratic fit looks good

# now work with residuals from quadratic fit to see if there is residual correlation
resids.quad <- fit.quad$residuals

acf(resids.quad)
## looks ok, but might be some slight issues

fit.3 <- arima(resids.quad,order=c(0,0,3))
acf(fit.3$residuals)
# acf looks a lot better

fit.4 <- arima(resids.quad,order=c(0,0,4))
acf(fit.4$residuals)

fit.4$aic
fit.3$aic
# pretty comparable models

# what if we try an AR model?
ar(resids.quad)
fit.ar <- arima(resids.quad,order=c(3,0,0))

fit.ar$aic

acf(fit.ar$residuals)
# aic is similar to MA(3) model; acf looks fine, too

# complete model fit #
######################
fit.final <- arima(invasive$Count,order=c(0,0,3),xreg=cbind(year,year2))
