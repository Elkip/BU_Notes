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

# Plot the series
# Explain the parts of this code and what each piece is doing.
plot(deaths$num,type='b',pch=20,ylab="Deaths",xlab="Time",xaxt='n')
axis(1,at=c(0,12,24,36,48,60),labels=c("1972","1973","1974","1975","1976","1977"))


# Investigate seasonality
plot(num~as.factor(num.month),xlab="Month")

# What is this plot showing? 
# How do you interpret the plot (i.e. what are its implications for modeling?)?


# Remove seasonal components
period1 <- 12
period2 <- 6

# create sinusoid terms for the two periods you just created.



# Determine which sinusoids to keep. Justify your decision with graphics.


# Plot the original data with the fitted values from the chosen model overlaid in a 
#   different color.



#################
# FIT THE TREND #
#################
# Look at the residuals from the model that you just fit.  
#  Does there appear to be a trend in these residuals?

#  Now, try to fit a linear trend using the time variable as the predictor. 
	# Hint: you will want to use the function lm().


#  Now investigate if the trend is quadratic. Fit the appropriate model.
time2 <- time^2

#  What model seems most appropriate?

# Plot the residuals from the seasonally adjusted model. Then overlay a plot of the 
#  fitted values from the model you just chose in a different color. 

# Get the residuals from this model and call them resids. Plot resids. How do they look? 
#   Do you feel that all trends and seasonal components have been removed? Why or why not?


# Plot the acf of resids. How does it look? What type of model do you think 
#   that you should use (look at slide 78 of the notes).



# try a few different models-
fit.ma6 <- arima(resids,order=c(0,0,6))
fit.ma12 <- arima(resids,order=c(0,0,12))

# use the AIC criteria to decide between the two models.
fit.ma6$aic
fit.ma12$aic
# indicates that the ma(12) is better (lower aic is indicative of better model)

# Also consider the acfs of the residuals from these models.
par(mfrow=c(2,1))
acf(fit.ma6$residuals)
acf(fit.ma12$residuals)

# The acf for ma(12) appears to be fitting better. What about these plots indicates that?


