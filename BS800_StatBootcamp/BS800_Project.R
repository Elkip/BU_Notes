help_data <- read.csv("/home/elkip/Documents/BU/BS800/Data/helplinreg.csv", header=TRUE)
head(help_data)
attach(help_data)

# Observe the Drinks.per.day variable
print("Distribution summary of drinks:")
summary(Drinks.per.Day)
print(paste("Number of NA values in drinks: ", sum(is.na(Drinks.per.Day))))
print(paste("Number of subjects with 0 drinks per day", sum(Drinks.per.Day == 0)))

# Observe the Drinks.per.day variable
print("Distribution summary of age:")
summary(age)
print(paste("Number of NA values in age: ", sum(is.na(age))))

# Fit linear regression model
plot(x=age, y=Drinks.per.Day)
reg <- lm(Drinks.per.Day ~ age)
summary(reg)

# Fit the line to the scatter plot
plot(x=age, y=Drinks.per.Day)
lines(x = age,y = reg$fitted.values, col=2)

# Predicted drinks at age 50
print(reg$coefficients[1] + reg$coefficients[2]*50)

# Residual Plot
plot(x=reg$fitted.values, y=reg$residuals)

# Take log of Drinks.per.day variable
lg_drinks <- log(Drinks.per.Day + 1)
summary(lg_drinks)

# Linear regression of lg_drinks on age
lg_reg <- lm(lg_drinks~age)
summary(lg_reg)

# Residual Plot
plot(x=lg_reg$fitted.values, y=lg_reg$residuals)

# Predicted drinks when age == 50
exp(lg_reg$coefficients[1] + lg_reg$coefficients[2]*50)-1

# Scatterplot of predicted drinks from both models
plot(x=age, y=Drinks.per.Day)
lines(x=sort(age), y=sort(reg$fitted.values), col=2, lwd=2)
lines(x=sort(age), y=sort((exp(lg_reg$fitted.values)-1)), col=3, lwd=2)
