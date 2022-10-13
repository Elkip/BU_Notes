install.packages("faraway")
library(faraway)
data(state)
names(state)
statedata <- data.frame(state.x77, row.names=state.abb)

names(statedata)
write.csv(statedata, file="statedata.csv", quote=FALSE, row.names=F)

############# from here
## the data were collected from U.S. Bureau of the Census. 
## use life expectancy as the response
## the remaining variables as predictors

statedata <- read.csv("statedata.csv")
g <- lm(Life.Exp ~ ., data = statedata)
summary(g)

### backward elimination

g <- lm(Life.Exp ~ ., data = statedata)
summary(g)$coefficients

g <- update(g, .~ . - Area)
summary(g)$coefficients

g <- update(g, .~ . - Illiteracy)
summary(g)$coefficients

g <- update(g, .~ . - Income)
summary(g)$coefficients

g <- update(g, .~ . - Population)
summary(g)$coefficients
summary(g)

## step(lm(Life.Exp ~ ., data = statedata), scope=list(lower=as.formula(.~Illiteracy)), direction="backward")

### the variables omitted from the model may still be 
## related to the response
summary(lm(Life.Exp ~ Illiteracy+Murder+Frost, statedata))$coeff


### 
## forward
f <- ~Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area
m0 <- lm(Life.Exp ~ 1, data = statedata)
m.forward <- step(m0, scope = f, direction ="forward", k=2)

## hand calculate the AIC value
aov(lm(Life.Exp ~ Murder, data=statedata))
# AIC = n*log(RSS/n) + 2p

n <- nrow(statedata)
n*log(34.46/n)+2*2

extractAIC(m.forward, k=2)		# by default k=2, AIC
extractAIC(m.forward,k=log(50))		# k=log(n), BIC

## final model using AIC
summary(m.forward)$coefficients

## use BIC
n <- nrow(statedata)
m.forward.BIC <- step(m0, scope = f, direction ="forward", 
			k=log(n), trace=FALSE)
summary(m.forward.BIC)$coefficients


### backward
m1 <- update(m0, f)
m.backward <- step(m1, scope = c(lower= ~ 1), 
			direction = "backward", trace=FALSE)
summary(m.backward)$coefficients

### stepwise
m.stepup <- step(m0, scope=f, direction="both", trace=FALSE)
summary(m.stepup)$coefficients

 


########## about Cp
install.packages("leaps")
library(leaps)
leaps <- regsubsets(Life.Exp ~ ., data = statedata)
rs <- summary(leaps)
par(mfrow=c(1,2))
plot(2:8, rs$cp, xlab="No. of parameters",
		ylab="Cp Statistic")
abline(0,1)

plot(2:8, rs$adjr2, xlab="No. of parameters",
		ylab="Adjusted R-Squared")
abline(0,1)
names(rs)

rs







