## normality
gala <- read.table("gala.txt", header=T)
gs <- lm(sqrt(Species) ~ Area + Elevation + Scruz + Nearest + Adjacent, gala)
g <- lm(Species ~ Area + Elevation + Scruz + Nearest + Adjacent, gala)

par(mfrow=c(2,2))
plot(fitted(g), residuals(g), xlab="Fitted", ylab="Residuals")

qqnorm(residuals(g), ylab="Residuals")
qqline(residuals(g))

library(car)
qqPlot(residuals(g))

hist(g$residuals)

shapiro.test(g$residuals)

dev.off()

##testing for curvature
library(alr4)
m2 <- lm(fertility ~ log(ppgdp) + pctUrban, UN11)
residualPlots(m2)
summary(m2)$coeff

UN12 <- UN11
UN12$pctUrban2 <- UN12$pctUrban**2
summary(a <- lm(fertility ~ log(ppgdp) + pctUrban + pctUrban2, UN12))$coeff

UN12$lppgdp2 <- (log(UN12$ppgdp))**2
summary(a <- lm(fertility ~ log(ppgdp) + pctUrban + lppgdp2, UN12))$coeff

UN12$fitted2 <- m2$fitted**2
summary(a <- lm(fertility ~ log(ppgdp) + pctUrban + fitted2, UN12))$coeff

m3 <- lm(fertility ~ log(ppgdp) + pctUrban + pctUrban2, UN12)
residualPlots(m3)

#### outliers
load("savings.rda")
g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
n <- nrow(savings)
pprime <- 5   # number of parameters
jack <- rstudent(g)   # studentized residual
jack[which.max(abs(jack))]  # maximum studentized residual

# threshold for lower tail
qt(0.05/(50*2), df = n-pprime-1 , lower.tail=TRUE)  

## checking which point is greater than threshold
yn.outlier <- abs(jack) > abs(qt(0.05/(50*2), df = n-pprime-1 , lower.tail=TRUE))
as.numeric(yn.outlier)
which(as.numeric(yn.outlier)==1)


### if calculate studentized residual with formula
rstd <- rstandard(g)
rstd.sq <- rstd[which.max(abs(rstd))]**2
jack.cal <- rstd[which.max(abs(rstd))]*sqrt((n-pprime-1)/(n-pprime-rstd.sq))



## simple example
x <- c(1,2,3,10)
y <- c(2.1,3.8,5.2,2.1)
a <- lm(y~x)
as <- summary(a)
rstudent(a)
astd <- rstandard(a)
astd*sqrt((4-2-1)/(4-2-astd**2))

#### influential points
cook <- cooks.distance(g)
n <- nrow(savings)
pprime <- 5

check <- cook[cook > 4/n]  # rule of thumb
sort(check, decreasing=TRUE) [1:5]  # list first five max

cook[cook>0.5]    # check Di>0.5
cook[(pf(cook, pprime, n-pprime)>0.5)] # use F-dist

influenceIndexPlot(g)

#countries <- row.names(savings)
#install.packages("faraway")
#library(faraway)
#halfnorm(cook, 2, labs= countries , ylab="Cook's distance")

g <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
g1 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, 
		subset = (countries != "Libya"))
g2 <- lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, 
		subset = (cook < max(cook)))
summary(g)$coefficients
summary(g1)$coefficients

par(mfrow=c(2,2))
plot(g)


