###
## read in data and set up the data
dat <- read.csv("/home/elkip/Datasets/illustrated_data.csv")
y <- dat$y
x <- as.matrix(data.frame(x0=rep(1,nrow(dat)), x1=dat$x1, x2=dat$x2))

## X'X
t(x)%*%x

## X'y
t(x)%*%y

## (X'X)^(-1)
solve(t(x)%*%x)

## beta = (X'X)^(-1)X'y
solve(t(x)%*%x)%*%(t(x)%*%y)


beta <- solve(t(x)%*%x)%*%(t(x)%*%y)

## RSS = y'y - b'X'y
RSS <- t(y)%*%y - t(beta)%*%t(x)%*%y
RSS 



# s2 = RSS/(n-p-1)
s2 <- RSS/(12-2-1)
s2

## SYY = y'y - n*y_bar^2
SYY <- t(y)%*%y - length(y)*mean(y)**2
SYY

## SSreg = bX'y - n*y_bar^2
SSreg <- t(beta) %*% t(x)%*%y - length(y)*mean(y)**2
SSreg



## R2 = 1- RSS/SYY
R2 <- 1 - RSS/SYY
R2

## R2.adj = ((n-1)R2 - p)/(n-p-1)
R2.adj <- ((12-1)*R2 -2)/(12-2-1)
R2.adj


## use built-in regression function
res <- lm(y~x1+x2, data= dat)
res.s <- summary(res)
names(res)
names(res.s)

### coefficients
res$coefficients
res.s$cov.unscaled	# (X'X)^(-1)

# s2 = RSS/(n-p-1)
s2 <- deviance(res)/res$df.residual

s <- res.s$sigma
s**2

# R2
1-deviance(res)/sum((y-mean(y))^2)
res.s$r.squared
res.s$adj.r.squared

anova.table <- anova(res)
names(anova.table)
anova.table$Sum


