## ----prep work, echo = FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------
library("knitr")
library("rjags")
library("coda")
library("formatR")
library("dplyr")
# Prevent code chunks out of range
# Use tidy = FALSE in each code chunk to disable this option
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
#setwd("/Users/xiaoyanliu/Documents/BU_Courses/BS849_Bayesian/Lecture4")


## ----L4S15_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
 model.1 <- "
model {
  for (i in 1:12) {
    y[i]     ~ dbin(theta, n[i])
    res[i]  <- (y[i] - n[i]*theta)/sqrt(n[i]*theta*(1-theta))
    res2[i] <- res[i]*res[i]
  }
  theta      ~ dunif(0, 1)
  X2.obs    <- sum(res2[])   # sum of squared stand. resids
}"


## ----L4S16_1, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE--------------------------
data.hosp <- list(y=c(41,25,24,23,25,42,24,53,26,25,58,31),
                 n=c(143,187,323,122,164,405,239,482,195,177,581,301))

jags.1 <- jags.model(textConnection(model.1),data=data.hosp, n.adapt=1500)
update(jags.1,10000)
test.1.theta <- coda.samples(jags.1, c('theta','X2.obs'), n.adapt=1500, n.iter=10000)
   
test.1 <- coda.samples(jags.1, c('res'), n.adapt=1500, n.iter=10000)
   out <- as.matrix(test.1)


## ----L4S16_2, eval = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE--------------------------
## test.1 <- coda.samples(jags.1, c('res'), n.adapt=1500, n.iter=10000)
##    out <- as.matrix(test.1)
##        boxplot(out)
## 
## #order plot
##        summary(test.1)
##        order(summary(test.1)[[2]][,3])
##        boxplot(out[,order(summary(test.1)[[2]][,3])])


## ----L4S16_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '20%', out.width = '90%', fig.align="center"----
plot(test.1.theta[,2])
par(mfrow = c(1,2))
boxplot(out); boxplot(out[,order(summary(test.1)[[2]][,3])])


## ----L4S17_1, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE--------------------------
 model.1 <- "
model {
  for (i in 1:11) {
    y[i]     ~ dbin(theta, n[i])
    res[i]  <- (y[i] - n[i]*theta)/sqrt(n[i]*theta*(1-theta))
    res2[i] <- res[i]*res[i]
  }
  theta      ~ dunif(0, 1)
  X2.obs    <- sum(res2[])   # sum of squared stand. resids
}"

data.hosp <- list(y=c(25,24,23,25,42,24,53,26,25,58,31),
                 n=c(187,323,122,164,405,239,482,195,177,581,301))

jags.1 <- jags.model(textConnection(model.1),data=data.hosp, n.adapt=1500)
update(jags.1,10000)
test.1.theta <- coda.samples(jags.1, c('theta','X2.obs'), n.adapt=1500, n.iter=10000)
   summary(test.1.theta)
   plot(test.1.theta)

test.1 <- coda.samples(jags.1, c('res'), n.adapt=1500, n.iter=10000)
   out <- as.matrix(test.1)
       boxplot(out)
#order plot
       boxplot(out[,order(summary(test.1)[[2]][,3])])


## ----L4S17_2, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
 model.1 <- "
model {
  for (i in 1:11) {
    y[i]     ~ dbin(theta, n[i])
    res[i]  <- (y[i] - n[i]*theta)/sqrt(n[i]*theta*(1-theta))
    res2[i] <- res[i]*res[i]
  }
  theta      ~ dunif(0, 1)
  X2.obs    <- sum(res2[])   # sum of squared stand. resids
}"


## ----L4S17_4, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.1.theta)

mean.theta <- round(summary(test.1.theta)[[1]]["theta", "Mean"],2) * 100
ci <- round(summary(test.1.theta)[[2]]["theta", c("2.5%", "97.5%")],2) *100


## ----L4S17_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '20%', out.width = '70%', fig.align="center"----
boxplot(out[,order(summary(test.1)[[2]][,3])])


## ----L4S19_2, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
 model.h <- "
model {
  for (i in 1:11) {
    y[i]             ~ dbin(theta[i], n[i])
    logit(theta[i]) <- mu+alpha[i]
          alpha[i]   ~ dnorm(0, tau)
  }
  theta.mean <- exp(mu)/(1+exp(mu))
       mu ~ dnorm(0, 0.001)
      tau ~ dgamma(1,1)
}
"


## ----L4S20_1, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE--------------------------
data.hosp <- list(y=c(25,24,23,25,42,24,53,26,25,58,31),
                 n=c(187,323,122,164,405,239,482,195,177,581,301))

jags.h <- jags.model(textConnection(model.h),data=data.hosp, n.adapt=1500)
update(jags.h,10000)
test.h.params <- coda.samples(jags.h, c('mu', 'alpha','theta','theta.mean'), n.adapt=1500, n.iter=10000)
test.h <- coda.samples(jags.h, c('theta'), n.adapt=1500, n.iter=10000)
out.h <- as.matrix(test.h)


## ----L4S20_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '70%', out.width = '70%', fig.align="center"----
par(mfrow = c(3,1))
traceplot(test.h.params[,c("alpha[1]", "alpha[2]", "alpha[3]")])


## ----L4S20_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '20%', out.width = '70%', fig.align="center"----
par(mfrow = c(3,1))
autocorr.plot(test.h.params[,c("alpha[1]")])
autocorr.plot(test.h.params[,c("alpha[2]")])
autocorr.plot(test.h.params[,c("alpha[3]")])


## ----L4S21_1, eval = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE--------------------------
## test.h <- coda.samples(jags.h, c('theta'), n.adapt=1500, n.iter=10000)
##    out.h <- as.matrix(test.h)
## 
## #order plot
##        boxplot(out.h[,order(summary(test.h)[[2]][,3])],cex.axis=0.7)


## ----L4S21_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
par(mfrow = c(1,1))
boxplot(out.h[,order(summary(test.h)[[2]][,3])],cex.axis=0.7)


## ----L4S22_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '70%', out.width = '70%', fig.align="center"----
summary(test.h.params[,13:24])[[2]]


## ----L4S23_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
 model.ind <- "
model {
  for (i in 1:11) {
    y[i]             ~ dbin(theta[i], n[i])
    theta[i]         ~ dbeta(1,1)
  }
}
"


## ----L4S24_1, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE--------------------------
data.hosp <- list(y=c(25,24,23,25,42,24,53,26,25,58,31),
                 n=c(187,323,122,164,405,239,482,195,177,581,301))

jags.ind <- jags.model(textConnection(model.ind),data=data.hosp, n.adapt=1500)
update(jags.ind,10000)
test.ind <- coda.samples(jags.ind, c('theta'), n.adapt=1500, n.iter=10000)
out.i <- as.matrix(test.ind)


## ----L4S24_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '70%', fig.align="center"----
plot(test.ind[,c(1,2,3)])

## ----L4S24_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.ind)[[2]]


## ----L4S24_4, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '70%', fig.align="center"----
autocorr.plot(test.ind[,c(1:6)])


## ----L4S25_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '70%', out.width = '70%', fig.align="center"----
boxplot( cbind(out.i[,order(summary(test.ind)[[2]][,3])],
               out.h[,order(summary(test.h)[[2]][,3])]),cex.lab=1.2, las=2, main = "Independent                 Hierarchical")
 abline(h=0.075)
 abline(h=0.1)
 abline(h=0.2)


## ----L4S27_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
boxplot(out.h[,order(summary(test.h)[[2]][,3])],cex.axis=0.7)


## ----L4S28_1, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----------------
#hierarchical model
test.h <- coda.samples(jags.h, c('theta'), n.adapt=1500, n.iter=1000)
   out.h <- as.matrix(test.h)
rank(out.h[1,])
out.h[1,]


## ----L4S29_1, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----------------
rank.out.h <- c()
  for(i in 1:nrow(out.h)){
    rank.out.h <- rbind(rank.out.h, rank(out.h[i,]))}

rank.out.h[1:5, ]


## ----L4S29_2, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE--------------------------------------
t(apply(rank.out.h, 2,quantile, probs=c(0.025, 0.5, 0.975)))
  boxplot(rank.out.h,cex.axis=0.75)
    order.column <- order(apply(rank.out.h,2,median))
      boxplot(rank.out.h[,order.column],cex.axis=0.75,las=2)


## ----L4S30_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '70%', fig.align="center"----
boxplot(rank.out.h[,order.column],cex.axis=0.75,las=2)


## ----L4S30_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
round(t(apply(rank.out.h, 2,quantile, probs=c(0.025, 0.5, 0.975))))


## ----L4S31_1, echo = FALSE, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE------------------------
# independent model
      test.ind <- coda.samples(jags.ind, c('theta'), n.adapt=1500, n.iter=10000)
           out.i <- as.matrix(test.ind)
rank.out.i <- c()
  for(i in 1:nrow(out.i)){
    rank.out.i <- rbind(rank.out.i, rank(out.i[i,]))}
#to compute the 95%CI of ranks


## ----L4S31_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '70%', fig.align="center"----
t(apply(rank.out.i, 2,quantile, probs=c(0.025, 0.5, 0.975)))


## ----L4S31_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '70%', fig.align="center"----
round(t(apply(rank.out.h, 2,quantile, probs=c(0.025, 0.5, 0.975))))


## ----L4S31_4, echo = FALSE, message=FALSE, warning=FALSE, out.height = '30%', out.width = '60%', fig.align="center"----
  boxplot(rank.out.i[,order.column],cex.axis=0.75,las=2)


## ----L4S31_5, echo = FALSE, message=FALSE, warning=FALSE, out.height = '30%', out.width = '60%', fig.align="center"----
  boxplot(rank.out.h[,order.column],cex.axis=0.75,las=2)


## ----L4S33_1, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE--------------------------------------
rats.data.p <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), xbar = 22, N = 30, T = 5,	
		Y = structure(
			.Data =   c(151, 199, 246, 283, 320,
							 145, 199, 249, 293, 354,
							 147, 214, 263, 312, 328,
							 155, 200, 237, 272, 297,
							 135, 188, 230, 280, 323,
							 159, 210, 252, 298, 331,
							 141, 189, 231, 275, 305,
							 159, 201, 248, 297, 338,
							 177, 236, 285, 350, 376,
							 134, 182, 220, 260, 296,
							 160, 208, 261, 313, 352,
							 143, 188, 220, 273, 314,
							 154, 200, 244, 289, 325,
							 171, 221, 270, 326, 358,
							 163, 216, 242, 281, 312,
							 160, 207, 248, 288, 324,
							 142, 187, 234, 280, 316,
							 156, 203, 243, 283, 317,
							 157, 212, 259, 307, 336,
							 152, 203, 246, 286, 321,
							 154, 205, 253, 298, 334,
							 139, 190, 225, 267, 302,
							 146, 191, 229, 272, 302,
							 157, 211, 250, 285, 323,
							 132, 185, 237, 286, 331,
							 160, 207, 257, 303, 345,
							 169, 216, 261, 295, 333,
							 157, 205, 248, 289, 316,
							 137, 180, 219, 258, 291,
							 153, 200, 244, 286, 324),
						.Dim = c(5,30)))

plot(rats.data.p$x,rats.data.p$Y[,1],ylim=c(140,370))
  for(i in 1:30){
     points(rats.data.p$x,rats.data.p$Y[,i],type="l")}
Y.t <- t(rats.data.p$Y)


## ----L4S34_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '60%', out.width = '60%', fig.align="center"----
plot(rats.data.p$x,rats.data.p$Y[,1],ylim=c(140,370))
  for(i in 1:30){
     points(rats.data.p$x,rats.data.p$Y[,i],type="l")}


## ----L4S36_1, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE--------------------------------------
 model.1 <- "
model
    {
        for( i in 1 : N ) {
            for( j in 1 : T ) {
                Y[i , j] ~ dnorm(mu[i , j],tau.c)
                mu[i , j] <- alpha.c + beta.c * (x[j] - xbar)
            }
        }
        tau.c ~ dgamma(1,1)
        alpha.c ~ dnorm(0.0,1.0E-6)    
         beta.c ~ dnorm(0.0,1.0E-6)
        alpha0 <- alpha.c +beta.c*(8- xbar )
       sigma.c <- 1/tau.c
    }"

rats.data <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), xbar = 22, N = 30, T = 5,	
		Y = Y.t)

jags.1 <- jags.model(textConnection(model.1),data=rats.data, n.adapt=1500)
update(jags.1,1000)
test.1 <- coda.samples(jags.1, c('alpha0','beta.c','sigma.c'), n.adapt=1500, n.iter=1000)
   summary(test.1)
   plot(test.1)
   autocorr.plot(test.1)


## ----L4S36_2, echo = FALSE, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE------------------------
alpha0 <- as.vector(round(summary(test.1)[[2]]["alpha0", c("2.5%", "50%", "97.5%")],1))
beta.c <- as.vector(round(summary(test.1)[[2]]["beta.c", c("2.5%", "50%", "97.5%")],1))
test.fixed <- test.1


## ----L4S36_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.1)[[2]]


## ----L4S37_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '30%', out.width = '70%', fig.align="center"----
plot(rats.data.p$x,rats.data.p$Y[,1],ylim=c(140,370))
  for(i in 1:30){
     points(rats.data.p$x,rats.data.p$Y[,i],type="l")}


## ----L4S39_1, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE--------------------------------------
 model.1 <- "
model
{
  for( i in 1 : N ) {
    for( j in 1 : T ) {
      Y[i , j] ~ dnorm(mu[i , j],tau.c)
      mu[i , j] <- alpha[i] + beta.c * (x[j] - xbar)
  }
      alpha[i] ~ dnorm(alpha.c,alpha.tau)
    }
    tau.c ~ dgamma(1,1)
    alpha.c ~ dnorm(0.0,1.0E-6)    
    alpha.tau ~ dgamma(1,1)
    beta.c ~ dnorm(0.0,1.0E-6)
    alpha.0 <- alpha.c +beta.c*(8- xbar )
    sigma.c <- 1/tau.c
    sigma.alpha <- 1/alpha.tau
}"

rats.data <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), 
                  xbar = 22, N = 30, T = 5,	Y = Y.t)

jags.1 <- jags.model(textConnection(model.1),data=rats.data, n.adapt=1500)
update(jags.1,10000)
test.1 <- coda.samples(jags.1, c('alpha.0','beta.c','alpha','sigma.c','sigma.alpha'), n.adapt=1500, n.iter=10000)
   summary(test.1)
   plot(test.1)
   autocorr.plot(test.1)
   summary(test.1[,31:34])


## ----L4S39_2, echo = FALSE, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE------------------------
alpha0 <- as.vector(round(summary(test.1)[[2]]["alpha.0", c("2.5%", "50%", "97.5%")],1))
beta.c <- as.vector(round(summary(test.1)[[2]]["beta.c", c("2.5%", "50%", "97.5%")],1))
test.int <- test.1


## ----L4S39_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.1)[[2]][c("alpha.0", "beta.c", "sigma.alpha", "sigma.c"), ]


## ----L4S42_1, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE--------------------------------------
 model.1 <- "
model
    {
        for( i in 1 : N ) {
            for( j in 1 : T ) {
                Y[i , j] ~ dnorm(mu[i , j],tau.c)
                mu[i , j] <- alpha[i] + beta[i] * (x[j] - xbar)
            }
            alpha[i] ~ dnorm(alpha.c,alpha.tau)
            beta[i] ~ dnorm(beta.c,beta.tau)
        }
        tau.c ~ dgamma(1,1)
        alpha.c ~ dnorm(0.0,1.0E-6)    
        alpha.tau ~ dgamma(1,1)
        beta.c ~ dnorm(0.0,1.0E-6)
        beta.tau ~ dgamma(1,1)
        alpha.0 <- alpha.c +beta.c*(8- xbar )
       sigma.c <- 1/tau.c
       sigma.alpha <- 1/alpha.tau
       sigma.beta <- 1/beta.tau

    }"

jags.1 <- jags.model(textConnection(model.1),data=rats.data, n.adapt=1500)
update(jags.1,10000)
test.1 <- coda.samples(jags.1, c('alpha.0','beta.c','alpha','beta','sigma.c','sigma.alpha','sigma.beta'), n.adapt=1500, n.iter=10000)
   summary(test.1)
   plot(test.1)
   autocorr.plot(test.1)
   summary(test.1[,c(31, 62:65)])


## ----L4S42_2, echo = FALSE, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE------------------------
alpha0 <- as.vector(round(summary(test.1)[[2]]["alpha.0", c("2.5%", "50%", "97.5%")],1))
beta.c <- as.vector(round(summary(test.1)[[2]]["beta.c", c("2.5%", "50%", "97.5%")],1))
test.int.slope <- test.1


## ----L4S42_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.1)[[2]][c("alpha.0", "beta.c", "sigma.alpha", "sigma.beta", "sigma.c"), ]


## ----L4S43_1, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.fixed)[[2]][c("alpha0", "beta.c", "sigma.c"), ]
sigma.c.fixed <- as.vector(round(summary(test.fixed)[[2]][c("sigma.c"), c("2.5%", "50%", "97.5%")], 1))

## ----L4S43_2, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.int)[[2]][c("alpha.0", "beta.c", "sigma.alpha", "sigma.c"), ]
sigma.c.int <- as.vector(round(summary(test.int)[[2]][c("sigma.c"), c("2.5%", "50%", "97.5%")], 1))
sigma.alpha.int <- as.vector(round(summary(test.int)[[2]][c("sigma.alpha"), c("2.5%", "50%", "97.5%")], 1))

## ----L4S43_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.int.slope)[[2]][c("alpha.0", "beta.c", "sigma.alpha", "sigma.beta", "sigma.c"), ]
sigma.c.int.slope <- as.vector(round(summary(test.int.slope)[[2]][c("sigma.c"), c("2.5%", "50%", "97.5%")], 1))
sigma.alpha.int.slope <- as.vector(round(summary(test.int.slope)[[2]][c("sigma.alpha"), c("2.5%", "50%", "97.5%")], 1))
sigma.beta.int.slope <- as.vector(round(summary(test.int.slope)[[2]][c("sigma.beta"), c("2.5%", "50%", "97.5%")], 1))


## ----L4S44_1, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.int)[[2]][1:10, ]


## ----L4S44_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '60%', out.width = '80%', fig.align="center"----
out <- as.matrix(test.int)
boxplot(out[,1:30], las=2)


## ----L4S45_1, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.int.slope)[[2]][1:10, ]

## ----L4S45_2, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.int.slope)[[2]][32:41, ]


## ----L4S45_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '30%', out.width = '80%', fig.align="center"----
out <- as.matrix(test.int.slope)
boxplot(out[,1:30], las=2)


## ----L4S45_4, echo = FALSE, message=FALSE, warning=FALSE, out.height = '30%', out.width = '80%', fig.align="center"----
out <- as.matrix(test.int.slope)
boxplot(out[,32:61], las=2)


## ----L4S46_1, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test.int.slope)[[2]][c("alpha.0", "beta.c", "sigma.alpha", "sigma.beta", "sigma.c"), ]

alpha.0.int.slope <- as.vector(round(summary(test.int.slope)[[2]][c("alpha.0"), c("2.5%", "50%", "97.5%")], 1))

beta.c.int.slope <- as.vector(round(summary(test.int.slope)[[2]][c("beta.c"), c("2.5%", "50%", "97.5%")], 1))


## ----L4S49_1, echo = FALSE, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE------------------------
 model.1 <- "
model
	{
		for( i in 1 : N ) {
			for( j in 1 : T ) {
				Y[i , j] ~ dnorm(mu[i , j],tau.c)
				mu[i , j] <- alpha[i] + beta.c * (x[j] - xbar)
			}
			alpha[i] ~ dnorm(alpha.c,alpha.tau)
		}
       	tau.c ~ dgamma(1,1)
		alpha.c ~ dnorm(0.0,1.0E-6)	   
		alpha.tau ~ dgamma(1,1)
		beta.c ~ dnorm(0.0,1.0E-6)
		alpha.0 <- alpha.c +beta.c*(8- xbar )
       sigma.c <- 1/tau.c
       sigma.alpha <- 1/alpha.tau
	}"

rats.data <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), xbar = 22, N = 30, T = 5,	
		Y = Y.t)


## ----L4S49_2, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE--------------------------------------
jags.1 <- jags.model(textConnection(model.1),data=rats.data, n.adapt=1500, n.chains=2)
update(jags.1,1000)
dic.1 <- dic.samples(jags.1,n.iter=1000)


## ----echo=FALSE------------------------------------------------------------------------------------------------------
DM <-read.csv('Y.school.mathscore.csv')
Yb <- as.vector(tapply(DM[,2],DM[,1],mean,simplify = TRUE))
n <- table(DM[,1])
Ord <- order(Yb)
m<-length(n)
par(mfrow=c(1,1))
plot(c(1,m),range(DM[,2]),xlab='Rank of School (based on mean score)',ylab='Math Score',type='n')
for (i in 1:m){
  lines(rep(i,2),range(DM[DM[,1]==Ord[i],2]),col='grey')
  points(rep(i,n[Ord[i]]),DM[DM[,1]==Ord[i],2],pch=21,bg='grey',cex=0.5)}


## ----out.width='80%',fig.align='center', echo=FALSE------------------------------------------------------------------
par(mfrow=c(1,2))
hist(Yb,xlab='Sample Means', ylab='Proportion',probability = TRUE,main='')
plot(as.vector(n),as.vector(Yb),ylab='Sample Mean',xlab='Sample Size',pch=21)


## --------------------------------------------------------------------------------------------------------------------
M<-"model{
 for (i in 1:N){
  Score[i]~dnorm(theta[School[i]],iS2)
}
 for (s in 1:S){
   theta[s]~dnorm(mu,iT2)
 }
 mu~dnorm(mu0,ig)
 iS2~dgamma(nu0/2,nu0*s02/2)
 iT2~dgamma(eta0/2,eta0*T02/2) 
}"
writeLines(M,'Mod.txt')


## ----L4S49_3a, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE-------------------------------------
library(rjags)
DM <-read.csv('Y.school.mathscore.csv')
Data <- list(N=nrow(DM),S=max(DM$school),School=DM$school,
             Score=DM$mathscore,nu0=1,eta0=1,s02=100,T02=100,mu0=50,ig=0.04)
  M1 <- jags.model('Mod.txt',data=Data,n.adapt = 1500)
  update(M1,10000)
  M2<-coda.samples(M1,variable.names = c('theta','iS2','iT2','mu'),n.iter = 100000)
  M3<-as.matrix(M2);
  S2<-M3[,1];T2<-M3[,2];MU<-M3[,3];TH<-M3[,-c(1:3)];


## ----out.width='70%',fig.align='center', warning=FALSE,echo=FALSE----------------------------------------------------
library(coda)
df <- function(x){
  acf.x<-acf(x,plot=FALSE,lag.max = 20)
  n.x<-effectiveSize(x)
  mcse.x<-sqrt(var(x)/n.x)
  list(ACF=acf.x,n.Eff=n.x,MC.SE=mcse.x)
}
d.mu<-df(MU);d.t2<-df(T2);d.s2<-df(S2);S<-length(MU)
par(mfrow=c(1,3))
boxplot(S2~rep(1:10,each=S/10),xlab='Block of Observations',ylab=expression(sigma^2))
boxplot(MU~rep(1:10,each=S/10),xlab='Block of Observations',ylab=expression(mu))
boxplot(T2~rep(1:10,each=S/10),xlab='Block of Observations',ylab=expression(tau^2))


## ----echo=FALSE------------------------------------------------------------------------------------------------------
Sum <- sapply(list(sqrt(S2),MU,sqrt(T2)), function(x) c(Mean=mean(x),quantile(x,probs = c(0.025,0.5,0.975))),simplify=T)
dimnames(Sum)[[2]]<-c('Sigma','MU','Tau')
Sum


## ----out.width='60%',fig.align='center', echo=FALSE------------------------------------------------------------------
par(mfrow=c(1,3),mar=c(4,4.5,1,1))
plot(density(S2,adjust=1.2),type='l',lwd=4,xlab=expression(sigma^2),main='',ylab='Posterior Density',cex.lab=1.7,cex.axis=1.7)
abline(v=quantile(S2,probs = c(0.025,0.5,0.975)),lty=5,col='grey',lwd=3)
par(mar=c(4,1.7,1,1))
plot(density(MU,adjust=1.2),type='l',lwd=4,xlab=expression(mu),main='',ylab='Posterior Density',cex.lab=1.7,cex.axis=1.7)
abline(v=quantile(MU,probs = c(0.025,0.5,0.975)),lty=5,col='grey',lwd=3)
plot(density(T2,adjust=1.4),type='l',lwd=4,xlab=expression(tau^2),main='',ylab='Posterior Density',cex.lab=1.7,cex.axis=1.7)
abline(v=quantile(T2,probs = c(0.025,0.5,0.975)),lty=5,col='grey',lwd=3)


## ----out.width='90%', fig.align='center', echo=FALSE-----------------------------------------------------------------
post.th <- as.vector(apply(TH,2,mean))
r.th <- post.th-Yb; inds<-order(-abs(r.th))
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1))
plot(Yb,post.th,xlim=range(c(Yb,post.th)),ylim=range(c(Yb,post.th)),xlab=expression(bar(y)),ylab=expression(hat(theta)))
abline(c(0,1))
plot(as.vector(n),r.th,ylab=expression(bar(y)-hat(theta)),xlab='Sample size')
abline(h=0)


## ----fig.align='center', fig.height=6, echo=FALSE--------------------------------------------------------------------
plot(range(c(Yb,post.th)),c(0,1),type='n',axes=FALSE,xlab='',ylab='',ylim=c(-0.1,1.2))
for (j in 1:100){
  lines(c(Yb[j],post.th[j]),c(1,0),lwd=1.4)
  lines(c(Yb[j],Yb[j]),c(1,1+n[j]/(5*max(n))),lty=3,col='red',lwd=1.5)
}
points(Yb,rep(1,100),pch=21,bg='orange',cex=1.2)
points(post.th,rep(0,100),pch=21,bg='blue',cex=1.2)
text(post.th[inds[1]],0,inds[1],pos=1,cex=1.2)
text(post.th[inds[2]],0,inds[2],pos=1,cex=1.2)
text(post.th[inds[3]],0,inds[3],pos=1,cex=1.2)
text(max(Yb),1,expression(bar(y)),pos=4,font=4,cex=1.5)
text(max(post.th),0,expression(hat(theta)),pos=4,font=4,cex=1.5)
mtext(expression('Observed Sample Means '* phantom('(orange dots) ') *' vs. Posterior Means ' *phantom('(blue dots)')),1,cex=1.5)
mtext(expression(phantom('Observed Sample Means ')* '(orange dots) ' *phantom('vs. Posterior Means') *phantom('(blue dots)')),1,col='orange',font=4,cex=1.5)
mtext(expression(phantom('Observed Sample Means (orange dots) ') *phantom('vs. Posterior Means') *' (blue dots)'),1,col='blue',font=4,cex=1.5)
mtext('Length of red lines are proportional with the school sample size',3,col='red',cex=1.5)


## ----echo=FALSE------------------------------------------------------------------------------------------------------
DM <-read.csv('Y.school.mathscore.csv')
Yb <- as.vector(tapply(DM[,2],DM[,1],mean,simplify = TRUE))
n <- table(DM[,1])
Ord <- order(Yb)
m<-length(n)


## --------------------------------------------------------------------------------------------------------------------
# Weakly informative priors
nu0 <- 1; s02<-100
eta0 <- 1; t02 <- 100
mu0 <- 50; g02 <- 25
# Data 
DM <-read.csv('Y.school.mathscore.csv')
Yb <- as.vector(tapply(DM[,2],DM[,1],mean,simplify = TRUE))
V <- tapply(DM[,2],DM[,1],var,simplify = TRUE)
n <- as.vector(table(DM[,1]))
m<-length(n)
# Starting values
theta <- Yb; sigma2 <- mean(V)
mu <- mean(theta); tau2 <- var(theta)
# Start Gibbs and initialize objects
set.seed(1)
S <- 5000
TH <- matrix(NA,nrow = S, ncol = m)
S2 <- MU <- T2 <- rep(NA,S)


## --------------------------------------------------------------------------------------------------------------------
for (s in 1:S){# we cant avoid looping in here
  # Sample theta
  Mean <- (mu/tau2+Yb*n/sigma2)/(1/tau2+n/sigma2)
  theta<- rnorm(m,Mean,sqrt(1/(1/tau2+n/sigma2)))
  # sample sigma^2
  nuS02 <- nu0*s02+sum((DM[,2]-rep(theta,n))^2)
  sigma2 <- (nuS02)/rchisq(1,nu0+sum(n))
  # sample mu
  muM<-(mu0/g02+mean(theta)*m/tau2)/(1/g02+m/tau2)
  mu <- rnorm(1,muM,sqrt(1/(1/g02+m/tau2)))
  # sample tau^2
  tau2 <- (eta0*t02+sum((theta-mu)^2))/rchisq(1,eta0+m)
  # Store Results
  TH[s,] <- theta; S2[s]<-sigma2;
  MU[s]<-mu; T2[s]<-tau2
}


## ----out.width='70%',fig.align='center', warning=FALSE,echo=FALSE----------------------------------------------------
library(coda)
df <- function(x){
  acf.x<-acf(x,plot=FALSE,lag.max = 20)
  n.x<-effectiveSize(x)
  mcse.x<-sqrt(var(x)/n.x)
  list(ACF=acf.x,n.Eff=n.x,MC.SE=mcse.x)
}
d.mu<-df(MU);d.t2<-df(T2);d.s2<-df(S2)
par(mfrow=c(1,3))
boxplot(S2~rep(1:10,each=S/10),xlab='Block of Observations',ylab=expression(sigma^2))
boxplot(MU~rep(1:10,each=S/10),xlab='Block of Observations',ylab=expression(mu))
boxplot(T2~rep(1:10,each=S/10),xlab='Block of Observations',ylab=expression(tau^2))


## ----echo=FALSE------------------------------------------------------------------------------------------------------
Sum <- sapply(list(sqrt(S2),MU,sqrt(T2)), function(x) c(Mean=mean(x),quantile(x,probs = c(0.025,0.5,0.975))),simplify=T)
dimnames(Sum)[[2]]<-c('Sigma','MU','Tau')
Sum


## ----out.width='60%',fig.align='center', echo=FALSE------------------------------------------------------------------
par(mfrow=c(1,3),mar=c(4,4.5,1,1))
plot(density(S2,adjust=1.2),type='l',lwd=4,xlab=expression(sigma^2),main='',ylab='Posterior Density',cex.lab=1.7,cex.axis=1.7)
abline(v=quantile(S2,probs = c(0.025,0.5,0.975)),lty=5,col='grey',lwd=3)
par(mar=c(4,1.7,1,1))
plot(density(MU,adjust=1.2),type='l',lwd=4,xlab=expression(mu),main='',ylab='Posterior Density',cex.lab=1.7,cex.axis=1.7)
abline(v=quantile(MU,probs = c(0.025,0.5,0.975)),lty=5,col='grey',lwd=3)
plot(density(T2,adjust=1.4),type='l',lwd=4,xlab=expression(tau^2),main='',ylab='Posterior Density',cex.lab=1.7,cex.axis=1.7)
abline(v=quantile(T2,probs = c(0.025,0.5,0.975)),lty=5,col='grey',lwd=3)


## ----out.width='90%', fig.align='center', echo=FALSE-----------------------------------------------------------------
post.th <- as.vector(apply(TH,2,mean))
r.th <- post.th-Yb; inds<-order(-abs(r.th))
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1))
plot(Yb,post.th,xlim=range(c(Yb,post.th)),ylim=range(c(Yb,post.th)),xlab=expression(bar(y)),ylab=expression(hat(theta)))
abline(c(0,1))
plot(as.vector(n),r.th,ylab=expression(bar(y)-hat(theta)),xlab='Sample size')
abline(h=0)


## ----fig.align='center', fig.height=6,echo=FALSE---------------------------------------------------------------------
plot(range(c(Yb,post.th)),c(0,1),type='n',axes=FALSE,xlab='',ylab='',ylim=c(-0.1,1.2))
for (j in 1:100){
  lines(c(Yb[j],post.th[j]),c(1,0),lwd=1.4)
  lines(c(Yb[j],Yb[j]),c(1,1+n[j]/(5*max(n))),lty=3,col='red',lwd=1.5)
}
points(Yb,rep(1,100),pch=21,bg='orange',cex=1.2)
points(post.th,rep(0,100),pch=21,bg='blue',cex=1.2)
text(post.th[inds[1]],0,inds[1],pos=1,cex=1.2)
text(post.th[inds[2]],0,inds[2],pos=1,cex=1.2)
text(post.th[inds[3]],0,inds[3],pos=1,cex=1.2)
text(max(Yb),1,expression(bar(y)),pos=4,font=4,cex=1.5)
text(max(post.th),0,expression(hat(theta)),pos=4,font=4,cex=1.5)
mtext(expression('Observed Sample Means '* phantom('(orange dots) ') *' vs. Posterior Means ' *phantom('(blue dots)')),1,cex=1.5)
mtext(expression(phantom('Observed Sample Means ')* '(orange dots) ' *phantom('vs. Posterior Means') *phantom('(blue dots)')),1,col='orange',font=4,cex=1.5)
mtext(expression(phantom('Observed Sample Means (orange dots) ') *phantom('vs. Posterior Means') *' (blue dots)'),1,col='blue',font=4,cex=1.5)
mtext('Length of red lines are proportional with the school sample size',3,col='red',cex=1.5)


## ----echo=FALSE------------------------------------------------------------------------------------------------------
# Remaining prior parameters are set
nu00<-2;s002<-2/100;alpha<-1
# Starting values
theta <- Yb; sigma2 <- V
mu <- mean(theta); tau2 <- var(theta);nu0<-1;s02<-m/mean(1/V)
# Start Gibbs and iniialize objects
set.seed(1)
S <- 5000
THj <- S2j<- matrix(NA,nrow=S, ncol =m)
MUj<-T2j<-S02<-NU0<-rep(NA,S)
for (s in 1:S){# we cant avoid looping in here
  # Sample theta
  theta<- rnorm(m,(mu/tau2+Yb*n/sigma2)/(1/tau2+n/sigma2),sqrt(1/(1/tau2+n/sigma2)))
  # sample sigma^2
  sigma2 <- (nu0*s02+tapply((DM[,2]-rep(theta,n))^2,DM[,1],sum,simplify = TRUE))/rchisq(m,nu0+n)
  # sample mu
  mu <- rnorm(1,(mu0/g02+mean(theta)*m/tau2)/(1/g02+m/tau2),sqrt(1/(1/g02+m/tau2)))
  # sample tau^2
  tau2 <- (eta0*t02+sum((theta-mu)^2))/rchisq(1,eta0+m)
  # Sigma_0^2
  s02 <- rchisq(1,nu00+m*nu0)/(s002+nu0*sum(1/sigma2))
  # Sample from nu0
  x<-1:30
  lp.nu0 <- (m*x)/2*log(x*s02/2)-m*lgamma(x/2)-(x/2+1)*sum(log(sigma2))-x*(alpha+(s02/2)*sum(1/sigma2))
  nu0<-sample(x,1,prob=exp(lp.nu0-max(lp.nu0)))
  # Store Results
  THj[s,] <- theta;S2j[s,]<-sigma2;MUj[s]<-mu;T2j[s]<-tau2;S02[s]<-s02;NU0[s]<-nu0
}


## ----eval=FALSE------------------------------------------------------------------------------------------------------
## # Remaining prior parameters are set
## nu00<-2;s002<-2/100;alpha<-1
## # Starting values
## theta <- Yb; sigma2 <- V
## mu <- mean(theta); tau2 <- var(theta);nu0<-1;s02<-m/mean(1/V)
## # Start Gibbs and iniialize objects
## set.seed(1)
## S <- 5000
## THj <- S2j<- matrix(NA,nrow=S, ncol =m)
## MUj<-T2j<-S02<-NU0<-rep(NA,S)
## for (s in 1:S){# we cant avoid looping in here
##   # Sample theta
##   theta<- rnorm(m,(mu/tau2+Yb*n/sigma2)/(1/tau2+n/sigma2),sqrt(1/(1/tau2+n/sigma2)))
##   # sample sigma^2
##   sigma2 <- (nu0*s02+tapply((DM[,2]-rep(theta,n))^2,DM[,1],sum,simplify = TRUE))/rchisq(m,nu0+n)
##   # sample mu
##   mu <- rnorm(1,(mu0/g02+mean(theta)*m/tau2)/(1/g02+m/tau2),sqrt(1/(1/g02+m/tau2)))
##   # sample tau^2
##   tau2 <- (eta0*t02+sum((theta-mu)^2))/rchisq(1,eta0+m)


## ----eval=FALSE------------------------------------------------------------------------------------------------------
##   # Sigma_0^2
##   s02 <- rchisq(1,nu00+m*nu0)/(s002+nu0*sum(1/sigma2))
##   # Sample from nu0
##   x<-1:30
##   lp.nu0 <- (m*x)/2*log(x*s02/2)-m*lgamma(x/2)-(x/2+1)*sum(log(sigma2))-x*(alpha+(s02/2)*sum(1/sigma2))
##   nu0<-sample(x,1,prob=exp(lp.nu0-max(lp.nu0)))
##   # Store Results
##   THj[s,] <- theta;S2j[s,]<-sigma2;MUj[s]<-mu;T2j[s]<-tau2;S02[s]<-s02;NU0[s]<-nu0
## 


## ----out.width='80%', fig.align='center',echo=FALSE------------------------------------------------------------------
par(mfrow=c(2,2),mar=c(4,3,1,1))
plot(density(MUj[-c(1:1000)],adjust=1.4),type='l',xlab=expression(mu),ylab='Posterior Density',lwd=3,main='',axes=F)
lines(density(MU[-c(1:1000)],adjust=1.4),lwd=3,col='grey')
axis(1);axis(2)
plot(density(T2j[-c(1:1000)],adjust=1.4),type='l',xlab=expression(tau^2),ylab='Posterior Density',lwd=3,main='',axes=F)
lines(density(T2[-c(1:1000)],adjust=1.4),lwd=3,col='grey')
axis(1);axis(2)
plot(as.numeric(names(table(NU0))),table(NU0)/S,type='h',lwd=5,xlab=expression(nu[0]),ylab='Posterior Density',axes=F)
axis(2)
axis(1,at=as.numeric(names(table(NU0))),labels=as.numeric(names(table(NU0))))
plot(density(S02[-c(1:1000)],adjust=1.4),type='l',xlab=expression(sigma[0]^2),ylab='Density',lwd=3,main='',axes=F)
axis(1);axis(2)


## ----out.width='70%', fig.align='center',echo=FALSE------------------------------------------------------------------
post.thj <- as.vector(apply(THj,2,mean))
r.thj <- post.thj-Yb; 
par(mfrow=c(1,2),mar=c(4,4.5,1,1))
plot(Yb,post.thj,xlim=range(c(Yb,post.thj)),ylim=range(c(Yb,post.thj)),xlab=expression(bar(y)),ylab=expression(hat(theta)))
abline(c(0,1))
plot(as.vector(n),r.thj,ylab=expression(bar(y)-hat(theta)),xlab='Sample size')
abline(h=0)


## ----out.width='70%', fig.align='center',echo=FALSE------------------------------------------------------------------
post.s2 <- as.vector(apply(S2j,2,mean))
r.s2 <- post.s2-V; 
par(mfrow=c(1,2),mar=c(4,4.5,1,1))
plot(V,post.s2,xlim=range(c(V,post.s2)),ylim=range(c(V,post.s2)),xlab=expression(s^2),ylab=expression(hat(sigma)^2))
abline(c(0,1))
plot(as.vector(n),r.s2,ylab=expression(s^2-hat(sigma)^2),xlab='Sample size')
abline(h=0)

