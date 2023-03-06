## ----prep work, echo = FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------
library("knitr")
library("rjags")
library("coda")
library("formatR")
library("dplyr")
# Prevent code chunks out of range
# Use tidy = FALSE in each code chunk to disable this option
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)


## ----L7S33_1, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------
data.br <- list(N = 20, n.birth = c(236, 739, 970, 2371, 309, 679, 26, 1272, 
3246, 1904, 357, 1080, 1027, 28, 2507, 138, 502, 1501, 2750, 
192), HT = c(0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 
1, 1, 1, 1), y = c(8, 16, 15, 23, 5, 13, 4, 19, 33, 19, 10, 16, 
22, 2, 11, 2, 18, 21, 24, 9))

str(data.br)


## ----L7S35_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------------------------------------------
model.1 <- "
model
  {
    for (i in 1 : N) {
      y[i] ~ dpois(lambda[i])
      lambda[i]  <- theta[i]  * n.birth[i]
      log(theta[i]) <- b0 + b1*(HT[i]-mean(HT[]))  
    }
    b0 ~ dnorm(0,0.001)
    b1 ~ dnorm(0,0.001)
    eff <- exp(b1)
  }
"
jags.1 <- jags.model(textConnection(model.1),data=data.br, n.adapt=1500)
update(jags.1, 10000)
test.1 <- coda.samples(jags.1, c('b0','b1','eff'), n.adapt=1500, n.iter=10000)


## ----L7S35_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '80%', fig.align="center"-------------------------
par(mgp=c(1.7,0.2,0),mar=c(3,2,3,1))
par(mfrow = c(4,1))
plot(test.1)


## ----L7S35_3, echo = FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
summary(test.1)


## ----L7S36_1, echo = FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
summary(test.1)


## ----L7S36_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '80%', fig.align="center"-------------------------
mod.1 <- glm(data.br$y~data.br$HT, family=poisson, offset=log(data.br$n.birt) )
  summary(mod.1)


## ----L7S41_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------------------------------------------
model.1 <- "
model
  {
    for (i in 1 : N) {
      theta[i] ~ dgamma(alpha, beta)
      lambda[i]  <- theta[i]  * t[i]
      x[i] ~ dpois(lambda[i])  
    }
    alpha ~ dexp(1)
    beta ~ dgamma(0.1, 1.0)
    theta.mean <- alpha/beta
  }
"
data <- list(t = c(94.3, 15.7, 62.9, 126, 5.24, 31.4, 1.05, 1.05, 2.1, 10.5),
             x = c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22), N = 10)

jags.1 <- jags.model(textConnection(model.1),data=data, n.adapt=1500,
                     inits = list(alpha = 1, beta = 1))
update(jags.1, 10000)
test.1 <- coda.samples(jags.1, c('alpha','beta','theta', 'theta.mean'), n.adapt=1500, n.iter=10000)


## ----L7S42_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '80%', out.width = '80%', fig.align="center"-------------------------
summary(test.1)[[1]]; summary(test.1)[[2]] 


## ----L7S42_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '80%', out.width = '80%', fig.align="center"-------------------------
par(mfrow = c(2,2))
autocorr.plot(test.1[, c("theta[1]", "theta[2]", "theta[3]", "theta[4]")])


## ----L7S45_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------------------------------------------
model.1 <- "
model
  {
    for (i in 1 : 2) {
      for(j in 1:2){
        n[i, j] ~ dpois (theta[i, j])
        log(theta[i, j]) <- mu + alpha[i] + beta[j] + gamma[i, j]
      }
    }
    # Prior
    mu ~ dnorm(0, 0.1)
    alpha[1] <- 0; alpha[2] ~ dnorm(0, 0.1)
    beta[1] <- 0; beta[2] ~ dnorm(0, 0.1)
    gamma[1, 1] <- 0; gamma[1, 2] <- 0;
    gamma[2, 1] <- 0; gamma[2, 2] ~ dnorm(0, 0.1)
  }
"
data <- list(n = structure(.Data = c(51, 992, 41, 245), .Dim = c(2, 2)))

jags.1 <- jags.model(textConnection(model.1),data=data, n.adapt=1500)
update(jags.1, 10000)
test.1 <- coda.samples(jags.1, c('mu', 'alpha', 'beta', 'gamma'), n.adapt=1500, n.iter=10000)


## ----L7S46_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '80%', out.width = '70%', fig.align="center"-------------------------
summary(test.1)[[1]]


## ----L7S46_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '80%', out.width = '70%', fig.align="center"-------------------------
summary(test.1)[[2]]

gamma22 <- round(as.vector(summary(test.1)[[2]]["gamma[2,2]", c("2.5%", "50%", "97.5%")]), 4)


## -------------------------------------------------------------------------------------------------------------------------------------------
options(width = 300)
leuktrt <- read.csv('leuktrt.csv'); 
leuktrt$logwk<-log(leuktrt$tweeks)
leuktrt$group<-factor(leuktrt$group)
library(survival)
fit<-survfit(Surv(tweeks, 1-censored) ~ group,data=leuktrt);
cat('\n Estimate and Compare time to relapse curves using Log-Rank test \n');
 survdiff(Surv(tweeks, 1-censored) ~ group,data=leuktrt)
summary(fit)$table


## ---- message=FALSE, warning=FALSE, fig.align="center",fig.width=4,fig.height=2.5, tidy = FALSE---------------------------------------------
options(width = 300)
  par(mar=c(2,1,1,0),mgp=c(0.6,0.1,0),cex.lab=0.5,cex.axis=0.5,cex.main=0.5, tck=-0.02)
a=plot(fit, mark.time=TRUE, mark=c(16),
        xlab='Weeks', ylab='Survival, %',lty=c(1,2),lwd=c(2),cex=.1,col=c(1,2),axes=FALSE,conf.int=FALSE)
axis(1)
axis(2, at=seq(0, 1, .2), labels=paste(seq(0, 100, 20),'%',sep=''),las=1)


## ---- echo=FALSE,eval=FALSE-----------------------------------------------------------------------------------------------------------------
## cat('\n Compare time to relapse by assuming an Exponential distribution - psm() function \n');
## psm(Surv(tweeks,1-censored) ~ group, dist = 'exponential',data=leuktrt) ;
## glm(relapse~group,family=poisson(), data=leuktrt,offset=logwk);


## ----echo=F---------------------------------------------------------------------------------------------------------------------------------
D<-read.csv('larynx.csv')
E<-table(D$delta)


## -------------------------------------------------------------------------------------------------------------------------------------------
 D<-read.csv('larynx.csv')
# New time
 D$new.time <-D$time
 D$new.time[D$delta == 0] <- NA # Censored
# censoring times
 D$cens <- D$time
 D$is.censored <- 1 - D$delta
 D$stage <- as.factor(D$stage)
 D$S2 <- 1*(D$stage==2); D$S3 <- 1*(D$stage==3); D$S4 <- 1*(D$stage==4)
#JAGS data and initial values
 dataj <- list(n = nrow(D), time = D$new.time, cens = D$cens, S2 = D$S2,S3 = D$S3,S4 = D$S4,
               age=D$age, diagyr=D$diagyr, is.censored = D$is.censored)
 initj <- list(time=ifelse(D$delta==0,D$time+1,NA),b=rep(0,6), alpha=1)


## -------------------------------------------------------------------------------------------------------------------------------------------
Model <- 'model{
# Survival and censoring times
 for (i in 1:n){
  is.censored[i] ~ dinterval(time[i],cens[i]) 
  time[i] ~ dweib(alpha,lambda[i])
  lambda[i] <- exp(-mu[i]*alpha)
   mu[i] <- b[1] + b[2]*S2[i] + b[3]*S3[i] + b[4]*S4[i] +
                 b[5]*(age[i]-mean(age[])) + b[6]*(diagyr[i]-mean(diagyr[]))
}
  alpha ~ dunif(0,10)
  for (p in 1:6) {
  b[p]~dnorm(0,0.001)
  }
}'


## ----cache=T,eval=F-------------------------------------------------------------------------------------------------------------------------
## library(rjags)
## jags.1 <- jags.model(textConnection(Model),data=dataj , inits = initj,n.adapt=1500,n.chains = 3 )
## update(jags.1,1500)
## test.1 <- coda.samples(jags.1, c('b','alpha'),
##                        n.adapt=1500, n.iter=50000,thin =10 )
## summary(test.1)


## ----cache=T,echo=F-------------------------------------------------------------------------------------------------------------------------
library(rjags)
jags.1 <- jags.model(textConnection(Model),data=dataj , inits = initj,n.adapt=1500,n.chains = 3 )
update(jags.1,1500)
test.1 <- coda.samples(jags.1, c('b','alpha'),
                       n.adapt=1500, n.iter=50000,thin =10 )
summary(test.1)


## ----echo=F---------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(2,4))
 traceplot(test.1)


## ----echo=F---------------------------------------------------------------------------------------------------------------------------------
 gelman.diag(test.1)


## ----L7S15_1, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------
 data <- read.csv("FHS.data.csv", header=T)
 summary(data)
 data$total[1:10]


## ----L7S16_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------------------------------------------
model.all <-"
  model{
  for (i in 1:N){
    total[i]~dnorm(mu[i], tau)
    mu[i]<-b0+b1*MALE[i]+b2*(AGE[i]-mean(AGE[]))+b3*(BMI[i]-mean(BMI[]))
  }
  #prior
  tau~dgamma(1,1)
  b0~dnorm(0,0.0001)
  b1~dnorm(0,0.0001)
  b2~dnorm(0,0.0001)
  b3~dnorm(0,0.0001)
  }
"


## ----L7S16_2, results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------------------------------------------
data.c <- na.omit(data)              ### Exclude missing data
FHS_data <- list(N=nrow(data.c), total=data.c$total,MALE=data.c$male,BMI=data.c$bmi,AGE=data.c$age,
                 m=as.numeric(is.na(data.c$total)==T))

jags.1 <- jags.model(textConnection(model.all),data=FHS_data, n.adapt=1500)
update(jags.1, 10000)
test.1 <- coda.samples(jags.1, c('b0','b1','b2','b3'), n.adapt=1500, n.iter=10000)

   plot(test.1)
   autocorr.plot(test.1)
   geweke.diag(test.1, frac1=0.1, frac2=0.5)
   summary(test.1)


## ----L7S17_1, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------
  summary(test.1)


## ----L7S17_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '90%', out.width = '90%', fig.align="center"-------------------------
par(mgp=c(1.7,0.2,0),mar=c(3,2,3,1))
    plot(test.1)


## ----L7S18_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------------------------------------------
  FHS_data <- list(N=nrow(data), total=data$total,MALE=data$male,BMI=data$bmi,AGE=data$age,
                 m=as.numeric(is.na(data$total)==T))

  jags.1 <- jags.model(textConnection(model.all),data=FHS_data, n.adapt=1500)
  update(jags.1, 10000)
  test.1 <- coda.samples(jags.1, c('b0','b1','b2','b3','total[3]'),
                       n.adapt=1500, n.iter=10000)


## ----L7S18_2, echo = FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
  summary(test.1)[[1]]; summary(test.1)[[2]]; 


## ----L7S18_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '80%', fig.align="center"-------------------------
par(mgp=c(1.7,0.2,0),mar=c(3,2,3,1))
  plot(test.1[,c("b0", "b1", "b2")])


## ----L7S23_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------------------------------------------
model.all <-"
  model{
  for (i in 1:N){
    BMIc[i] ~ dnorm(theta[i], tau2)
    theta[i] <- a0 + a1 * MALE[i] + a2*AGEc[i]
    total[i]~dnorm(mu[i], tau)
    mu[i]<-b0+b1*MALE[i]+b2*AGEc[i]+b3*BMIc[i]
  }
  #prior
  tau~dgamma(1,1); tau2~dgamma(1,1)
  b0~dnorm(0,0.0001); b1~dnorm(0,0.0001); b2~dnorm(0,0.0001); b3~dnorm(0,0.0001)
  a0~dnorm(0,0.0001); a1~dnorm(0,0.0001); a2~dnorm(0,0.0001)
  }
"


## ----L7S23_2, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE,cache=TRUE--------------------------------------
FHS_data <- list(N=nrow(data), total=data$total,MALE=data$male,BMI=data$bmi,BMIc = data$bmi - mean(data$bmi), AGE=data$age, AGEc=data$age - mean(data$age), 
                 m=as.numeric(is.na(data$total)==T))

jags.1 <- jags.model(textConnection(model.all),data=FHS_data, n.adapt=1500)
update(jags.1, 10000)
test.1 <- coda.samples(jags.1, c('a0', 'a1', 'a2', 'b0','b1','b2','b3'),
                       n.adapt=1500, n.iter=10000)

summ.test1 <- summary(test.1)


## ----L7S23_3, echo = FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
summ.test1[[1]]; summ.test1[[2]]


## ----L7S50_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------------------------------------------
model.1 <- "
model
  {
    for(i in 1:N){
      y[i] ~ dnorm(mu[i], inv.sigma2) 
      mu[i] <- alpha - beta * pow(gamma, x[i])
    }
    alpha ~ dunif(0, 100)
    beta ~ dunif(0, 100)
    gamma ~ dunif(0, 1)
    inv.sigma2 <- 1/pow(sigma, 2)
    log(sigma) <- log.sigma
    log.sigma ~ dunif(-10, 10)
  }
"
# Inits
inits <- list(alpha = 3, beta = 2, gamma = 0.9, log.sigma = -5)

data <- list(x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
                   8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
                   13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5, 35, 40), 
             y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47, 
                   2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
                   2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57, NA, NA),
             N = 29)


## ----L7S51_1, echo = FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
jags.1 <- jags.model(textConnection(model.1),data=data, inits = inits, n.adapt=1500)
update(jags.1, 10000)
test.1 <- coda.samples(jags.1, c('alpha', 'beta', 'gamma', 'y[28]','y[29]'), n.adapt=1500, n.iter=10000)
summary(test.1)

