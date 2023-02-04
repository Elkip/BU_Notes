## ----prep work, echo = FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------
library("knitr")
library("rjags")
library("coda")
library("formatR")
library(ggplot2)
library("dplyr")
# Prevent code chunks out of range
# Use tidy = FALSE in each code chunk to disable this option
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
#setwd("/Users/xiaoyanliu/Documents/BU_Courses/BS849_Bayesian/Lecture3")


## ---- echo=FALSE-----------------------------------------------------------------------------------------------------
  data.1 <- source("saudi.data.2.txt")[[1]]
   mh <- round(mean(data.1$hbf),1)
   sdh <- round(sd(data.1$hbf),1)
   mn<-round((15/36+mh/sdh^2)/(1/36+1/sdh^2),2)
   pn <-round(1/36+1/sdh^2,2)


## ----L3S10_1, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE--------------------------------------
library(rjags)

model.1 <- 
  "model  {  
   for (i in 1:N)  {  
   hbf[i] ~ dnorm(b.0,tau.t)
                           }
 
 ## prior on precision parameters
     tau.t ~ dgamma(1,1);
 ### prior on mean given precision
  mu.0 <- 15
 tau.0 <- 1/36
    b.0 ~ dnorm(mu.0, tau.0);
    }
"
 data.1 <- source("saudi.data.2.txt")[[1]]

 model_mean <- jags.model(textConnection(model.1), data=data.1, n.adapt=1000)
 update(model_mean,10000)
 test_mean  <- coda.samples(model_mean, c('b.0'), n.iter = 10000)
 summary(test_mean)
 plot(test_mean)


## ----L3S10_2, eval = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
##  data.1.example <- list(N = 283, HU = c(1, 2, 1, 2, 1, 2, ...),
##                         sex = c(2, 1, 2, 2, 2, 2,...),...)


## ----L3S10_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '20%', out.width = '70%', fig.align="center"----
plot(test_mean)
summary(test_mean)


## ----L3S11_1, eval = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
## model.1 <-
##   "model  {
##    for (i in 1:N)  {
##    hbf[i] ~ dnorm(b.0,tau.t)
##                            }
## 
##  ## prior on precision parameters
##      tau.t ~ dgamma(1,1);
##  ### prior on mean given precision
##   mu.0 <- 5
##  tau.0 <- 0.44
##     b.0 ~ dnorm(mu.0, tau.0);
##     }
## "


## --------------------------------------------------------------------------------------------------------------------
  data.1 <- source("saudi.data.2.txt")[[1]]
   mh <- mean(data.1$hbf)  #data mean
   sdh <- sd(data.1$hbf)   # data sd
   mn<-(15/36+mh/sdh^2)/(1/36+1/sdh^2) # Posterior mean
   pn <- 1/36+1/sdh^2    #posterior precision


## ----echo=FALSE------------------------------------------------------------------------------------------------------
  set.seed(2000)

## --------------------------------------------------------------------------------------------------------------------
   sample.post <- rnorm(10000,mn,sqrt(1/pn)) # Posterior samples


## ----echo=FALSE------------------------------------------------------------------------------------------------------
  set.seed(2000)

## --------------------------------------------------------------------------------------------------------------------
   Y.pred <- rnorm(10000,sample.post,sdh) # predictive samples
   mean(Y.pred>20)                       # Calculate probability Y_pred>20


## ----L3S13_1, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE--------------------------------------
model.1 <- 
  "model  {  
   for (i in 1:N)  {  
   hbf[i] ~ dnorm(b.0,tau.t)
                           }
 
 ## prior on precision parameters
   tau.t ~ dgamma(1,1);
 ### prior on mean given precision
  mu.0 <- 15
 tau.0 <- 0.028
    b.0 ~ dnorm(mu.0, tau.0);

### prediction
  hbf.new  ~ dnorm(b.0,tau.t)
  pred <- step(hbf.new-20)
    }
"


## ----L3S13_2, results = "hide", echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------
 model_mean <- jags.model(textConnection(model.1), data=data.1, n.adapt = 1000)
 update(model_mean,10000)
 test_mean  <- coda.samples(model_mean, c('b.0','hbf.new','pred'), n.iter = 100000)


## ----L3S13_4, echo = FALSE, message=FALSE, warning=FALSE, out.height = '20%', out.width = '70%', fig.align="center"----
densplot(test_mean[,2], main = "Density of hbf.new")


## ----L3S13_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test_mean)


## ----L3S15_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '70%', out.width = '70%', fig.align="center"----
hist(data.1$hbf)


## ----L3S15_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '70%', out.width = '70%', fig.align="center"----
densplot(test_mean[,2], main = "Density of hbf.new")


## ----L3S16_1, echo = FALSE, message=FALSE, warning=FALSE, out.width='80%', fig.align='center', echo=FALSE------------
par(mfrow=c(1,1),mar=c(3.2,3.7,1,1),mgp=c(2,0.7,0),cex.lab=1.4,cex.axis=1.4)
y <- seq(0, 10, by = 0.01)
z <- dlnorm(y)
plot(y, z, type = 'l',xlab='Variable, y',ylab=expression('Density lNormal('*eta*','*sigma^2*')'),lwd=2,axes=F)
axis(1);axis(2)
legend('center',legend=c(expression(eta==0*','~sigma^2==1)),bty='n',cex=1.4)


## ----L3S17_1, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE,tidy=FALSE---------------------------
model.1 <- 
  "model  {  
   for (i in 1:N)  {  
   hbf[i] ~ dlnorm(lb.0,tau.t)
                           }
 
 ## prior on precision parameters
     tau.t ~ dgamma(1,1);
 ### prior on mean  
     lb.0 ~ dnorm(1.6,1.6);
     b.0 <- exp(lb.0)
### prediction
  hbf.new  ~ dlnorm(lb.0,tau.t)
  pred <- step(hbf.new-20)
    }
"

model_mean<-jags.model(textConnection(model.1),
                  data=data.1, n.adapt = 1000)
 update(model_mean,10000)
 test_mean  <- coda.samples(model_mean, 
               c('b.0','hbf.new','pred'), 
               n.iter = 10000)
 summary(test_mean)
 plot(test_mean)


## ----L3S18_1, message=FALSE, warning=FALSE---------------------------------------------------------------------------
summary(test_mean)


## ----L3S18_2, message=FALSE, warning=FALSE, out.height = '50%', out.width = '80%', fig.align="center"----------------
plot(test_mean[,c(1,2)])


## ----L3S19_1, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE--------------------------------------
model.1.noninfo <- "model  {  
   for (i in 1:N)  {  
   hbf[i] ~ dlnorm(lb.0,tau.t)
                           }
 
 ## prior on precision parameters
     tau.t ~ dgamma(1,1);
 ### prior on mean  
    lb.0 ~ dnorm(1,0.0001);
     b.0 <- exp(lb.0)
### prediction
  hbf.new  ~ dlnorm(lb.0,tau.t)
  pred <- step(hbf.new-20)
    }"

 model_mean.noninfo <- jags.model(textConnection(model.1.noninfo), data=data.1, n.adapt = 1000)
 update(model_mean.noninfo,10000)
 test_mean.noninfo  <- coda.samples(model_mean.noninfo, c('b.0','hbf.new','pred'), n.iter = 10000)
 summary(test_mean.noninfo)
 plot(test_mean.noninfo)


## ----L3S20_1, message=FALSE, warning=FALSE---------------------------------------------------------------------------
summary(test_mean.noninfo)


## ----L3S20_2, message=FALSE, warning=FALSE---------------------------------------------------------------------------
summary(test_mean)


## ----echo=FALSE------------------------------------------------------------------------------------------------------
table(data.1$rs766432)


## ---- echo = FALSE, message=FALSE, warning=FALSE, out.width='80%', fig.align='center', echo=FALSE--------------------
par(mfrow=c(1,1),mar=c(3.2,3.7,1,1),mgp=c(2,0.7,0),cex.lab=1.4,cex.axis=1.4)
data.1 <- source("saudi.data.2.txt")[[1]]

boxplot(data.1$hbf ~ data.1$rs766432, names=c("HM","HT","NP"), xlab="", ylab="HbF Values")


## ----L3S25_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE,cache=T--------------------------------
model.1 <-"model  {
   for (i in 1:N)  {
   hbf[i] ~ dlnorm(mu[i],tau.t)
       mu[i] <- b.0+b.1 *equals(rs766432[i],2)+b.2 *equals(rs766432[i],3)
                              }
 ## prior on precision parameters
     tau.t ~ dgamma(1,1);
     
 ### prior on mean given precision
     b.0 ~ dnorm(0, 0.001);
     b.1 ~ dnorm(0, 0.001);
     b.2 ~ dnorm(0, 0.001);
    
### prediction
  lmu.1 <- b.0;     hbf.1  ~ dlnorm( lmu.1,tau.t); pred.1 <- step(hbf.1-20)
  lmu.2 <- b.0+b.1; hbf.2  ~ dlnorm( lmu.2,tau.t); pred.2 <- step(hbf.2-20)
  lmu.3 <- b.0+b.2; hbf.3  ~ dlnorm( lmu.3,tau.t); pred.3 <- step(hbf.3-20)
  
### fitted medians by genotypes
  mu.1 <- exp(lmu.1)
  mu.2 <- exp(lmu.2)
  mu.3 <- exp(lmu.3)
  
 par.b[1] <- b.0;    par.b[2] <- b.1;    par.b[3] <- b.2
 par.h[1] <- hbf.1;  par.h[2] <- hbf.2;  par.h[3] <- hbf.3;
 par.m[1] <- mu.1;   par.m[2] <- mu.2;   par.m[3] <- mu.3
 par.p[1] <- pred.1; par.p[2] <- pred.2; par.p[3] <- pred.3
    }"
 data.1 <- source("saudi.data.2.txt")[[1]]

 model_hbf <- jags.model(textConnection(model.1), data=data.1, n.adapt = 1000)
 update(model_hbf, 10000)
 test_hbf  <- coda.samples(model_hbf, c('par.b','par.h','par.m','par.p'),
                           n.iter = 10000)
 summary(test_hbf)
 plot(test_hbf)
 autocorr.plot(test_hbf)


## ----L3S26_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '100%', fig.align="center"----
autocorr.plot(test_hbf)


## ----L3S25_1l, results = "hide", fig.show="hide", message=FALSE, warning=FALSE,cache=T-------------------------------
 model_hbf <- jags.model(textConnection(model.1), data=data.1, n.adapt = 1000)
 update(model_hbf, 10000)
 test_hbf  <- coda.samples(model_hbf, c('par.b','par.h','par.m','par.p'),n.iter = 100000, thin = 30)


## ----L3S26_1l, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '100%', fig.align="center"----
autocorr.plot(test_hbf)


## ----L3S27_1, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test_hbf)[[2]]


## ----L3S28_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
library(rjags)

model.1 <- "model  {  
   for (i in 1:N)  {  
   hbf[i] ~ dlnorm(mu[i],tau.t)
       mu[i] <- b.0+b.2 *equals(rs766432[i],3)
                              }
 ## prior on precision parameters
     tau.t ~ dgamma(1,1);
 ### prior on mean given precision
     b.0 ~ dnorm(0, 0.001);
     b.2 ~ dnorm(0, 0.001);
### prediction
  lmu.1 <- (b.0); hbf.1  ~ dlnorm( lmu.1,tau.t); pred.1 <- step(hbf.1-20)
  lmu.3 <- (b.0+b.2); hbf.3  ~ dlnorm( lmu.3,tau.t); pred.3 <- step(hbf.3-20)
### fitted means by genotypes
  mu.1 <- exp(lmu.1)
  mu.3 <- exp(lmu.3)
 par.b[1]  <- b.0;    par.b[2] <- b.2;
 par.h[1] <- hbf.1;   par.h[2] <- hbf.3; 
 par.m[1] <- mu.1;    par.m[2] <- mu.3
 par.p[1] <- pred.1;  par.p[2] <- pred.3
  }"
 data.1 <- source("saudi.data.2.txt")[[1]]

 model_hbf <- jags.model(textConnection(model.1), data=data.1, n.adapt = 1000)
 update(model_hbf, 10000)
 test_hbf  <- coda.samples(model_hbf, c('par.b','par.h','par.m','par.p'), 
                           n.iter = 10000) 
 summary(test_hbf)  
 plot(test_hbf)
 autocorr.plot(test_hbf)


## ----L3S28_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '90%', fig.align="center"----
par(mfrow = c(2,1)) 
autocorr.plot(test_hbf[,c(1:6)])


## ----L3S29_1, message=FALSE, warning=FALSE, out.height = '15%', out.width = '80%', fig.align="center"----------------
test_hbf1  <- coda.samples(model_hbf, c('par.b', 'par.h', 'par.m', 'par.p'),  
                          thin = 5, n.iter = 20000)
autocorr.plot(test_hbf1[,c(1,2)])


## ----L3S29_2, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
summary(test_hbf1)[[2]]


## ----L3S29_2l, echo = FALSE, message=FALSE, warning=FALSE------------------------------------------------------------
summary(test_hbf)[[2]]


## ----L3S32_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '60%', fig.align="center"----
 data.1 <- list( N=25,
lev.20 =  c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
lev.25 =  c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0),
lev.30 =  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0),
lev.35 =  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1),
y = c(7,7,15,11,9,12,17,12,18,18, 14,18,18,19,19,
         19,25,22,19,23,7,10,11,15,11))

data.anova <- data.frame("y" = data.1$y, 
                         "lev20" = data.1$lev.20, "lev25" = data.1$lev.25, 
                         "lev30" = data.1$lev.30, "lev35" = data.1$lev.35) %>%
              mutate(cat = case_when(lev20 == 1 ~ "20%",
                                     lev25 == 1 ~ "25%",
                                     lev30 == 1 ~ "30%",
                                     lev35 == 1 ~ "35%",
                                     TRUE ~ "15%"))

means <- tapply(data.anova$y,data.anova$cat,mean)
par(cex.lab=1.5,cex.axis=1.5)
boxplot(data.anova$y ~ data.anova$cat, xlab="", ylab="", main = "Boxplot of 15% ~ 35% \n (means are indicated by solid circle)",axes=F) ;
axis(1,at=1:5,labels=c('15%', '20%', '25%', '30%' , '35%'));axis(2)
points(means, pch = 16)


## ----L3S37_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
model.1 <- 
  "model {
### data model
  for(i in 1:N){
   y[i] ~dnorm(mu[i], tau)
  mu[i] <- b.15 + b.20*lev.20[i] +b.25 *lev.25[i] + 
                           b.30*lev.30[i] +b.35 * lev.35[i] }

###   prior
         b.15 ~dnorm(0,0.0001); ## referent group     
         b.20 ~dnorm(0,0.0001);      
         b.25 ~dnorm(0,0.0001);       
         b.30 ~dnorm(0,0.0001);       
         b.35 ~dnorm(0,0.0001);     
         tau  ~dgamma(1,1)  
### difference in strength between level 3 (25%) and level 4 (30%)
   b.30.25 <- b.30-b.25

### estimated strength in groups (30%)
   strength[1] <- b.15  
   strength[2] <- strength[1]+b.20 
   strength[3] <- strength[1]+b.25 
   strength[4] <- strength[1]+b.30 
   strength[5] <- strength[1]+b.35 
    }
"


## ----L3S37_2, eval = FALSE,results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------
##  data.1 <- list(     N =  25,
##                 lev.20 =  c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
##                 lev.25 =  c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0),
##                 lev.30 =  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0),
##                 lev.35 =  c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1),
##                 y      =  c(7,7,15,11,9,12,17,12,18,18,14,18,18,19,19,
##                             19,25,22,19,23,7,10,11,15,11))


## ----L3S38_1, echo = FALSE, results = 'hide', message=FALSE, warning=FALSE, out.height = '30%', out.width = '70%', fig.align="center"----
 model_mean <- jags.model(textConnection(model.1), data=data.1, n.adapt=1000)
 update(model_mean, 10000)
 test_mean  <- coda.samples(model_mean, c('b.15','b.20','b.25','b.30','b.35', 'strength'), n.iter = 10000)
 par(mfrow = c(1,2))
 autocorr.plot(test_mean[,c(1,2)])


## ----L3S38_2, echo = FALSE, results = 'hide', message=FALSE, warning=FALSE, out.height = '30%', out.width = '70%', fig.align="center"----
 test_mean  <- coda.samples(model_mean, c('b.15','b.20','b.25','b.30','b.35', 'strength','b.30.25'), n.iter = 20000,thin=5)
 par(mfrow = c(1,2))
 autocorr.plot(test_mean[,c(1,2)])


## ----L3S38_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '85%', out.width = '85%', fig.align="center"----
knitr::kable(summary(test_mean)[[2]])

strength.max <- round(summary(test_mean)[[2]]["strength[4]", "50%"], 1)
ci.max <- round(summary(test_mean)[[2]]["strength[4]", c("2.5%", "97.5%")], 1)

strength.3rd <- round(summary(test_mean)[[2]]["strength[3]", "50%"], 1)
ci.3rd <- round(summary(test_mean)[[2]]["strength[3]", c("2.5%", "97.5%")], 1)

strength.diff <- round(summary(test_mean)[[2]]["b.30.25", "50%"], 1)
ci.diff <- round(summary(test_mean)[[2]]["b.30.25", c("2.5%", "97.5%")], 1)


## ----L3S39_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '40%', fig.align="center"----
out <- as.matrix(test_mean)
boxplot(out[,7:11])


## ----L3S42_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '70%', fig.align="center"----
 ### read data 
  hbf.data <- read.csv("msh.data.3.09.csv",header=T)
  hbf_after <- hbf.data$hbfp_2y
  hbf_baseline <- hbf.data$hbfp_bl
  treatment <- hbf.data$Drug;
  par(mfrow=c(1,2),mar=c(3.2,3.7,1,1),mgp=c(2,0.7,0),cex.lab=1.4,cex.axis=1.4);
  hist(hbf_baseline,xlab='Baseline HbF',main='');    hist(hbf_after,xlab='Follow-up HbF',main='')
  
  par(mfrow=c(1,1),mar=c(3.2,3.7,1,1),mgp=c(2,0.7,0),cex.lab=1.4,cex.axis=1.4)
  plot( hbf_baseline, hbf_after, xlab='Baseline HbF',ylab='Follow-up HbF')
  points( hbf_baseline[ treatment=="Hy"], hbf_after[ treatment=="Hy"], col=2)


## ----L3S42_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '70%', fig.align="center"----
  par(mfrow=c(1,2),mar=c(3.2,3.7,1,1),mgp=c(2,0.7,0),cex.lab=1.4,cex.axis=1.4);
  hist(log(hbf_baseline), xlab='Log Baseline HbF',main='');    hist(log(hbf_after), xlab='Log Follow-up HbF',main='')

  par(mfrow=c(1,1),mar=c(3.2,3.7,1,1),mgp=c(2,0.7,0),cex.lab=1.4,cex.axis=1.4);
  plot( log(hbf_baseline), log(hbf_after), xlab='Log Baseline HbF',ylab='Log Follow-up HbF')
  points( log(hbf_baseline[ treatment=="Hy"]), log(hbf_after[ treatment=="Hy"]), col=2)


## ----L3S45_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
model.1 <- 
  "model{
### data model
   for(i in 1:N){
     hbf_after[i] ~dlnorm(mu[i],tau)
       Lhbf_baseline[i] <- log(hbf_baseline[i])
       mu[i] <- beta.0 + beta.d*Drug[i] + 
                                   beta.b*(Lhbf_baseline[i]-mean(Lhbf_baseline[])) + 
                                   beta.i*Drug[i] *(Lhbf_baseline[i]-mean(Lhbf_baseline[])) 
                       }
  ### prior density
     beta.0 ~ dnorm(0,0.0001)
     beta.d ~dnorm(0, 0.0001)
     beta.b ~dnorm(0, 0.0001)
      beta.i ~dnorm(0,0.0001)
       tau ~ dgamma(1,1);

### inference
  parameter[1] <- beta.0
  parameter[2] <- beta.d
  parameter[3] <- beta.b
  parameter[4] <- beta.i
    }"
### generate data  
   Drug = rep(0,nrow(hbf.data))
   Drug[ treatment=="Hy"] <- 1; table(Drug, hbf.data$Drug)
 data.1 <- list(
     N = as.numeric(nrow(hbf.data)),
     hbf_baseline = hbf_baseline ,
     hbf_after = hbf_after,
     Drug= Drug)

 model_mean <- jags.model(textConnection(model.1), data=data.1, n.adapt=1000)
 update(model_mean, 10000)
 test_mean  <- coda.samples(model_mean, c('parameter'), n.iter = 10000)


## ----L3S46_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
 summary(test_mean)

param4 <- as.vector(round(summary(test_mean)[[2]]["parameter[4]", c("2.5%", "50%", "97.5%")], 2))

params <- as.vector(round(summary(test_mean)[[2]][, "50%"], 2))
m <- round(mean(log(hbf_baseline)),2)


## ----L3S47_1, message=FALSE, warning=FALSE---------------------------------------------------------------------------
 mean(log(hbf_baseline))


## ----L3S47_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
 summary(test_mean)[2]


## ----L3S48_1, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE--------------------------
model.1 <- 
  "model{
### data model
   for(i in 1:N){
     hbf_after[i] ~dlnorm(mu[i],tau)
       Lhbf_baseline[i] <- log(hbf_baseline[i])
       mu[i] <- beta.0 + beta.d*Drug[i] + 
                                   beta.b*(Lhbf_baseline[i]-mean(Lhbf_baseline[])) + 
                                   beta.i*Drug[i] *(Lhbf_baseline[i]-mean(Lhbf_baseline[])) 
                       }
  ### prior density
     beta.0 ~ dnorm(0,0.0001)
     beta.d ~dnorm(0, 0.0001)
     beta.b ~dnorm(0, 0.0001)
      beta.i ~dnorm(0,0.0001)
       tau ~ dgamma(1,1);

### Vector of Parameters
  parameter[1] <- beta.0
  parameter[2] <- beta.d
  parameter[3] <- beta.b
  parameter[4] <- beta.i

### Estimation of Median HbF
   for(i in 1:10){
       unt.HbF[i] <- exp( beta.0 + beta.b*(log(hbf.pre[i])-mean(Lhbf_baseline[])) )
       treat.HbF[i] <- exp( beta.0 + beta.d+(beta.b+beta.i)*(log(hbf.pre[i])-mean(Lhbf_baseline[])) )
                                   }
  prob <- step( treat.HbF[2]-5)
    }
"

 data.1 <- list(hbf.pre = c(1,2,3,4,5,6,7,8,9,10),
     N = as.numeric(nrow(hbf.data)),
     hbf_baseline = hbf_baseline ,
     hbf_after = hbf_after,
      Drug= Drug)
### generate data  
 model_mean <- jags.model(textConnection(model.1), data=data.1, n.adapt=1000)
 update(model_mean, 10000)
 test_mean  <- coda.samples(model_mean, c('unt.HbF', 'treat.HbF'), n.iter = 20000,thin=5)

 out <- as.matrix(test_mean)
 boxplot(out)


## ----L3S48_2, eval = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE--------------------------
## "#### Prediction
##    for(i in 1:10){
##        unt.HbF[i] <- exp( beta.0 + beta.b*(log(hbf.pre[i])-mean(Lhbf_baseline[])) )
##        treat.HbF[i] <- exp( beta.0 + beta.d+(beta.b+beta.i)*(log(hbf.pre[i])-mean(Lhbf_baseline[])) )
##                                    }
##   prob <- step( treat.HbF[2]-5)
##     }
## "
## ### generate data
## test_mean  <- coda.samples(model_mean, c('unt.HbF', 'treat.HbF'), n.iter = 20000, thin=5)
##  summary(test_mean)
## 
##  out <- as.matrix(test_mean)
##  boxplot(out)


## ----L3S48_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
 summary(test_mean)[[2]]


## ----L3S48_4, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '50%', fig.align="center"----
 boxplot(out)


## ----L3S50_1, results = "hide", fig.show="hide", echo = FALSE, message=FALSE, warning=FALSE--------------------------
hdl.data <- read.table("hdl-data.txt",sep="\t",header=T)
   hdl.data$BMI <- hdl.data$BMI5
   new.hdl.data <- na.omit(hdl.data)

  ## generate data in format for jags
 data.fhs <- list(N = nrow(new.hdl.data),
           hdl=as.numeric(new.hdl.data[,1]),
           age=as.numeric(new.hdl.data$AGE5),
           sex=as.numeric(new.hdl.data$SEX),bmi=new.hdl.data$BMI)


## ----L3S50_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '20%', out.width = '70%', fig.align="center"----
par(mfrow=c(1,2))
  hist(new.hdl.data[,1],main="")
  hist(log(new.hdl.data[,1]),main="")


## ----L3S50_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '40%', out.width = '70%', fig.align="center"----
par(mfrow=c(1,2))
  plot(new.hdl.data)


## ----L3S52_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
 model.fhs <- "
model  {  
## likelihood 
   for (i in 1:N)  {  
    hdl[i] ~ dnorm(mu[i],tau)
   mu[i] <- beta.0+beta.age*(age[i]-mean.age)+
              beta.sex*(sex[i]-1)+
              beta.bmi*(bmi[i]-mean.bmi) }
  
 ##prior
                  tau ~ dgamma(1,1);
            beta.0 ~ dnorm(0, 0.0001);
        beta.age ~ dnorm(0, 0.0001);
  beta.sex ~  dnorm(0, 0.0001);
  beta.bmi ~ dnorm(0, 0.0001);
###
### predicted HDL in females with age 50 and bmi 25
 mean.hdl.F <- beta.0+beta.age*(50-mean.age)+ 
                beta.bmi*(25-mean.bmi) 
 mean.hdl.M <- beta.0+beta.age*(50-mean.age)+ 
                beta.bmi*(25-mean.bmi) +beta.sex

 mean.age <- mean(age[])
 mean.bmi <- mean(bmi[])  
   }"


## ----L3S53_1, results = "hide", fig.show = "hide", echo = FALSE, message=FALSE, warning=FALSE------------------------
 jags.fhs <- jags.model(textConnection(model.fhs),data=data.fhs, n.adapt=1500)
 update(jags.fhs, n.iter=1000)
 test.fhs <- coda.samples(jags.fhs, c('beta.0','beta.age','beta.sex','beta.bmi'),  n.iter=1000)


## ----L3S53_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '80%', fig.align="center"----
par(mfrow = c(2,2))
traceplot(test.fhs); 


## ----L3S53_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '80%', fig.align="center"----
autocorr.plot(test.fhs)


## ----L3S54_1, eval = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
## temp <- update(jags.fhs, n.iter=1000)
## test.fhs <- coda.samples(jags.fhs, c('beta.0','beta.age','beta.sex','beta.bmi'),  n.iter=1000)


## ----L3S54_2, results = "hide", echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------
 jags.fhs <- jags.model(textConnection(model.fhs),data=data.fhs, n.adapt=1500)
 update(jags.fhs, n.iter=1000)
 test.fhs <- coda.samples(jags.fhs, c('beta.0','beta.age','beta.sex','beta.bmi'),  n.iter=1000)


## ----L3S54_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '70%', fig.align="center"----
summary(test.fhs)


## ----L3S54_4, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '70%', fig.align="center"----
densplot(test.fhs[,2], main = "Density of beta.age")


## ----L3S55_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
 model.fhs.noage <- "
model  {  
## likelihood 
   for (i in 1:N)  {  
    hdl[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta.0+beta.sex*(sex[i]-1)+ beta.bmi*(bmi[i]-mean.bmi)
   }
 ##prior
  tau ~ dgamma(1,1);
  beta.0 ~ dnorm(0, 0.0001);
  beta.sex ~  dnorm(0, 0.0001);
  beta.bmi ~ dnorm(0, 0.0001);
###
### predicted HDL in females with  bmi 25
 mean.hdl.F <- beta.0+ beta.bmi*(25-mean.bmi) 
 mean.hdl.M <- beta.0+ beta.bmi*(25-mean.bmi) +beta.sex

  mean.bmi <- mean(bmi[])  
   }"


## ----L3S56_1, results = "hide", echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------
 jags.fhs.noage <- jags.model(textConnection(model.fhs.noage),data=data.fhs, n.adapt=1500)
 update(jags.fhs.noage, n.iter=1000)
 test.fhs.noage <- coda.samples(jags.fhs.noage, c('beta.0','beta.age','beta.sex','beta.bmi'),  n.iter=1000)


## ----L3S56_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
summary(test.fhs.noage)


## ----L3S56_3, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
summary(test.fhs)


## ----L3S58_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
 model.fhs.noage.res <- "
model  {  
## likelihood 
   for (i in 1:N)  {  
    hdl[i] ~ dnorm(mu[i],tau)
   mu[i] <- beta.0+beta.sex*(sex[i]-1)+ beta.bmi*(bmi[i]-mean.bmi)
   
  ## residuals
  r[i] <- hdl[i]-mu[i]
  sr[i] <- r[i]*sqrt(tau)}
  
 ##prior
  tau ~ dgamma(1,1);
  beta.0 ~ dnorm(0, 0.0001);
  beta.sex ~  dnorm(0, 0.0001);
  beta.bmi ~ dnorm(0, 0.0001);
###
### predicted HDL in females with  bmi 25
 mean.hdl.F <- beta.0+ beta.bmi*(25-mean.bmi) 
 mean.hdl.M <- beta.0+ beta.bmi*(25-mean.bmi) +beta.sex
  mean.bmi <- mean(bmi[])  
   }"


## ----echo=FALSE------------------------------------------------------------------------------------------------------
nn <-  data.fhs$N


## ----L3S59_1, results = "hide", echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------
 jags.fhs.noage.res <- jags.model(textConnection(model.fhs.noage.res),data=data.fhs, n.adapt=1500)
 update(jags.fhs.noage.res, 1000)
 test.fhs.noage.res <- coda.samples(jags.fhs.noage.res, c('r','sr'),  n.iter=1000)
 out <- as.matrix(test.fhs.noage.res)


## ----L3S59_2, eval = FALSE, results = "hide", message=FALSE, warning=FALSE-------------------------------------------
##  jags.fhs.noage.res <- jags.model(textConnection(model.fhs.noage.res),data=data.fhs, n.adapt=1500)
##  update(jags.fhs.noage.res, 1000)
##  test.fhs.noage.res <- coda.samples(jags.fhs.noage.res, c('r','sr'),  n.iter=1000)
##     out <- as.matrix(test.fhs.noage.res)
##     out[1:10,1:5]


## ----L3S59_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
out[1:10,1:5]


## ----L3S59_4, message=FALSE, warning=FALSE, out.height = '30%', out.width = '50%', fig.align="center"----------------
     hist(apply(out[,1:data.fhs$N],2,mean))


## ----L3S59_5, message=FALSE, warning=FALSE, out.height = '30%', out.width = '50%', fig.align="center"----------------
     hist(apply(out[, 3468:ncol(out)],2,mean))


## ----L3S60_1, message=FALSE, warning=FALSE, out.width='80%', fig.align='center', echo=FALSE--------------------------
par(mfrow=c(1,1),mar=c(3.2,3.7,1,1),mgp=c(2,0.7,0),cex.lab=1.4,cex.axis=1.4)
   qplot(new.hdl.data[,1], apply(out[, 3468:ncol(out)],2,median), xlab = "Observed Values, hdl[i]", ylab = "Standardized Residuals, Sr[i]")+theme_bw()


## ----L3S60_2, message=FALSE, warning=FALSE, out.width='80%', fig.align='center', echo=FALSE--------------------------
par(mfrow=c(1,1),mar=c(3.2,3.7,1,1),mgp=c(2,0.7,0),cex.lab=1.4,cex.axis=1.4)
   qplot(new.hdl.data[,5], apply(out[, 3468:ncol(out)],2,median),  xlab = "Observed Values, BMI[i]", ylab = "Standardized Residuals, Sr[i]")+theme_bw()+stat_smooth(method="loess",se=FALSE)


## ----L3S64_1, message=FALSE, warning=FALSE---------------------------------------------------------------------------
mod <- lm(HDL5 ~ AGE5 + BMI + SEX, data = new.hdl.data);
step(mod)


## ----L3S64_2, message=FALSE, warning=FALSE---------------------------------------------------------------------------
mod <- lm(HDL5 ~ AGE5 + BMI + SEX, data = new.hdl.data);
step(mod, k = log(nrow(new.hdl.data)))

