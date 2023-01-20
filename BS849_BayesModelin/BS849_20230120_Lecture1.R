## ----prep work, echo = FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------
library("knitr")
library("rjags")
library("coda")
library("formatR")
# Prevent code chunks out of range
# Use tidy = FALSE in each code chunk to disable this option
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)


## ----L1S21_1, message=FALSE, warning=FALSE, out.height = '70%', out.width = '70%', fig.align="center"----------------
 # Generate samples
   x <- rbinom(1000,8, 0.5) 
 # Represent histogram
     hist(x,main='')                


## ----L1S21_2, message=FALSE, warning=FALSE,eval=FALSE----------------------------------------------------------------
##  # Estimate P(X<=2) as
## 
##  # Proportion of samples <= 2
## 
##     sum(x <= 2)/1000
## 


## ----L1S23_1, results = 'hide', fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
library(rjags)

### model is defined as a string
model11.bug <-
  "model {
      Y ~ dbin(0.5, 8)
       P2 <- step(2 - Y)
    }"
writeLines(model11.bug,'model11.txt') 
# Now run the Gibbs sampling for 1000 iterations

### (Step 1) Compile BUGS model 
M11<-jags.model('model11.txt', n.chains = 1, n.adapt=1000)
### (Step 2) Generate 1000 samples and discard 
mcmc_11 <- update(M11, n.iter=1000)
### (Step 3) Generate 10000 samples and retain for inference 
test_11 <- coda.samples(M11, variable.names=c('P2','Y'), n.iter = 10000)


## ----L1S24_1, message=FALSE, warning=FALSE,eval=FALSE----------------------------------------------------------------
## model.bug <-
##   "model {
##       Y ~ dbin(0.5, 8)
##      P2 <- step(2 - Y)
##   }"
## writeLines(model.bug,'model.txt')


## ----L1S25_1, results = 'hide', message=FALSE, warning=FALSE,eval=FALSE----------------------------------------------
## # Now run the Gibbs sampling for 1000 iterations
## 
## ### (Step 1) Compile BUGS model
## M11<-jags.model('model11.txt', n.chains = 1, n.adapt=1000)
## ### (Step 2) Generate 1000 samples and discard
## mcmc_11 <- update(M11, n.iter=1000)
## ### (Step 3) Generate 10000 samples and retain for inference
## test_11 <- coda.samples(M11, c('P2','Y'), n.iter = 10000)


## ----L1S26_1, message=FALSE, warning=FALSE---------------------------------------------------------------------------
# Now run the Gibbs sampling for 1000 iterations

### (Step 1) Compile BUGS model 
M11<-jags.model('model11.txt', n.adapt=1000,quiet	=TRUE)
### (Step 2) Generate 1000 samples and discard 
mcmc_11 <- update(M11, n.iter=1000)
### (Step 3) Generate 10000 samples and retain for inference 
test_11 <- coda.samples(M11, c('P2','Y'), n.iter = 10000)

### Generate summary statistics on the samples
summary(test_11)


## ----L1S26_2, message=FALSE, warning=FALSE, out.height = '85%', out.width = '85%', fig.align="center"----------------
plot(test_11)


## ----L1S27_1, message=FALSE, warning=FALSE,eval=FALSE----------------------------------------------------------------
## ## Calculations in R
##  x <- rbinom(1000,8, 0.5)
##  sum(x <= 2)/1000
##  hist(x,main = '')

## ----L1S27_1b, message=FALSE, warning=FALSE,echo=FALSE---------------------------------------------------------------
## Calculations in R
 set.seed(1)
 x <- rbinom(1000,8, 0.5)
 round(sum(x <= 2)/1000,3)
 hist(x,main = '')


## ----L1S27_2, message=FALSE, warning=FALSE, out.height = '85%', out.width = '85%', fig.align="center"----------------
## Calculations in JAGS from R using rjags
model_11.bug <-
  "model {
     Y ~ dbin(0.5, 8)
     P2 <- step(2 - Y)
    }"
### Generate summary statistics on the samples
summary(test_11)[[1]][,c(1,2)]


## ----L1S28_1, eval = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
## model_11.bug <-
## 
##   "data{
##     p <- 0.5
##     n <- 8
##   }
##   model {
##      Y ~ dbin(p, n)
##      P2 <- step(2 - Y)
##     }"


## ----L1S32_1, results = "hide", message=FALSE, warning=FALSE---------------------------------------------------------
### model is defined as a string
model_12.bug <- "model {
   theta ~ dbeta(3, 27)
    P1 <- step( theta - 0.2) 
    P2 <- step( 0.03 - theta) 
  }"
model_12 <- jags.model(textConnection(model_12.bug))

## ----L1S32_3, message=FALSE, warning=FALSE---------------------------------------------------------------------------
mcmc_12 <- update(model_12, n.iter=1000, progress.bar="gui")
test_12 <- coda.samples(model_12, c('P1','P2','theta'), 
                            n.iter = 10000)
plot(test_12)


## ----L1S32_2, message=FALSE, warning=FALSE---------------------------------------------------------------------------
summary(test_12)


## ----L1S35_1, results = "hide", message=FALSE, warning=FALSE---------------------------------------------------------
### model is defined as a string
model_13.bug <- 
 "model {
   theta ~ dbeta(3,27)
       Y    ~ dbin(theta, 20)
       P1 <- step( theta-0.2) 
       P6 <- step( Y - 5.5)
              }"


## ----L1S36_1, results = "hide", message=FALSE, warning=FALSE---------------------------------------------------------
### model is defined as a string
model_13.bug <- 
 "model {
   theta ~ dbeta(3,27)
       Y    ~ dbin(theta, 20)
  P1 <- step( theta-0.2) 
  P6 <- step( Y - 5.5)
              }"
model_13 <- jags.model(textConnection(model_13.bug))
mcmc_13 <- update(model_13, n.iter=1000)
test_13 <- coda.samples(model_13, c('P1','P6','theta','Y'), 
                        n.iter = 10000)


## ----L1S36_2, message=FALSE, warning=FALSE---------------------------------------------------------------------------
summary(test_13)


## ----L1S37_1, message=FALSE, warning=FALSE, out.height = '70%', out.width = '70%', fig.align="center"----------------
plot(test_13[,c(3,4)])


## ----L1S41_1, eval = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
##   "model{
##          Y ~ dnorm(0, 1)
##          prob.2 <- step(- 1 - Y) ## 1 if -1-Y > 0 => Y < -1
##     }"


## ----L1S42_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
model_int <-
  "model{
          Y ~ dnorm(0, 1)
          prob.2 <- step( -1-Y)
       }"

model_int <- jags.model(textConnection(model_int))
 mcmc_int <- update(model_int, n.iter=1000)
 test_int <- coda.samples(model_int, c('prob.2','Y'), n.iter = 10000)
 summary(test_int)
    plot(test_int)


## ----L1S42_2, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
 summary(test_int)


## ----L1S50_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
model.1 <-
  "model{
          theta ~ dbeta(1,9)
       }"

model.1 <- jags.model(textConnection(model.1))
 mcmc.1 <- update(model.1, n.iter=1000)
 test <- coda.samples(model.1, c('theta'), n.iter = 10000)
 summary(test)
    plot(test)


## ----L1S50_2, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
 summary(test)


## ----L1S50_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
densplot(test[,1])


## ----L1S51_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
model.1 <-
  "model{
          theta ~ dbeta(88,922)
       }"

model.1 <- jags.model(textConnection(model.1))
 mcmc.1 <- update(model.1, n.iter=1000)
 test <- coda.samples(model.1, c('theta'), n.iter = 10000)
 summary(test)
    plot(test)


## ----L1S51_2, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
 summary(test)


## ----L1S51_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
densplot(test[,1])


## ----L1S54_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
model.1 <-
  "model{
          theta ~ dbeta(88,922)
          Odds <- theta/(1-theta)
       }"

model.1 <- jags.model(textConnection(model.1))
 mcmc.1 <- update(model.1, n.iter=1000)
 test <- coda.samples(model.1, c('theta', 'Odds'), n.iter = 10000)
 summary(test)
    plot(test)


## ----L1S54_2, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
 summary(test)


## ----L1S54_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
plot(test)


## ----L1S57_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
model.1 <-
  "model{
          theta ~ dbeta(1,9)
          Y ~ dbin(theta, 1000)
          Odds <- theta/(1-theta)
       }"


## ----L1S58_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------
model.1 <-
  "model{
          theta ~ dbeta(1,9)
           Y ~  dbin(theta, n)
          Odds <- theta/(1-theta)
       }"

odds.data <- list( Y=87, n=1000) # Data
model.1 <- jags.model(textConnection(model.1), data=odds.data)
 mcmc.1 <- update(model.1, n.iter=1000)
 test <- coda.samples(model.1, c('theta','Odds'), n.iter = 10000)
 summary(test)
    plot(test)


## ----L1S58_2, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
 summary(test)


## ----L1S58_3, echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------
plot(test)


## ----L1S77_1, message=FALSE, warning=FALSE, out.height = '50%', out.width = '100%', fig.align="center"---------------
theta.1 <- rbeta(10000, 1, 1)

theta.2 <- rbeta(10000, 1, 1)

OR.1.1 <- (theta.1/(1-theta.1))/(theta.2/(1 - theta.2))

par(mfrow = c(1, 3))
hist(theta.1, probability = T)
hist(OR.1.1, probability = T)
hist(log(OR.1.1), probability = T)


## ----L1S78_1, message=FALSE, warning=FALSE, out.height = '50%', out.width = '100%', fig.align="center"---------------
theta.1 <- rbeta(10000, 0.5, 0.5)

theta.2 <- rbeta(10000, 0.5, 0.5)

OR.5.5 <- (theta.1/(1-theta.1))/(theta.2/(1 - theta.2))

par(mfrow = c(1, 3))
hist(theta.1, probability = T)
hist(OR.5.5, probability = T)
hist(log(OR.5.5), probability = T)


## ----L1S79_1, message=FALSE, warning=FALSE, out.height = '50%', out.width = '100%', fig.align="center"---------------
theta.1 <- rbeta(10000, 2, 2)

theta.2 <- rbeta(10000, 2, 2)

OR.2.2 <- (theta.1/(1-theta.1))/(theta.2/(1 - theta.2))

par(mfrow = c(1, 3))
hist(theta.1, probability = T)
hist(OR.2.2, probability = T)
hist(log(OR.2.2), probability = T)

