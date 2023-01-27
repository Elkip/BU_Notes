## ----prep work, echo = FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------
library("knitr")
library("rjags")
library("coda")
library("formatR")
# Prevent code chunks out of range
# Use tidy = FALSE in each code chunk to disable this option
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)


## ---- include = F----------------------------------------------------------------------------------------------------
defOut <- knitr::knit_hooks$get("plot")  # save the default plot hook 
knitr::knit_hooks$set(plot = function(x, options) {  # set new plot hook ...
  x <- defOut(x, options)  # first apply the default hook
  if(!is.null(options$wrapfigure)) {  # then, if option wrapfigure is given ...
    # create the new opening string for the wrapfigure environment ...
    wf <- sprintf("\\begin{wrapfigure}{%s}{%g\\textwidth}", options$wrapfigure[[1]], options$wrapfigure[[2]])
    x  <- gsub("\\begin{figure}", wf, x, fixed = T)  # and replace the default one with it.
    x  <- gsub("{figure}", "{wrapfigure}", x, fixed = T)  # also replace the environment ending
  }
  return(x)
})


## ----echo = F, warning = F, message = F, out.width = ".45\\textwidth", fig.cap = "", fig.align="right", wrapfigure = list("R", .45)----
x <- seq(-5, 5, by = 0.01)
par(mfrow=c(1,1),cex.lab=1.4,cex.axis=1.4, mar=c(3,3.2,0.7,0.5),mgp=c(1.9,0.7,0))
plot(x, exp(x)/(1 + exp(x)), type = "l",xlab=expression('Predictor ('*eta*')'),ylab=expression('Logit('*eta*')'),lwd=2 )


## ----L2S7_1,  message=FALSE, warning=FALSE, tidy = FALSE-------------------------------------------------------------
   X.E <- c( rep(1, 39), rep(0, 114), rep(1, 24), rep(0,154))
   Y.D <- c( rep(1, 153), rep(0, 178))
#  Create JAGS data as a list
   dput( list(N=331, X.E = X.E, Y.D = Y.D),"data.txt") 
    #  view data.txt:    file.show("data.txt")
    xtabs(~Y.D + X.E)
    


## ----L2S7_1a, results = "hide", message=FALSE, warning=FALSE, tidy = FALSE-------------------------------------------
    mod <- glm(Y.D ~ X.E, family = binomial) 
    summary(mod)


## ----L2S8_1, echo = FALSE, message=FALSE, warning=FALSE--------------------------------------------------------------
summary(mod)


## ----L2S11_1 Logistic Jags, results = FALSE, message=FALSE, warning=FALSE--------------------------------------------

# Analysis with JAGS 
library(rjags)

model.1 <-
  "model{
  ### data model
   for(i in 1:N){
      Y.D[i] ~ dbin(p[i], 1)
     logit(p[i]) <- beta_0 + beta_1*X.E[i]
                        }
          OR <- exp(beta_1)
    pos.prob <- step(beta_1)
 ### prior
      beta_1 ~ dnorm(0,0.0001)
      beta_0 ~ dnorm(0,0.0001)
                 }"
# In R Data stored as a list
 data.1 <- list(N = 331, X.E = X.E, Y.D = Y.D)
# Compile model (data is part of it!); `adapt` for 2,000 samples
 model_odds <- jags.model(textConnection(model.1), 
                          data = data.1, n.adapt = 2000)
 update(model_odds, n.iter = 5000) # 5,000 burn-in samples
# Get 10,000 samples from the posterior distribution of OR, beta_0,beta_1
 test_odds  <- coda.samples(model_odds, 
        c('OR','beta_1','beta_0'), n.iter = 10000)


## ----L2S12_1 Logistic Jags, message=FALSE, warning=FALSE-------------------------------------------------------------
 summary(test_odds)


## ----L2S13_1 Logistic Jags, message=FALSE, warning=FALSE,out.height="80%",fig.align='center'-------------------------
 plot(test_odds)


## ----L2S15_1 MLE, echo = FALSE, message=FALSE, warning=FALSE---------------------------------------------------------
summary(mod)


## ----L2S15_2 Bayesian, echo = FALSE, message=FALSE, warning=FALSE----------------------------------------------------
summary(test_odds)$statistics


## ----L2S39_1, message=FALSE, warning=FALSE, tidy = FALSE-------------------------------------------------------------
X.E <- rep(c(1,0,1,0,1,0,1,0), c(21,26,18,88,17,59,7,95)) 
X.age <- rep(c(0, 1, 0, 1), c(47,106,76,102))
Y.D <- rep(c(1,0), c(153,178))

mod <- glm(Y.D ~ X.E+X.age, family=binomial) # Logistic Regression 
summary(mod)


## ----L2S40_1, results="hide", message=FALSE, warning=FALSE, tidy = FALSE---------------------------------------------
library(rjags)

model.1 <- 
  "model{
  ### data model
   for(i in 1:N){
   Y.D[i] ~ dbin(p[i], 1)
  
     logit(p[i]) <- beta_0+beta_1*X.E[i]+beta_2*X.age[i]
                        }
    OR <-exp(beta_1)
    pos.prob <- step(beta_1)
 ### prior
      beta_0 ~ dnorm(0,0.0001)
      beta_1 ~ dnorm(0,0.0001)
      beta_2 ~ dnorm(0,0.0001)
                 }"

 data.1 <- list(N=331, X.E=X.E, Y.D=Y.D, X.age=X.age)

 model_odds <- jags.model(textConnection(model.1), data=data.1,
                          n.adapt =1000)
 update(model_odds, n.iter = 5000)
test_odds  <- coda.samples(model_odds, c('OR','beta_0','beta_1','beta_2'), n.iter = 10000)


## ----L2S41_1, message=FALSE, warning=FALSE, tidy = FALSE-------------------------------------------------------------
summary(test_odds)


## ----L2S41_1b, message=FALSE, warning=FALSE, tidy = FALSE------------------------------------------------------------
summary(mod)$coefficients


## ----L2S42_1, echo = FALSE, message=FALSE, warning=FALSE, tidy = FALSE, fig.show='hold', out.width='.49\\linewidth'----
par(mfrow = c(4,1))
traceplot(test_odds)
par(mfrow = c(1,1))
autocorr.plot(test_odds)


## ----L2S43_1, echo = FALSE, message=FALSE, warning=FALSE, tidy = FALSE-----------------------------------------------
densplot(test_odds[,1])


## ----L2S43_2, echo = FALSE, message=FALSE, warning=FALSE, tidy = FALSE-----------------------------------------------
summary(test_odds)


## ----L2S46_1, results="hide", message=FALSE, warning=FALSE-----------------------------------------------------------
model.1 <- 
  "model{
  ### data model
   for(i in 1:N){
   Y.D[i] ~ dbin(p[i], 1)
  
     logit(p[i]) <- beta_0+beta_1*X.E[i]+beta_2*X.age[i]
                        }
    OR <-exp(beta_1)
    pos.prob <- step(OR - 2)
 ### prior
      beta_0 ~ dnorm(0,0.0001)
      beta_1 ~ dnorm(0,0.0001)
      beta_2 ~ dnorm(0,0.0001)
                 }"

 data.1 <- list(N=331, X.E=X.E, Y.D=Y.D, X.age=X.age)

model_odds <- jags.model(textConnection(model.1), data=data.1, 
                         n.adapt = 1000)
update(model_odds, n.iter = 10000)
test_odds  <- coda.samples(model_odds, c('OR','pos.prob'), 
                           n.iter = 10000)


## ----L2S46_2, message=FALSE, warning=FALSE, tidy = FALSE-------------------------------------------------------------
summary(test_odds)


## ----L2S46_3, echo = FALSE, message=FALSE, warning=FALSE, tidy = FALSE-----------------------------------------------
pos.prob <- summary(test_odds)$statistics[2,1]
pos.odds <- round(pos.prob / (1 - pos.prob),1)


## ----L2S52_1, results="hide", message=FALSE, warning=FALSE, tidy = TRUE----------------------------------------------
model.1 <- 
  "model{
  ### data model
   for(i in 1:N){
   Y.D[i] ~ dbin(p[i], 1)
  
     logit(p[i]) <- beta_0+beta_1*X.E[i]+beta_2*X.age[i]+beta_3*X.E[i]*X.age[i]
                        }
    OR.young <-exp(beta_1)
    OR.old   <- exp(beta_1+beta_3)
    ROR <- exp(beta_3)
 ### prior
      beta_0 ~ dnorm(0,0.0001)
      beta_1 ~ dnorm(0,0.0001)
      beta_2 ~ dnorm(0,0.0001)
      beta_3 ~ dnorm(0,0.0001)
                 }"

 data.1 <- list(N=331, X.E=X.E, Y.D=Y.D, X.age=X.age)

 model_odds <- jags.model(textConnection(model.1), data=data.1, n.adapt = 1000)
 update(model_odds, n.iter = 10000)
 test_odds  <- coda.samples(model_odds, c('OR.young','OR.old','ROR','beta_1','beta_2','beta_3'), 
                            n.iter = 10000)


## ----L2S53_1, echo=FALSE, message=FALSE, warning=FALSE, tidy = FALSE, out.height='50%'-------------------------------
par(mfrow=c(1,2), oma=c(0,0,3,0))
plot(test_odds[,6])
mtext("Trace and Density of beta_3", line=0, side=3, outer=TRUE, cex=2)


## ----L2S53_2, echo=FALSE, message=FALSE, warning=FALSE, tidy = FALSE-------------------------------------------------
summary(test_odds)


## ----L2S73_1 Simpson, results = FALSE, message=FALSE, warning=FALSE--------------------------------------------------
model.simpson <-
  "model{  
    ### data model
    black_defendent_death ~ dbin(theta_1, m_1)
    black_defendent_other ~ dbin(theta_0, m_0)  ## control

     logit(theta_0) <- beta_0;     
     logit(theta_1) <- beta_0+beta_1
     OR <-exp(beta_1)
    ### prior
    beta_1 ~ dnorm(0,0.0001);
    beta_0 ~ dnorm(0,0.0001)}"

 data.simpson <- list(black_defendent_death= 59, m_1=131, black_defendent_other = 2448, m_0=4633)
 model_simpson <- jags.model(textConnection(model.simpson), data=data.simpson, n.adapt = 1000)
 update(model_odds, n.iter = 10000)
 simpson_odds  <- coda.samples(model_simpson, c('OR', 'theta_0', 'theta_1'), n.iter = 10000)


## ----L2S74_1, echo = FALSE, message=FALSE, warning=FALSE, tidy = FALSE-----------------------------------------------
plot(simpson_odds[,1])


## ----L2S74_2, echo = FALSE, message=FALSE, warning=FALSE, tidy = FALSE-----------------------------------------------
summary(simpson_odds)


## ----L2S75_1, echo = FALSE, message=FALSE, warning=FALSE, tidy = FALSE-----------------------------------------------
plot(simpson_odds[,c(2,3)])


## ----L2S75_2, echo = FALSE, message=FALSE, warning=FALSE, tidy = FALSE-----------------------------------------------
summary(simpson_odds)[1]


## ----L2S79_1 Simpson, results = FALSE, message=FALSE, warning=FALSE--------------------------------------------------
model.simpson1 <-
  "model{  
    ### data model
    for(i in 1:N){
      Y_death[i] ~ dbin(theta[i], 1)
      
      logit(theta[i]) <-beta_0 + beta_1 * X.race.V[i] + beta_2 * X.race.M[i]
    }
    
    OR <- exp(beta_2)

    ### prior
    beta_1 ~ dnorm(0,0.0001);
    beta_0 ~ dnorm(0,0.0001);
    beta_2 ~ dnorm(0,0.0001)}"

 ### simpson's paradox
 ### victim race
 ###               Black             White
 ###          Defendent race     Defendent race
 ## Sentence  Black   White     Black   White   
 ##  Death    10      1         49      71   || 131
 ##  Other    2209    111       239     2074 || 4633

Y_death <- c(rep(1, 131), rep(0, 4633))
X.race.M <- c(rep(1,10),   rep(0,1),   rep(1, 49),  rep(0, 71),
              rep(1,2209), rep(0,111), rep(1, 239), rep(0, 2074))
X.race.V <- c(rep(1, 11),   rep(0, 120),
              rep(1, 2320), rep(0,2313))

mod <- glm(Y_death ~ X.race.M * X.race.V, family = binomial)
dput(list(N = 4764, X.race.M = X.race.M, X.race.V = X.race.V, Y_death = Y_death), "data.simpson.paradox.txt")


## ----L2S80_1, echo=FALSE, message=FALSE, warning=FALSE, tidy = FALSE, out.height='50%', cache=TRUE-------------------
data.simpson1 <- list(N = 4764, X.race.M = X.race.M, X.race.V = X.race.V, Y_death = Y_death)
model_simpson1 <- jags.model(textConnection(model.simpson1), data=data.simpson1, n.adapt = 1000)
update(model_simpson1, n.iter = 10000)
simpson_odds1  <- coda.samples(model_simpson1, c('OR', 'beta_1', 'beta_2'), n.iter = 10000)

plot(simpson_odds1)


## ----L2S80_2, echo=FALSE, message=FALSE, warning=FALSE, tidy = FALSE-------------------------------------------------
summary(simpson_odds1)


## ----L2S83_1 Simpson, echo = FALSE, results = FALSE, message=FALSE, warning=FALSE,cache=TRUE-------------------------
model.simpson2 <-
  "model{  
    ### data model
    for(i in 1:N){
      Y_death[i] ~ dbin(theta[i], 1)
      
      logit(theta[i]) <-beta_0 + beta_1 * X.race.V[i] + beta_2 * X.race.M[i] + beta_3 * X.race.V[i] * X.race.M[i]
    }
    
    OR1 <- exp(beta_2 + beta_3)
    OR2 <-  exp(beta_2)
    ### prior
    beta_1 ~ dnorm(0,0.0001);
    beta_0 ~ dnorm(0,0.0001);
    beta_2 ~ dnorm(0,0.0001);
    beta_3 ~ dnorm(0,0.0001)}"

model_simpson2 <- jags.model(textConnection(model.simpson2), data=data.simpson1, n.adapt = 1000)
update(model_simpson2,n.iter= 10000)
simpson_odds2  <- coda.samples(model_simpson2, c('OR1', 'OR2', 'beta_1', 'beta_2', 'beta_3'), n.iter = 10000,thin=10)



## ----L2S83_1, echo=FALSE, message=FALSE, warning=FALSE, tidy = FALSE, out.height='50%'-------------------------------
autocorr.plot(simpson_odds2)


## ----L2S83_2, echo=FALSE, message=FALSE, warning=FALSE, tidy = FALSE, out.height='50%'-------------------------------
summary(simpson_odds2)

a <- summary(simpson_odds2)$quantiles

or1.est <- round(a[1,"50%"], 2)
or1.l  <-  round(a[1,"2.5%"], 2)
or1.u  <-  round(a[1,"97.5%"], 2)

or2.est <- round(a[2,"50%"], 2)
or2.l  <-  round(a[2,"2.5%"], 2)
or2.u  <-  round(a[2,"97.5%"], 2)


## --------------------------------------------------------------------------------------------------------------------
MN<- 'model{ 
sigma2<- 169 # known variance
# precision = 1/variance
   tau <- pow(sigma2,-1) 
 # prior on normal mean
   mu ~ dnorm(120,1)
for (i in 1:N){
    y[i] ~ dnorm(mu,tau)
}
# Predict New Patient
  y.new ~ dnorm(mu,tau) 
  p.125 <- step(y.new-125)
  p.above <- step(mu-125)
  p.below <- step(125-mu)
}'


## --------------------------------------------------------------------------------------------------------------------
MN<- 'model{ 
sigma2<- 169 # known variance
# precision = 1/variance
   tau <- pow(sigma2,-1) 
 # prior on normal mean
   mu ~ dnorm(120,0.04)
for (i in 1:N){
    y[i] ~ dnorm(mu,tau)
}
# Predict New Patient
  y.new ~ dnorm(mu,tau) 
  p.125 <- step(y.new-125)
  p.above <- step(mu-125)
  p.below <- step(125-mu)
}'
### Data
BP.data<- 
list(N=20, y=c(98,160,136,128,130,
              114,123,134,128,107,
              123,125,129,132,154,
              115,126,132,136,130))
modelBP <- jags.model(textConnection(MN), data=BP.data, n.adapt = 1000,quiet = T)
 update(modelBP, n.iter = 10000)
TestBP  <- coda.samples(modelBP, c('p.above','mu','y.new'), 
                            n.iter = 10000)
summary(TestBP)$statistics

## ----echo=F----------------------------------------------------------------------------------------------------------
p1<-round(summary(TestBP)$statistics['p.above',1],2)


## --------------------------------------------------------------------------------------------------------------------
m.prior <- 'model{
mu~dnorm(120,0.04)
p.125 <- step(mu-125)
}'


## ----echo=FALSE------------------------------------------------------------------------------------------------------
m.prior <- 'model{
mu~dnorm(120,0.04)
p.125 <- step(mu-125)
}'
modelPr <- jags.model(textConnection(m.prior), n.adapt = 1000,quiet = T)
 update(modelPr, n.iter = 10000)
TestPr  <- coda.samples(modelPr, c('p.125'), 
                            n.iter = 10000)
p0<-round(summary(TestPr)$statistics[1],2)


## --------------------------------------------------------------------------------------------------------------------
MN<- 'model{ 
sigma2<- 169 # known variance
# precision = 1/variance
   tau <- pow(sigma2,-1) 
 # prior on normal mean
   mu ~ dnorm(120,0.01)
for (i in 1:N){
    y[i] ~ dnorm(mu,tau)
}
# Predict New Patient
  y.new ~ dnorm(mu,tau) 
  p.125 <- step(y.new-125)
  p.above <- step(mu-125)
  p.below <- step(125-mu)
}'
### Data
BP.data<- 
list(N=20, y=c(98,160,136,128,130,
              114,123,134,128,107,
              123,125,129,132,154,
              115,126,132,136,130))
modelBP <- jags.model(textConnection(MN), data=BP.data, n.adapt = 1000,quiet = T)
 update(modelBP, n.iter = 10000)
TestBP  <- coda.samples(modelBP, c('p.above','mu','y.new'), 
                            n.iter = 10000)
summary(TestBP)$statistics


## ----echo=F----------------------------------------------------------------------------------------------------------
p1<-round(summary(TestBP)$statistics['p.above',1],2)


## --------------------------------------------------------------------------------------------------------------------
m.prior <- 'model{
mu~dnorm(120,0.01)
p.125 <- step(mu-125)
}'


## ----echo=FALSE------------------------------------------------------------------------------------------------------
m.prior <- 'model{
mu~dnorm(120,0.01)
p.125 <- step(mu-125)
}'
modelPr <- jags.model(textConnection(m.prior), n.adapt = 1000,quiet = T)
 update(modelPr, n.iter = 10000)
TestPr  <- coda.samples(modelPr, c('p.125'), 
                            n.iter = 10000)
p0<-round(summary(TestPr)$statistics[1],2)

