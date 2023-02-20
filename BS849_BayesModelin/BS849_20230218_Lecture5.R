## ----prep work, echo = FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------
library("knitr")
library("rjags")
library("coda")
library("formatR")
library("dplyr")
# Prevent code chunks out of range
# Use tidy = FALSE in each code chunk to disable this option
opts_chunk$set(tidy.opts = list(width.cutoff = 60), tidy = TRUE)


## ----L4S5_7, message=FALSE, warning=FALSE, fig.width=3.7,fig.height=2, fig.align="center"-----------------------------------------------------------------------------------------
HB.data <- read.csv("HB.data.csv", header = T)
par(
  mfrow = c(1, 1),
  mar = c(3.5, 3, 0.05, 0.5),
  mgp = c(1.5, 0.5, 0),
  cex.lab = 0.7,
  cex.axis = 0.7
)
plot(
  unlist(HB.data[1, 5:8]),
  ylim = c(0, max(HB.data[, 5:8], na.rm = T)),
  ylab = 'Anti-HB titre',
  xlab = 'Time',
  axes = F
)
axis(1, at = 1:4, labels = 0:3)
axis(2)
for (i in 1:106) {
  lines(unlist(HB.data[i, 5:8]), col = i)
}


## ----L5S8_1, eval = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
## model.1 <- "model {
## for (i in 1:N) {
##  for(j in offset[i] : (offset[i+1]-1)){
##      y[j]  ~ dnorm(psi[j], tau.y)
##    psi[j] <- alpha[i] + beta[i]*(t[j] - tbar)
##                + gamma*(y0[i] - y0bar)
##               }
##     alpha[i] ~ dnorm(mu.alpha, tau.alpha)
##      beta[i] ~ dnorm(mu.beta, tau.beta)
##     }
##  ### priors...
## }"


## ----L5S9_1, echo = FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------
y <- na.omit(c(t(HB.data[, 6:8])))
offset <- c(1, 1 + length(which(is.na(HB.data[1, 6:8]) == F)))
i <- 1
while (offset[(i + 1)] < length(y) - 1) {
  i <- i + 1
  offset <-
    c(offset, offset[i] + length(which(is.na(HB.data[i, 6:8]) == F)))
}
offset <- c(offset, (length(y) + 1))


## ----L5S9_2, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
cbind(ID = 1:5, HB.data[1:5, 6:8], ID = 6:10, HB.data[6:10, 6:8])
offset[1:11]


## ----L5S9_3, eval = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----------------------------------------------------------------------------------------
## model.1 <- "
## model {
##   for (i in 1:N) {
##        for(j in offset[i] : (offset[i+1]-1)){...}
## ...
## }"


## ----L5S10_1, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------
y <- na.omit(c(t(HB.data[, 6:8])))  # All outcome data
y[1:15]

## ----L5S10_2, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------
y0 <- HB.data[, 5]                             # Baseline data
t <-
  c(t(HB.data[, c(2, 3, 4)]))[-na.action(y)] # Times corresponding to non-missing outcome
t[1:15]


## ----L5S10_3a, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------
i <- 1

offset <- c(1, 1 + length(which(is.na(HB.data[1, 6:8]) == F)))


## ----L5S10_3b, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------
while (offset[(i + 1)] < length(y) - 1) {
  i <- i + 1
  offset <-
    c(offset, offset[i] + length(which(is.na(HB.data[i, 6:8]) == F))) # Calculate offsett for subject i
}
offset <-
  c(offset, (length(y) + 1))                                  # Concatenate to the set of offsets
offset[1:10]


## ----L5S11_1, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE---------------------------------------------------------------------------------------
data.offs <-
  list(
    N = nrow(HB.data),
    y = y,
    t = t,
    y0 = y0,
    offset = offset
  )


## ----L5S11_2, results = "hide", fig.show="hide", message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------
model.1 <- "
model {
  for (i in 1:N) {
       for(j in offset[i] : (offset[i+1]-1)){
         y[j]    ~ dnorm(psi[j], tau.y)
       psi[j]   <- alpha[i] + beta[i]*(t[j] - tbar)
                     + gamma*(y0[i] - y0bar) }
    alpha[i]    ~ dnorm(mu.alpha, tau.alpha)
    beta[i]     ~ dnorm(mu.beta, tau.beta)
        }
                   sigma.a  <- 1/tau.alpha
                   sigma.b  <- 1/tau.beta
                   sigma.y  <- 1/tau.y
          mu.alpha ~ dnorm(0, 0.0001)
           mu.beta ~ dnorm(0, 0.0001)
             gamma ~ dnorm(0, 0.0001)
          tau.alpha ~dgamma(1,1)
          tau.beta ~dgamma(1,1)
          tau.y ~dgamma(1,1)
  y0bar <- mean(y0[])
   tbar <- mean(t[])
}"
library(rjags)
jags.1 <-
  jags.model(textConnection(model.1), data = data.offs, n.adapt = 1500)
update(jags.1, 10000)


## ----L5S12_1, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------
test.1 <-
  coda.samples(
    jags.1,
    c('mu.alpha', 'mu.beta', 'gamma', 'sigma.y', 'sigma.a', 'sigma.b'),
    n.adapt = 1500,
    n.iter = 10000
  )
summary(test.1)


## ----L4S5_13, message=FALSE, warning=FALSE, out.height = '70%', out.width = '70%', fig.align="center",cache=T---------------------------------------------------------------------
test.1 <-
  coda.samples(jags.1, c('alpha'), n.adapt = 1500, n.iter = 1000)
out <- as.matrix(test.1)
boxplot(out, las = 2)
abline(h = 6.1)  # Intercept for constant slope and intercept model


## ----L4S5_14, message=FALSE, warning=FALSE, out.height = '70%', out.width = '70%', fig.align="center",cache=T---------------------------------------------------------------------
test.1 <-
  coda.samples(jags.1, c('beta'), n.adapt = 1500, n.iter = 1000)
out <- as.matrix(test.1)
boxplot(out, las = 2)
abline(h = -1.06)


## ----L5S15_1, echo = FALSE, message=FALSE, warning=FALSE,cache=T------------------------------------------------------------------------------------------------------------------
child <- rep(1:106, each = 3)      # Construct index
child <- child[-na.action(y)]
data.nest <-
  list(
    N = nrow(HB.data),
    Nobs = length(y),
    y = y,
    t = t,
    y0 = y0,
    child = child
  )


model.2 <- "model {
for (j in 1:Nobs) {
         y[j]    ~ dnorm(psi[j], tau.y)
       psi[j]   <- alpha[child[j]] + beta[child[j]]*(t[j] - tbar)
                     + gamma*(y0[child[j]] - y0bar) }
## random effects
for(i in 1:N){
    alpha[i]    ~ dnorm(mu.alpha, tau.alpha)
    beta[i]     ~ dnorm(mu.beta, tau.beta)
        }
                   sigma.a  <- 1/tau.alpha
                   sigma.b  <- 1/tau.beta
                   sigma.y  <- 1/tau.y
          mu.alpha ~ dnorm(0, 0.0001)
           mu.beta ~ dnorm(0, 0.0001)
             gamma ~ dnorm(0, 0.0001)
          tau.alpha ~dgamma(1,1)
          tau.beta ~dgamma(1,1)
          tau.y ~dgamma(1,1)
  y0bar <- mean(y0[])
   tbar <- mean(t[])
}"

jags.2 <-
  jags.model(textConnection(model.2), data = data.nest, n.adapt = 1500)
update(jags.2, 10000)
test.2 <-
  coda.samples(
    jags.2,
    c('mu.alpha', 'mu.beta', 'gamma', 'sigma.y', 'sigma.a', 'sigma.b'),
    n.adapt = 1500,
    n.iter = 10000
  )
summary(test.2)


## ----L5S16_1, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------
y[1:30]
child[1:10]


## ----L5S17_1, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------
### Nested indexing
child <- rep(1:106, each = 3)
child[1:10]
child <- child[-na.action(y)]
child[1:10]
y[1:10]

data.nest <-
  list(
    N = nrow(HB.data),
    Nobs = length(y),
    y = y,
    t = t,
    y0 = y0,
    child = child
  )


## ----L5S10_1a, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------
y <- HB.data[, 6:8]       # All outcome data (tabular)
y0 <- HB.data[, 5]       # Baseline data
t <- HB.data[, c(2, 3, 4)] # Times corresponding to outcome
pData <- list(y = y,
              t = t,
              y0 = y0,
              N = 106)


## ----L5S18_1, eval = FALSE, message=FALSE, warning=FALSE,cashe=T,results='hide'---------------------------------------------------------------------------------------------------
## model.2 <- "model {
##   for (i in 1:N) {
##     for (j in 1:3) {
##       y[i,j]    ~ dnorm(psi[i,j], tau.y)
##       psi[i,j] <- alpha[i] + beta[i]*(t[i,j] - 6.49)
##                 + gamma*(y0[i] - 6.76)
##                 }
##     alpha[i]    ~ dnorm(mu.alpha, tau.alpha)
##     beta[i]     ~ dnorm(mu.beta, tau.beta)
##     }
##                    sigma.a  <- 1/tau.alpha
##                    sigma.b  <- 1/tau.beta
##                    sigma.y  <- 1/tau.y
##           mu.alpha ~ dnorm(0, 0.0001)
##            mu.beta ~ dnorm(0, 0.0001)
##              gamma ~ dnorm(0, 0.0001)
##           tau.alpha ~dgamma(1,1)
##           tau.beta ~dgamma(1,1)
##           tau.y ~dgamma(1,1)
## }"
## jags.2 <- jags.model(textConnection(model.2),data=pData, n.adapt=1500)
## update(jags.2,10000)
## test.2 <- coda.samples(jags.2, c('mu.alpha','mu.beta','gamma','sigma.y','sigma.a','sigma.b','y[1,3]'), n.adapt=1500, n.iter=10000)
##    summary(test.2)


## ----L5S22_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------
model.snp <- "model
{ mu.pc1 <- mean(PC1[])
  mu.pc2 <- mean(PC2[])
  for (i in 1 : N) {
   pc1[i] <- PC1[i]
   pc2[i] <- PC2[i]
   # logistic model
   outcome[i] ~ dbin(p[i], 1)
      logit(p[i]) <- beta.sex* Sex[i] + beta.pc1*(pc1[i]-mu.pc1) +
         beta.pc2* (pc2[i] -mu.pc2) +beta.snp*snp[i]+b[fam[i]]
        }
  for( i in 1:F){
      b[ i ] ~ dnorm(beta.0, tau)
     }  ###  random effect per family
  # Priors:
   beta.sex ~   dnorm(0.0, 0.01); beta.0 ~   dnorm(0.0, 0.01)
    beta.pc1 ~  dnorm(0.0, 0.01);  beta.pc2 ~  dnorm(0.0, 0.01);
     beta.snp ~   dnorm(0.0, 0.01)
    tau ~ dgamma(1, 1);

  parameter[1] <-  beta.0;  parameter[2] <- beta.sex
  parameter[3] <- beta.pc1; parameter[4] <- beta.pc2;
  parameter[5] <- beta.snp;
}"


## ----L5S28_1, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE,cache=TRUE----------------------------------------------------------------------------
all.data <- read.csv("LLFS.subset.csv", header = T)
all.data <- na.omit(all.data)

## define new indicator of family
ind.family <- rep(NA, length(all.data[, "pedid"]))
for (i in 1:length(unique(all.data[, "pedid"]))) {
  ind.family[which(all.data[, "pedid"] == unique(all.data[, "pedid"])[i])] <-
    rep(i, length(which(all.data[, "pedid"] == unique(all.data[, "pedid"])[i])))
}

total.subjects <- nrow(all.data)

## a different way to create family index
all.data$fam[1] <- 1
for (i in 2:nrow(all.data)) {
  if (all.data$pedid[i] == all.data$pedid[i - 1]) {
    all.data$fam[i] <- all.data$fam[i - 1]
  } else{
    all.data$fam[i] <- all.data$fam[i - 1] + 1
  }
}

## recode sex
Sex <- as.numeric((all.data$sex)) - 1

SNP <- all.data[, grep("rs", names(all.data))]

data.snp <-
  list(
    N = total.subjects,
    F = length(unique(ind.family)),
    outcome = all.data$outcome,
    snp = SNP[, 7],
    PC1 = all.data$pc1,
    PC2 = all.data$pc2,
    Sex = Sex,
    fam = ind.family
  )

beta.snp.out <- c()
for (ind in 1:ncol(SNP)) {
  print(ind)
  data.snp <-
    list(
      N = total.subjects,
      F = length(unique(ind.family)),
      outcome = all.data$outcome,
      snp = SNP[, ind],
      PC1 = all.data$pc1,
      PC2 = all.data$pc2,
      Sex = Sex,
      fam = ind.family
    )
  
  jags.snp <-
    jags.model(
      textConnection(model.snp),
      data = data.snp,
      n.adapt = 1500,
      n.chains = 3
    )
  
  update(jags.snp, 1000)
  test.snp <-
    coda.samples(jags.snp,
                 c('beta.snp'),
                 n.adapt = 1500,
                 n.iter = 1000)
  beta.snp.out <- cbind(beta.snp.out, as.matrix(test.snp)[, 1])
}


## ----L5S28_2a,  results='hide', message=FALSE, warning=FALSE,out.height = '50%', out.width = '50%', fig.align="center"------------------------------------------------------------
jags.snp <-
  jags.model(
    textConnection(model.snp),
    data = data.snp,
    n.adapt = 1500,
    n.chains = 3
  )

## ----L5S28_2a1,  results='hide', message=FALSE, warning=FALSE,cache=T,out.height = '50%', out.width = '50%', fig.align="center"---------------------------------------------------
update(jags.snp, 1000)
test.snp <- coda.samples(jags.snp, c('beta.snp'), n.iter = 1000)
geweke.diag(test.snp, frac1 = 0.1, frac2 = 0.5)
gelman.diag(test.snp)
gelman.plot(test.snp, ylim = c(1, 4))


## ----L5S28_2b, results='hide',  message=FALSE, warning=FALSE,out.height = '50%', out.width = '50%', fig.align="center", cache=T---------------------------------------------------
test.snp <-
  coda.samples(jags.snp, c('beta.snp'), n.iter = 8000, thin = 8)
gelman.plot(test.snp, ylim = c(1, 4))


## ----L5S28_2c,  results='hide', message=FALSE, warning=FALSE,out.height = '50%', out.width = '50%', fig.align="center",cache=TRUE-------------------------------------------------
test.snp <-
  coda.samples(jags.snp, c('beta.snp'), n.iter = 16000, thin = 16)
gelman.diag(test.snp)
gelman.plot(test.snp, ylim = c(1, 4))


## ----L5S28_3,  message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------
geweke.diag(beta.snp.out, frac1 = 0.1, frac2 = 0.5)
gelman.diag(test.snp)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Simulation Parameters
### Historical Data
Y12 <-
  24      # events in the treated hist group
Y22 <- 30      # events in the hist comparator group
N12  <- 300    # size of historical active
N22  <- 300    # size of historical control
### New Study Samle size
N1  <- 600          # Number of subjects per group

n0 <-
  matrix(c(rep(N1, 2), c(N12, N22)), 2, 2)  # 1st column New, second Hist
# ist row trt, second CTRL
d   <- 0.05  # NI margin

pi1 <- 0.15 + d  # Rate
pi2 <- 0.15  # Base Rate

s  <- 0.1     # Standard deviation of bias


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rjags)
### Model  JAGS Code
Model <- 'model{
        for (t in 1:2){ # treat 1=exp, 2=std
            for (j in 1:2){ # study 1-current, 2-hist
              Y[t,j]~dbin(p[t,j],n[t,j])
        }
        logit(p[t,2]) <- logit(p[t,1]) + delta[t]
        p[t,1] ~ dbeta(1,1)
        delta[t] ~ dnorm(0,tau[t])
        tau[t] <- pow(sigma[t],-2)
    }
    Pr<-step(p[2,1] + d0 - p[1,1])
}'
writeLines(Model, 'M.txt')

nSim <- 100     ### Number of simulation

Y11 <-
  rbinom(nSim, n0[1, 1], pi1)   # Generate data in the active group
Y21 <-
  rbinom(nSim, n0[2, 1], pi2)   # Generate data in the comparator group

Y <- rbind(Y11, Y21, Y12, Y22)   ### put together: each study one column


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Jf <- function(j, ...) {
  Data <- list(
    n = n0,
    Y = matrix(Y[, j], 2, 2),
    sigma = c(s, s),
    d0 = d
  ) # Data
  M1 <-
    jags.model('M.txt',
               data = Data,
               n.chains = 1,
               quiet = TRUE) # Compile
  update(M1, 1000, progress.bar = 'none')                   # Burn-in
  M2 <-
    coda.samples(M1, c('p', 'Pr'), n.iter = 20000, progress.bar = 'none') # Samples
  summary(M2)[[1]][, 1]                                     # Write out
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(snowfall)  # Use snowfall package to do that
sfInit(parallel = TRUE, cpus = 4)  # Start Cluster
sfLibrary(coda) # Ask for R-Package on cluster
sfLibrary(rjags)
sfExport('Y', 'n0', 'd', 's')   # Export all the data that will be used by the function.
pbb <-
  do.call('cbind', (sfLapply(1:nSim, Jf))) # Distribute calculations
sfStop()   # Stop the cluster

### Calculate Power
Pow <- mean(pbb[1, ] > .975)
cat(
  c(
    '\nGroup Size = ',
    N1,
    '; Power = ',
    Pow,
    '\nEvents hTRT =',
    Y12,
    '; hN TRT = ',
    N12,
    '\nEvents hCTRL =',
    Y22,
    '; hN CTRL = ',
    N22
  )
)
