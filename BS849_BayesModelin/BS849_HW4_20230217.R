library(rjags)
library(coda)

# Exercise 0
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

Y.t <- t(rats.data.p$Y)

model.og <- "model
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

rats.data <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), xbar = 22, N = 30, T = 5,	
                  Y = Y.t)

jags.og <- jags.model(textConnection(model.1),data=rats.data, n.adapt=1500)
update(jags.og,1000)
test.og <- coda.samples(jags.og, c('alpha.0','beta.c','sigma.c'), n.adapt=1500, n.iter=1000)
summary(test.og)
plot(test.og)
autocorr.plot(test.og)

model.1 <- "model
    {
        for( i in 1 : N ) {
            for( j in 1 : T ) {
                Y[i , j] ~ dnorm(mu[i , j],tau.c)
                mu[i , j] <- alpha[i] + beta[i] * (x[j] - xbar)
            }
            alpha[i] ~ dnorm(alpha.c,alpha.tau)
            beta[i] ~ dnorm(beta.c,beta.tau)
        }
        tau.c ~ dgamma(.1,.1)
        alpha.c ~ dnorm(0.0,1.0E-6)    
        alpha.tau ~ dgamma(.1,.1)
        beta.c ~ dnorm(0.0,1.0E-6)
        beta.tau ~ dgamma(.1,.1)
        alpha.0 <- alpha.c +beta.c*(8- xbar )
        sigma.c <- 1/tau.c
        sigma.alpha <- 1/alpha.tau
        sigma.beta <- 1/beta.tau
    }"

jags.1 <- jags.model(textConnection(model.1),data=rats.data, n.adapt=1500)
update(jags.1,1000)
test.1 <- coda.samples(jags.1, c('alpha.0','beta.c','sigma.c'), n.adapt=1500, n.iter=1000)
summary(test.1)
plot(test.1)
autocorr.plot(test.1)

model.2 <- "model
    {
        for( i in 1 : N ) {
            for( j in 1 : T ) {
                Y[i , j] ~ dnorm(mu[i , j],tau.c)
                mu[i , j] <- alpha[i] + beta[i] * (x[j] - xbar)
            }
            alpha[i] ~ dnorm(alpha.c,alpha.tau)
            beta[i] ~ dnorm(beta.c,beta.tau)
        }
        tau.c ~ dgamma(.01,.01)
        alpha.c ~ dnorm(0.0,1.0E-6)    
        alpha.tau ~ dgamma(.01,.01)
        beta.c ~ dnorm(0.0,1.0E-6)
        beta.tau ~ dgamma(.01,.01)
        alpha.0 <- alpha.c + beta.c*(8- xbar )
        sigma.c <- 1/tau.c
        sigma.alpha <- 1/alpha.tau
        sigma.beta <- 1/beta.tau
    }"

jags.2 <- jags.model(textConnection(model.2),data=rats.data, n.adapt=1500)
update(jags.2,1000)
test.2 <- coda.samples(jags.2, c('alpha.0','beta.c','sigma.c'), n.adapt=1500, n.iter=1000)
summary(test.2)
plot(test.2)
autocorr.plot(test.2)

model.3 <- "model
    {
        for( i in 1 : N ) {
            for( j in 1 : T ) {
                Y[i , j] ~ dnorm(mu[i , j],tau.c)
                mu[i , j] <- alpha[i] + beta[i] * (x[j] - xbar)
            }
            alpha[i] ~ dnorm(alpha.c,alpha.tau)
            beta[i] ~ dnorm(beta.c,beta.tau)
        }
        tau.c ~ dgamma(.01,.01)
        alpha.c ~ dnorm(0.0,1.0E-6)    
        alpha.tau ~ dgamma(.01,.01)
        beta.c ~ dnorm(0.0,1.0E-6)
        beta.tau ~ dgamma(.01,.01)
        alpha.0 <- alpha.c + beta.c*(8- xbar )
        sigma.c <- 1/tau.c
        sigma.alpha <- 1/alpha.tau
        sigma.beta <- 1/beta.tau
    }"

jags.3 <- jags.model(textConnection(model.3),data=rats.data, n.adapt=1500)
update(jags.3,1000)
test.3 <- coda.samples(jags.3, c('alpha.0','beta.c','sigma.c'), n.adapt=1500, n.iter=1000)
summary(test.3)
plot(test.3)
autocorr.plot(test.3)

# Exercise 1
# 1. Analyze the data using pooled model. Use logistic regression to find the OR
hos.data <-read.csv('/home/elkip/Datasets/data.RCT2.csv')
N <- as.matrix(hos.data[c('Number.treated','Number.untreated')])
Y <- as.matrix(hos.data[c('MI.cases.in.treated','MI.cases.in.untreated')]) # num of cases for treated
TRT<- c(1,0)  # Treated indicator

hos.model1 <- "model {
		for( i in 1 : N ) {
			for( j in 1 : 2 ) {
				Y[i , j] ~ dbin(theta[i,j], n[i,j])
				logit(theta[i,j]) <- alpha[i] + beta.c * (TRT[j])
			}
			alpha[i] ~ dnorm(alpha.c,alpha.tau)
		}
    tau.c ~ dgamma(1, 1)
		alpha.c ~ dnorm(0, .001)	   
		alpha.tau ~ dgamma(1, 1)
		beta.c ~ dnorm(0, .001)
		beta.tau ~ dgamma(1, 1)
}"

hos.list <- list(Y=Y, N=22, TRT=TRT, n=N)
jags.h <- jags.model(textConnection(hos.model1), data=hos.list, n.adapt=1500)
update(jags.h,10000)
test.h.params <- coda.samples(jags.h, c('beta.c'), n.adapt=1500, n.iter=10000)
test.h <- coda.samples(jags.h, c('theta'), n.adapt=1500, n.iter=10000)
summary(test.h.params)
summary(test.h)
out.h <- as.matrix(test.h)
summary(out.h)

# 2. Analyze the data using hierarchical logistic regression
dataj <- list(y=Y,n=N,TRT=c(1,0))
hos.data2 <- "model {
       for (i in 1:22) { # For each study
        for (j in 1:2) { # For each treatment group: j=1 treated,j=2 untreated
          y[i,j] ~ dbin(p[i,j],n[i,j])
          logit(p[i,j]) <- alpha[i] + beta[i]*TRT[j]
          }
        alpha[i] ~ dnorm(alpha0,tau.alpha)
        beta[i] ~ dnorm(beta0,tau.beta)
        OR[i] <- exp(beta[i])
       }
       # Priors
       alpha0 ~ dnorm(0, 0.001)
       beta0 ~ dnorm(0, 0.001)
       tau.alpha ~ dgamma(1, 1)
       tau.beta ~ dgamma(1, 1)
       # Rank of OR
       rank.OR <- rank(OR[])
}"
jags.ran <- jags.model(textConnection(hos.data2),data=dataj, n.adapt=1500)
update(jags.ran,10000)
R.ran <- coda.samples(jags.ran, c('OR'), n.iter=10000)
colMeans(summary(R.ran[,])$quantiles)
R.rank <- coda.samples(jags.ran, c('rank.OR'), n.iter=10000)
out.r <- as.matrix(R.rank)
boxplot(out.r[,order(summary(R.rank)[[2]][,3])])

# 3. Use an independent logistic regression
dataj <- list(y=Y,n=N,TRT=c(1,0))
hos.data3 <- "model {
       for (i in 1:22) { # For each study
        for (j in 1:2) { # For each treatment group: j=1 treated,j=2 untreated
          y[i,j] ~ dbin(p[i,j],n[i,j])
          logit(p[i,j]) <- alpha.c + beta[i]*TRT[j]
          }
        beta[i] ~ dnorm(beta0,tau.beta)
        OR[i] <- exp(beta[i])
       }
       # Priors
       alpha.c ~ dnorm(0, 0.001)
       beta0 ~ dnorm(0, 0.001)
       tau.alpha ~ dgamma(1, 1)
       tau.beta ~ dgamma(1, 1)
       # Rank of OR
       rank.OR <- rank(OR[])
}"
jags.ran2 <- jags.model(textConnection(hos.data3),data=dataj, n.adapt=1500)
update(jags.ran2,10000)
R.ran2 <- coda.samples(jags.ran2, c('OR'), n.iter=10000)
colMeans(summary(R.ran2[,])$quantiles)
R.rank2 <- coda.samples(jags.ran2, c('rank.OR'), n.iter=10000)
out.r2 <- as.matrix(R.rank2)
boxplot(out.r2[,order(summary(R.rank2)[[2]][,3])])

# 5 List ranks
order(summary(R.rank)[[2]][,3])
order(summary(R.rank2)[[2]][,3])

# 6 Predict OR for MI in a new trial
hos.data4 <- "model {
       for (i in 1:22) { # For each study
        for (j in 1:2) { # For each treatment group: j=1 treated,j=2 untreated
          y[i,j] ~ dbin(p[i,j],n[i,j])
          logit(p[i,j]) <- alpha.c + beta[i]*TRT[j]
          }
        beta[i] ~ dnorm(beta0,tau.beta)
        OR[i] <- exp(beta[i])
       }
       # Priors
       alpha.c ~ dnorm(0, 0.001)
       beta0 ~ dnorm(0, 0.001)
       tau.alpha ~ dgamma(1, 1)
       tau.beta ~ dgamma(1, 1)
       beta.new ~ dnorm(beta0, tau.beta)
       # Rank of OR
       OR.new <- exp(beta.new)
}"
jags.ran4 <- jags.model(textConnection(hos.data4),data=dataj, n.adapt=1500)
update(jags.ran4,10000)
R.ran4 <- coda.samples(jags.ran4, c('OR.new'), n.iter=10000)
summary(R.ran4)

# 7. Predict the odds ratio for a new MI in a new trial at hosptial 1
hos.data5 <- "model {
       for (i in 1:22) { # For each study
        for (j in 1:2) { # For each treatment group: j=1 treated,j=2 untreated
          y[i,j] ~ dbin(p[i,j],n[i,j])
          logit(p[i,j]) <- alpha.c + beta[i]*TRT[j]
          }
        beta[i] ~ dnorm(beta0,tau.beta)
       }
       # Priors
       alpha.c ~ dnorm(0, 0.001)
       beta0 ~ dnorm(0, 0.001)
       tau.alpha ~ dgamma(1, 1)
       tau.beta ~ dgamma(2, 1)
       beta.new ~ dnorm(beta0, tau.beta)
       
       OR <- exp(beta[1])
}"
jags.ran5 <- jags.model(textConnection(hos.data5),data=dataj, n.adapt=1500)
update(jags.ran5,10000)
R.ran5 <- coda.samples(jags.ran5, c('OR'), n.iter=10000)
summary(R.ran5)
