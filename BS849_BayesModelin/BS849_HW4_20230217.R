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
