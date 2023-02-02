library(rjags)

# Read in the data
X.E <- rep(c(1,0,1,0,1,0,1,0), c(30,207,33,327,117,216,25,114))
X.smokes <- rep(c(0,1,0,1),c(237, 333, 360, 139))
Y.D <- rep(c(1,0), c(570, 499))

# 1.1 Model the crude association between MI and coffee drinking
model.1 <- "model{
  ### data model
  for(i in 1:N){
    Y.D[i] ~ dbin(p[i], 1)
    logit(p[i]) <- beta_0+beta_1*X.E[i]
  }
  OR <- exp(beta_1)
  pos.prob <- step(beta_1)
  ### prior
  beta_0 ~ dnorm(0,0.0001)
  beta_1 ~ dnorm(0,0.0001)
}"
# Put data into list
data.1 <- list(N = 1069, X.E = X.E, Y.D = Y.D)
# Compile model; `adapt` for 2,000 samples
model_odds1 <- jags.model(textConnection(model.1),  data = data.1, n.adapt = 2000)
# 5,000 burn-in samples
update(model_odds1, n.iter = 5000)
# Get 10,000 samples from the posterior distribution of OR, beta_0,beta_1
test_odds1  <- coda.samples(model_odds1, 
                           c('OR','beta_1','beta_0', 'pos.prob'), n.iter = 10000)

# 1.2 Model the association between MI and coffee drinking adjusted for smoking
model.2 <- "model{
  ### data model
  for(i in 1:N){
    Y.D[i] ~ dbin(p[i], 1)
    logit(p[i]) <- beta_0+beta_1*X.E[i]+beta_2*X.smokes[i]
  }
  OR <- exp(beta_1)
  ### prior
  beta_0 ~ dnorm(0,0.0001)
  beta_1 ~ dnorm(0,0.0001)
  beta_2 ~ dnorm(0,0.0001)
}"
data.2 <- list(N = 1069, X.E = X.E, X.smokes = X.smokes, Y.D = Y.D)
model_odds2 <- jags.model(textConnection(model.2),  data = data.2, n.adapt = 2000)
update(model_odds2, n.iter = 5000)
test_odds2  <- coda.samples(model_odds2, 
                           c('OR','beta_2','beta_1','beta_0'), n.iter = 10000)

# 1.3 Model the association between MI and coffee drinking with smoking interaction
model.3 <- "model{
  ### data model
  for(i in 1:N){
    Y.D[i] ~ dbin(p[i], 1)
    logit(p[i]) <- beta_0+beta_1*X.E[i]+beta_2*X.smokes[i]+beta_3*X.E[i]*X.smokes[i]
  }
  OR.nonsmoking <- exp(beta_1)
  OR.smoking   <- exp(beta_1+beta_3)
  ROR <- exp(beta_3)
  ### prior
  beta_0 ~ dnorm(0,0.0001)
  beta_1 ~ dnorm(0,0.0001)
  beta_2 ~ dnorm(0,0.0001)
  beta_3 ~ dnorm(0,0.0001)
}"
data.3 <- list(N = 1069, X.E = X.E, X.smokes = X.smokes, Y.D = Y.D)
model_odds3 <- jags.model(textConnection(model.3),  data = data.3, n.adapt = 2000)
update(model_odds3, n.iter = 5000)
test_odds3  <- coda.samples(model_odds3, 
                            c('OR.nonsmoking', 'OR.smoking', 'ROR','beta_3', 'beta_2','beta_1','beta_0'), n.iter = 10000)


# 1.5 Report Measures of Association
summary(test_odds1)
summary(test_odds2)
summary(test_odds3)
