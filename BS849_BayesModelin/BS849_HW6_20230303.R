library(rjags)
library(coda)
## Exercise 1
# a Merge 2 distribution of 500 trajectories
par(
  mfrow = c(1, 1),
  mar = c(3, 2, 1.5, 0),
  mgp = c(1, 0.2, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.02
)
set.seed(10)
x <- c(1:6)

mu.01  <- c(2, 5)  # Intercepts
mu1 <- c(5, 7)     # Slopes
n.subj <- 100 

h1 <- 1 + rbinom(100, 1, 0.7) ## two groups
alpha1 <- rnorm(100, 0, 1)
y.data1 <- c()
ind1 <- c()

for (i in 1:500) {
  ind1 <- rbind(ind1, rep(i, 6))
  y.data1 <- rbind(y.data1,
                  c(mu.01[h1[i]] + mu1[h1[i]] * x + alpha1[i] + rnorm(6, 0, 1)))
}

plot(
  x,
  y.data1[1, ],
  ylim = c(0, 50),
  xlab = 'Time',
  ylab = 'Outcome',
  type = 'n'
)

for (i in 1:500) {
  lines(x, y.data1[i, ], col = h1[i])
}


# b Merge 3 distributions of 500 trajectories
set.seed(10)

mu.02  <- c(2, 5, 6)  # Intercepts
mu2 <- c(5, 7, 2)     # Slopes

h2 <- 1 + rbinom(100, 1, 0.5)
for (i in 1:length(h2)) {
  if (h2[i]==2) {
    h2[i] <- h2[i] + rbinom(1, 1, .4) ## three groups
  }
}
hist(h2)
alpha2 <- rnorm(100, 0, 1)
y.data2 <- c()
ind2 <- c()
for (i in 1:500) {
  ind2 <- rbind(ind2, rep(i, 6))
  y.data2 <- rbind(y.data2,
                  c(mu.02[h2[i]] + mu2[h2[i]] * x + alpha2[i] + rnorm(6, 0, 1)))
}

plot(
  x,
  y.data2[1, ],
  ylim = c(0, 50),
  xlab = 'Time',
  ylab = 'Outcome',
  type = 'n'
)

for (i in 1:500) {
  lines(x, y.data2[i, ], col = h2[i])
}

# c Analyze the 2 model cluster the a Bayesian model
data.traj1 <- list(
  n.subj = n.subj,
  y = y.data1,
  x = x
)

model.1 <- "model
{    
  for(i in 1:n.subj)
  {
    epsilon[ i ] ~ dbin(theta, 1)
    w[i] <- 1+epsilon[i]
    for(j in 1:6)
    {
      y[i,j] ~ dnorm(mu[i,j], tau)
      mu[i,j] <- b0[ w[i] ]+b1[ w[i] ]* x[j]
    }
  }

  ## cluster effects and fixed effects
  for(i in 1:2)
  {
    b1[i] ~ dnorm(0,0.01)
  }

  b0[1] ~ dnorm(0,0.001) ## 0; ## constraint parameters for identifiability
  b0[2] <- b0[1]+ delta;
  delta ~ dgamma(1,1)

  ### variance components
  theta ~ dbeta(1,1)
  tau ~ dgamma(1,1)
}"

jags.1 <- jags.model(textConnection(model.1), data = data.traj1, n.adapt = 1500)
update(jags.1, 1500)
test.1 <- coda.samples(jags.1, c('b0', 'b1', 'theta', 'epsilon'), 
                       n.adapt = 1500, n.iter = 1500)

summ.test.1 <- summary(test.1[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")])

summ.test.1[[1]]
summ.test.1[[2]]
geweke.diag(test.1[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")], frac1=0.1, frac2=0.5)
plot(test.1[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")])
autocorr.plot(test.1[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")])

b0.1 <- round(as.vector(summ.test.1[[2]]["b0[1]", c("2.5%", "50%", "97.5%")]), 2)
b0.2 <- round(as.vector(summ.test.1[[2]]["b0[2]", c("2.5%", "50%", "97.5%")]), 2)

b1.1 <- round(as.vector(summ.test.1[[2]]["b1[1]", c("2.5%", "50%", "97.5%")]), 2)
b1.2 <- round(as.vector(summ.test.1[[2]]["b1[2]", c("2.5%", "50%", "97.5%")]), 2)

theta <- round(as.vector(summ.test.1[[2]]["theta", c("2.5%", "50%", "97.5%")]), 2) * 100

plot(
  x,
  y.data1[1, ],
  ylim = c(0, 50),
  xlab = 'Time',
  ylab = 'Outcome',
  type = 'n'
)

for (i in 1:500) {
  lines(x, y.data1[i, ], col = h1[i])
}


abline(b0.1[2], b1.1[2], col=c("green"), lwd=5)
abline(b0.2[2], b1.2[2], col=c("pink"), lwd=5)

# 3 clusters
data.traj2 <- list(
  n.subj = n.subj,
  y = y.data2,
  x = x
)

jags.1 <- jags.model(textConnection(model.1), data = data.traj2, n.adapt = 1500)
update(jags.1, 1500)
test.1 <- coda.samples(jags.1, c('b0', 'b1', 'theta', 'epsilon'), 
                       n.adapt = 1500, n.iter = 10000)

summ.test.1 <- summary(test.1[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")])

summ.test.1[[1]]
summ.test.1[[2]]
geweke.diag(test.1[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")], frac1=0.1, frac2=0.5)
plot(test.1[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")])
autocorr.plot(test.1[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")])

b0.1 <- round(as.vector(summ.test.1[[2]]["b0[1]", c("2.5%", "50%", "97.5%")]), 2)
b0.2 <- round(as.vector(summ.test.1[[2]]["b0[2]", c("2.5%", "50%", "97.5%")]), 2)

b1.1 <- round(as.vector(summ.test.1[[2]]["b1[1]", c("2.5%", "50%", "97.5%")]), 2)
b1.2 <- round(as.vector(summ.test.1[[2]]["b1[2]", c("2.5%", "50%", "97.5%")]), 2)

theta <- round(as.vector(summ.test.1[[2]]["theta", c("2.5%", "50%", "97.5%")]), 2) * 100

plot(
  x,
  y.data1[1, ],
  ylim = c(0, 50),
  xlab = 'Time',
  ylab = 'Outcome',
  type = 'n'
)

for (i in 1:500) {
  lines(x, y.data2[i, ], col = h2[i])
}


abline(b0.1[2], b1.1[2], col=c("green"), lwd=5)
abline(b0.2[2], b1.2[2], col=c("pink"), lwd=5)

# d Analyze the 3 model cluster with a Bayesian model
# 2 clusters
data.traj1 <- list(
  n.subj = n.subj,
  y = y.data1,
  x = x
)

model.1 <- "model
{    
  for(i in 1:n.subj)
  {
    epsilon[ i ] ~ dcat(theta[])
    # epsilon[ i ] ~ dcat(theta, 1)
    # w[i] <- 1+epsilon[i]
    for(j in 1:6)
    {
      y[i,j] ~ dnorm(mu[i,j], tau)
      mu[i,j] <- b0[ epsilon[i] ]+b1[ epsilon[i] ]* x[j]
    }
  }

  ## cluster effects and fixed effects
  for(i in 1:3)
  {
    b1[i] ~ dnorm(0,0.01)
  }

  b0[1] ~ dnorm(0,0.001) ## 0; ## constraint parameters for identifiability
  b0[2] <- b0[1] + delta;
  b0[3] <- b0[2] + delta2;
  delta ~ dgamma(1,1)
  delta2 ~ dgamma(1,1)

  ### variance components
  # theta ~ ddirch(alpha[])
  theta[1:3] ~ ddirch(alpha[])
  alpha[1] <- 1
  alpha[2] <- 1
  alpha[3] <- 1
  tau ~ dgamma(1,1)
}"

jags.1 <- jags.model(textConnection(model.1), data = data.traj1, n.adapt = 1500)
update(jags.1, 1500)
test.1 <- coda.samples(jags.1, c('b0', 'b1', 'theta', 'epsilon'), n.adapt = 1500, n.iter = 1500)

summ.test.1 <- summary(test.1[, c("b0[1]","b0[2]","b0[3]","b1[1]","b1[2]","b1[3]")])

summ.test.1[[1]]
summ.test.1[[2]]
geweke.diag(test.1[, c("b0[1]","b0[2]", "b0[3]","b1[1]","b1[2]", "b1[3]")], frac1=0.1, frac2=0.5)
plot(test.1[, c("b0[1]","b0[2]", "b0[3]","b1[1]","b1[2]", "b1[3]")])
autocorr.plot(test.1[, c("b0[1]","b0[2]", "b0[3]","b1[1]","b1[2]", "b1[3]")])

b0.1 <- round(as.vector(summ.test.1[[2]]["b0[1]", c("2.5%", "50%", "97.5%")]), 2)
b0.2 <- round(as.vector(summ.test.1[[2]]["b0[2]", c("2.5%", "50%", "97.5%")]), 2)
b0.3 <- round(as.vector(summ.test.1[[2]]["b0[3]", c("2.5%", "50%", "97.5%")]), 2)

b1.1 <- round(as.vector(summ.test.1[[2]]["b1[1]", c("2.5%", "50%", "97.5%")]), 2)
b1.2 <- round(as.vector(summ.test.1[[2]]["b1[2]", c("2.5%", "50%", "97.5%")]), 2)
b1.3 <- round(as.vector(summ.test.1[[2]]["b1[3]", c("2.5%", "50%", "97.5%")]), 2)

plot(
  x,
  y.data1[1, ],
  ylim = c(0, 50),
  xlab = 'Time',
  ylab = 'Outcome',
  type = 'n'
)

for (i in 1:500) {
  lines(x, y.data1[i, ], col = h1[i])
}

abline(b0.1[2], b1.1[2], col=c("green"), lwd=5)
abline(b0.2[2], b1.2[2], col=c("pink"), lwd=5)
abline(b0.3[2], b1.3[2], col=c("orange"), lwd=5)

# 3 clusters
data.traj1 <- list(
  n.subj = n.subj,
  y = y.data2,
  x = x
)

jags.1 <- jags.model(textConnection(model.1), data = data.traj1, n.adapt = 1500)
update(jags.1, 1500)
test.1 <- coda.samples(jags.1, c('b0', 'b1', 'theta', 'epsilon'), n.adapt = 1500, n.iter = 10000)

summ.test.1 <- summary(test.1[, c("b0[1]","b0[2]","b0[3]","b1[1]","b1[2]","b1[3]")])

summ.test.1[[1]]
summ.test.1[[2]]
geweke.diag(test.1[, c("b0[1]","b0[2]", "b0[3]","b1[1]","b1[2]", "b1[3]")], frac1=0.1, frac2=0.5)
plot(test.1[, c("b0[1]","b0[2]", "b0[3]","b1[1]","b1[2]", "b1[3]")])
autocorr.plot(test.1[, c("b0[1]","b0[2]", "b0[3]","b1[1]","b1[2]", "b1[3]")])

b0.1 <- round(as.vector(summ.test.1[[2]]["b0[1]", c("2.5%", "50%", "97.5%")]), 2)
b0.2 <- round(as.vector(summ.test.1[[2]]["b0[2]", c("2.5%", "50%", "97.5%")]), 2)
b0.3 <- round(as.vector(summ.test.1[[2]]["b0[3]", c("2.5%", "50%", "97.5%")]), 2)

b1.1 <- round(as.vector(summ.test.1[[2]]["b1[1]", c("2.5%", "50%", "97.5%")]), 2)
b1.2 <- round(as.vector(summ.test.1[[2]]["b1[2]", c("2.5%", "50%", "97.5%")]), 2)
b1.3 <- round(as.vector(summ.test.1[[2]]["b1[3]", c("2.5%", "50%", "97.5%")]), 2)

theta <- round(as.vector(summ.test.1[[2]]["theta", c("2.5%", "50%", "97.5%")]), 2) * 100

plot(
  x,
  y.data1[1, ],
  ylim = c(0, 50),
  xlab = 'Time',
  ylab = 'Outcome',
  type = 'n'
)

for (i in 1:500) {
  lines(x, y.data2[i, ], col = h2[i])
}

abline(b0.1[2], b1.1[2], col=c("green"), lwd=5)
abline(b0.2[2], b1.2[2], col=c("pink"), lwd=5)
abline(b0.3[2], b1.3[2], col=c("orange"), lwd=5)
