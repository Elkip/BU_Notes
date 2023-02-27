library(rjags)
library(coda)
## Exercise 1
# a: Merge 2 distribution of 500 trajectories
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

mu.0  <- c(2, 5)  # Intercepts
mu <- c(5, 7)     # Slopes
n.subj <- 100 

h <- 1 + rbinom(100, 1, 0.7) ## two groups
alpha <- rnorm(100, 0, 1)
y.data <- c()
ind <- c()
for (i in 1:100) {
  ind <- rbind(ind, rep(i, 6))
  y.data <- rbind(y.data,
                  c(mu.0[h[i]] + mu[h[i]] * x + alpha[i] + rnorm(6, 0, 1)))
}

plot(
  x,
  y.data[1, ],
  ylim = c(0, 50),
  xlab = 'Time',
  ylab = 'Outcome',
  type = 'n'
)

for (i in 1:100) {
  lines(x, y.data[i, ], col = h[i])
}

data.traj1 <- list(
  N = 500,
  n.subj = n.subj,
  h = h,
  y = y.data,
  x = x
)

model.1 <- "model
{    
  for(i in 1:n.subj)
  {
    epsilon[ i ] ~ dbin(theta, 1)
    w[i] <- 1+epsilon[i]
    for(j in 1:5)
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

jags.1 <- jags.model(textConnection(model.1), data = data.traj, n.adapt = 1500)
update(jags.1, 1500)
test.1 <- coda.samples(jags.1, c('b0', 'b1', 'theta', 'epsilon'), 
                       n.adapt = 1500, n.iter = 1500)
