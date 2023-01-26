library("rjags")
library("coda")

set.seed(0)
# 2b. Simulate a sample of 10000 values and generate the empirical density
model_1.tx <- "model {
   theta ~ dbeta(3, 2)
   P1 <- step(theta - .8)
  }"
model_1 <- jags.model(textConnection(model_1.tx))

update(model_1, n.iter = 1000)

test_1 <- coda.samples(model_1, c('theta', 'P1'), 
                        n.iter = 10000)
plot(test1)

# 2c. Estimate the probability theta > .8
summary(test1)

# 2e. Estimate the probability in a set of 50, n = 33 (66%)
model_2.tx <- "model {
   theta ~ dbeta(3, 2)
   Y ~ dbin(theta, 50) 
   P2 <- step(Y - 33)
  }"
model_2 <- jags.model(textConnection(model_2.tx))
update(model_2, n.iter = 1000)
test2 <- coda.samples(model_2, c("Y", "P2"), n.iter = 1000)
summary(test2)

# 3b. Compute the Bayesian estimate of prevalence of carriers
model_3.sc <- "model {
  theta ~ dbeta(7, 13)
}"
model_3 <- jags.model(textConnection(model_3.sc))
update(model_3, n.iter = 1000)
test3 <- coda.samples(model_3, c("theta"), n.iter = 1000)
summary(test3)


# 3c
model_4.sc <- "model {
  theta ~ dbeta(7, 13)
  Y ~ dbin(theta, 100)
  P1 <- step(Y - 50)
}"
model_4 <- jags.model(textConnection(model_4.sc))
update(model_4, n.iter = 1000)
test4 <- coda.samples(model_4, c("Y", "P1"), n.iter = 1000)
summary(test4)

# 3d
# P1 <- step(70 - Y - 30)?
model_5.sc <- "model {
  theta ~ dbeta(7, 13)
  Y ~ dbin(theta, 100)
  P1 <- step(Y - 30)
  P2 <- step(70 - Y)
}"
model_5 <- jags.model(textConnection(model_5.sc))
update(model_5, n.iter = 1000)
test5 <- coda.samples(model_5, c("Y", "P1", "P2"), n.iter = 1000)
summary(test5)

