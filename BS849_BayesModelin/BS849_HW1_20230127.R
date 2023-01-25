library("rjags")
library("coda")

model_1.tx <- "model {
   theta ~ dbeta(3, 2)
   Y ~ dbin(theta, 10000) 
   P1 <- step(theta - .8)
   P2 <- step(Y - 33)
  }"
model_1 <- jags.model(textConnection(model_1.tx))

# Do I have to use update??
# update(model_1, n.iter = 1000)

# 2b. Simluate a sample of 10000 values and generate the empirical density
test_1 <- coda.samples(model_1, c('theta', 'Y'), 
                        n.iter = 10000)
plot(test_1)

# 2c. Estimate the probability theta > .8
test2 <- coda.samples(model_1, c("theta", "P1"), n.iter = 1000)
summary(test2)

# 2e. Estimate the probability in a set of 50, n = 33 (66%)
model_2.tx <- "model {
   theta ~ dbeta(3, 2)
   Y ~ dbin(theta, 50) 
   P2 <- step(Y - 33)
  }"
model_2 <- jags.model(textConnection(model_2.tx))
test3 <- coda.samples(model_2, c("Y", "P2"), n.iter = 1000)
summary(test3)
