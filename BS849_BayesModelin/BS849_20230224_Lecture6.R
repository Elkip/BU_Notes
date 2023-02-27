## ----prep work, echo = FALSE, message=FALSE, warning=FALSE---------------
library("knitr")
library("rjags")
library("coda")
library("formatR")
library("dplyr")
# Prevent code chunks out of range
# Use tidy = FALSE in each code chunk to disable this option
opts_chunk$set(tidy.opts = list(width.cutoff = 60), tidy = TRUE)
#setwd("/Users/xiaoyanliu/Documents/BU_Courses/BS849_Bayesian/Lecture6")


## ---- fig.width=5,fig.height=2, echo=FALSE-------------------------------
par(
  mfrow = c(1, 3),
  mar = c(3.5, 3, 0.05, 0.5),
  mgp = c(2.3, 0.7, 0),
  cex.lab = 0.9,
  cex.axis = 0.9
)
set.seed(10)
x <- rnorm(100, 0, 1)
y <- rnorm(300, 2, 1)
hist(c(x, y), main = '', xlab = 'Observed Value,X')
y <- rnorm(300, 3, 1)
hist(c(x, y), main = '', xlab = 'Observed Value,X')
y <- rnorm(300, 4, 1)
hist(c(x, y), main = '', xlab = 'Observed Value,X')

data.sim1 <- list(N = 400, y = c(x, y))


## ----L6S5_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----
model.1 <- "model{
  p[1] <- theta
  p[2] <- 1 - theta
for( i in 1 : N ) {

 epsilon[i] ~ dcat(p[])

 y[i] ~ dnorm(mu[epsilon[i]],tau[epsilon[i]])
}
  theta ~ dbeta(1,1)

  for (j in 1:2){
    mu[j] ~ dnorm(0.0, .0000001);
    tau[j] ~ dgamma(1,1)
    sigma[j] <- pow(tau[j],-2)
  }
    }"


## ----L6S6_1, message=FALSE, warning=FALSE, fig.align="center",fig.width=3.5,fig.height=2----
par(
  mfrow = c(1, 1),
  mar = c(3.7, 3, 0.05, 0.5),
  mgp = c(1.5, 0.5, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  tck = -0.02
)
set.seed(10)
x <- rnorm(100, 0, 1)
y <- rnorm(300, 4, 1)
z <- c(x, y)
hist(z, main = '', xlab = 'Observed Value,X')
data.sim1 <- list(N = 400, y = c(x, y))


## ----L6S7_1,  results='hide', echo = FALSE, message=FALSE, warning=FALSE----
library(rjags)
jags.1 <-
  jags.model(textConnection(model.1), data = data.sim1, n.adapt = 1500)
update(jags.1, 10000)
test.1 <- coda.samples(jags.1,
                       c('mu', 'sigma', 'theta', 'epsilon'),
                       n.adapt = 1500,
                       n.iter = 10000)


## ----L6S7_2, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=4.5,fig.height=1.7----
par(
  mfrow = c(1, 3),
  mar = c(3.5, 1, 1.5, 0),
  mgp = c(1, 0.2, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.02
)
traceplot(test.1[, c("mu[1]", 'mu[2]', "theta")], lwd = 0.5)


## ----L6S7_3, message=FALSE, warning=FALSE--------------------------------
summary(test.1[, c("mu[1]", "mu[2]", "theta")])


## ----L6S7_4, message=FALSE, warning=FALSE--------------------------------
geweke.diag(test.1[, c("mu[1]", "mu[2]", "theta")], frac1 = 0.1, frac2 =
              0.5)


## ----L6S7_5, echo = FALSE, message=FALSE, warning=FALSE------------------
summ.test1 <- summary(test.1[, c("mu[1]", "mu[2]", "theta")])

mu1 <-
  round(as.vector(summ.test1[[2]]["mu[1]", c("2.5%", "50%", "97.5%")]), 2)
mu2 <-
  round(as.vector(summ.test1[[2]]["mu[2]", c("2.5%", "50%", "97.5%")]), 2)
theta <-
  round(as.vector(summ.test1[[2]]["theta", c("2.5%", "50%", "97.5%")]), 2) * 100


## ----L6S8_1, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=2,fig.height=3----
par(
  mfrow = c(2, 1),
  mar = c(2, 1, 1, 0),
  mgp = c(.5, 0.2, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.03
)
densplot(test.1[, c("epsilon[1]", "epsilon[2]")], col = 2)


## ----echo=FALSE----------------------------------------------------------
t1 <- table(test.1[, c("epsilon[1]")])
comp <- names(t1)[t1 == max(t1)]#choose which component labeled 1 or 2


## ----L6S9_1, message=FALSE, warning=FALSE--------------------------------
summary(test.1[, c("epsilon[1]", "epsilon[2]", "epsilon[3]", "epsilon[5]")])


## ----L6S10_1, message=FALSE, warning=FALSE-------------------------------
temp <- as.data.frame(as.matrix(test.1))
res.epsilon <- temp[, grep("epsil", names(temp))]
z <- data.sim1$y
table(c(rep(1, 100), rep(2, 300)), apply(res.epsilon, 2, median))

## ---- echo=F-------------------------------------------------------------
t2 <- table(c(rep(0, 100), rep(1, 300)), apply(res.epsilon, 2, median))
er <- min(sum(diag(t2)), sum(t2[2, 1], t2[1, 2]))


## ----L6S10_2, message=FALSE, warning=FALSE, fig.align="center",fig.width=3,fig.height=1.6,eval=FALSE----
##    ind.a <-which(apply(res.epsilon,2,median)==1) # Group 1
##    plot(c(1:400)[-ind.a],z[-ind.a],col=3,cex=0.5,xlab='Index',ylab='',ylim=range(z))
##    points(c(1:400)[ind.a],z[ind.a],col=2,pch=22,bg='gray',cex=0.5)

## ----L6S10_2a, message=FALSE, warning=FALSE, fig.align="center",fig.width=3,fig.height=1.6,echo=F----
par(
  mfrow = c(1, 1),
  mar = c(2, 2, 1, 0),
  mgp = c(.65, 0.2, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.03
)
ind.a <- which(apply(res.epsilon, 2, median) == 1) # Group 1
plot(
  c(1:400)[-ind.a],
  z[-ind.a],
  col = 3,
  cex = 0.5,
  xlab = 'Index',
  ylab = '',
  ylim = range(z)
)
points(
  c(1:400)[ind.a],
  z[ind.a],
  col = 2,
  pch = 22,
  bg = 'gray',
  cex = 0.5
)


## ----L6S11_1, message=FALSE, warning=FALSE, fig.align="center",fig.width=3.5,fig.height=2----
par(
  mfrow = c(1, 1),
  mar = c(3.7, 3, 0.05, 0.5),
  mgp = c(1.5, 0.5, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  tck = -0.02
)
set.seed(10)
x <- rnorm(100, 0.5, 1)
y <- rnorm(300, 4, 2)
z <- c(x, y)
hist(z, main = '', xlab = 'Observed Value,X')
data.sim2 <- list(N = 400, y = z)


## ----L6S12_1, echo = FALSE, results='hide', message=FALSE, warning=FALSE----
jags.1 <-
  jags.model(textConnection(model.1), data = data.sim2, n.adapt = 1500)
update(jags.1, 10000)
test.1 <-
  coda.samples(jags.1,
               c('mu', 'sigma', 'theta', 'epsilon'),
               n.adapt = 1500,
               n.iter = 10000)


## ----L6S12_2, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=4,fig.height=1.5----
par(
  mfrow = c(1, 3),
  mar = c(3.5, 1, 1.5, 0),
  mgp = c(1, 0.2, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.02
)
traceplot(test.1[, c("mu[1]", "mu[2]", "theta")])


## ----L6S12_3, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=2,fig.height=1.5----
par(
  mfrow = c(2, 1),
  mar = c(1, 1, 1, 0),
  mgp = c(.5, 0.2, 0),
  cex.lab = 0.4,
  cex.axis = 0.4,
  cex.main = 0.4,
  tck = -0.02
)
densplot(test.1[, c("epsilon[1]", "epsilon[2]")], col = 2)


## ----L6S12_4, message=FALSE, warning=FALSE-------------------------------
summary(test.1[, c("mu[1]", "mu[2]", "theta")])


## ----L6S13_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----
model.2 <- "model{
  p[1]<- theta
  p[2] <- 1 - theta
  for( i in 1 : N ) {
    epsilon[i] ~ dcat(p[])
    y[i] ~ dnorm(mu[epsilon[i]], tau[epsilon[i]])
  }
  theta ~ dbeta(1,1)
  for (j in 1:2){
  tau[j] ~ dgamma(1,1)
   sigma[j] <- pow(tau[j],-2)
  }
    mu[1] ~ dnorm(0.0, .0000001);
    mu[2] <- mu[1] + diff
    diff ~ dgamma(1,1)
    }"


## ----L6S16_1, message=FALSE, warning=FALSE, fig.align="center",fig.width=3,fig.height=1.5,echo=FALSE----
par(
  mfrow = c(1, 1),
  mar = c(3.7, 3, 0.05, 0.5),
  mgp = c(1, 0.5, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  tck = -0.02
)
set.seed(5)
data <- read.csv("data.change.csv", header = T)
data <- data[sample(c(1:nrow(data)), 500), ]
data.list <-
  list(
    N = nrow(data),
    gr.s = data$grip.strength.b,
    ga.s = data$gait.b
  )
hist(data$grip.strength.b, xlab = 'Grip Strength', main = '')


## ----L6S16_2, message=FALSE, warning=FALSE, fig.align="center",fig.width=3,fig.height=1.5,echo=F----
par(
  mfrow = c(1, 1),
  mar = c(3.7, 3, 0.05, 0.5),
  mgp = c(1, 0.5, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  tck = -0.02
)
hist(data$gait.b, xlab = 'Gate Speed', main = '')


## ----L6S17_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----
model.1 <- "model{
    p[1] <- theta
    p[2] <- 1 - theta
for( i in 1 : N ) {
  gr.s[i] ~ dnorm(mu.grs[epsilon[i]], tau.grs[epsilon[i]])
  ga.s[i] ~ dnorm(mu.gas[epsilon[i]], tau.gas[epsilon[i]])
  epsilon[i] ~ dcat(p[])
}
  theta ~ dbeta(1,1)
for (j in 1:2){
  mu.grs[j] ~ dnorm(0.0, .0000001)
  mu.gas[j] ~ dnorm(0.0, .0000001)
  tau.grs[j] ~ dgamma(1,1)
  tau.gas[j] ~ dgamma(1,1)
  }
  }"
set.seed(5)
jags.1 <-
  jags.model(textConnection(model.1), data = data.list, n.adapt = 1500)
update(jags.1, 1500)
test.1 <-
  coda.samples(jags.1,
               c('mu.grs', 'mu.gas', 'theta', 'epsilon', "tau.grs", "tau.gas"),
               n.iter = 1500)


## ----L6S18_1, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=4,fig.height=1.7----
par(
  mfrow = c(2, 3),
  mar = c(2, 1, 1, 0),
  mgp = c(0.6, 0.1, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.02
)
traceplot(test.1[, c("mu.grs[1]", "mu.grs[2]", "mu.gas[1]", "mu.gas[2]", "theta")])


## ----L6S18_2, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=4,fig.height=1.7----
par(
  mfrow = c(2, 3),
  mar = c(2, 1, 1, 0),
  mgp = c(0.6, 0.1, 0),
  cex.lab = 0.4,
  cex.axis = 0.4,
  cex.main = 0.4,
  tck = -0.02
)
autocorr.plot(test.1[, c("mu.grs[1]", "mu.grs[2]", "mu.gas[1]", "mu.gas[2]", "theta")], auto.layout = FALSE)


## ----L6S19_1, echo = FALSE, message=FALSE, warning=FALSE-----------------
summary(test.1[, c(
  "mu.grs[1]",
  "mu.grs[2]",
  "tau.grs[1]",
  "tau.grs[2]",
  "mu.gas[1]",
  "mu.gas[2]",
  "tau.gas[1]",
  "tau.gas[2]",
  "theta"
)])
geweke.diag(test.1[, c("mu.grs[1]", "mu.grs[2]", "mu.gas[1]", "mu.gas[2]", "theta")], frac1 =
              0.1, frac2 = 0.5)

summ.test.1 <-
  summary(test.1[, c(
    "mu.grs[1]",
    "mu.grs[2]",
    "tau.grs[1]",
    "tau.grs[2]",
    "mu.gas[1]",
    "mu.gas[2]",
    "tau.gas[1]",
    "tau.gas[2]",
    "theta"
  )])
summ.epsilon <-
  summary(test.1[, c("epsilon[1]", "epsilon[2]", "epsilon[3]", "epsilon[5]")])#grep("epsil", names(temp))
epsilon1 <-
  mean(unlist(test.1[, c("epsilon[1]")]) == 1)#grep("epsil", names(temp))

grs1 <-
  round(as.vector(summ.test.1[[2]]["mu.grs[1]", c("2.5%", "50%", "97.5%")]), 2)
gas1 <-
  round(as.vector(summ.test.1[[2]]["mu.gas[1]", c("2.5%", "50%", "97.5%")]), 2)
tau.grs1 <-
  round(as.vector(summ.test.1[[2]]["tau.grs[1]", c("2.5%", "50%", "97.5%")]), 2)
tau.gas1 <-
  round(as.vector(summ.test.1[[2]]["tau.gas[1]", c("2.5%", "50%", "97.5%")]), 2)

grs2 <-
  round(as.vector(summ.test.1[[2]]["mu.grs[2]", c("2.5%", "50%", "97.5%")]), 2)
gas2 <-
  round(as.vector(summ.test.1[[2]]["mu.gas[2]", c("2.5%", "50%", "97.5%")]), 2)
tau.grs2 <-
  round(as.vector(summ.test.1[[2]]["tau.grs[2]", c("2.5%", "50%", "97.5%")]), 2)
tau.gas2 <-
  round(as.vector(summ.test.1[[2]]["tau.gas[2]", c("2.5%", "50%", "97.5%")]), 2)

theta <-
  round(as.vector(summ.test.1[[2]]["theta", c("2.5%", "50%", "97.5%")]), 2) * 100

weaker <- ifelse(grs1[2] < grs2[2], 1, 2)


## ----L6S20_1, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=2,fig.height=3----
par(
  mfrow = c(2, 1),
  mar = c(3, 1, 1.2, 0),
  mgp = c(.5, 0.1, 0),
  cex.lab = 0.4,
  cex.axis = 0.4,
  cex.main = 0.4,
  tck = -0.02
)
hist(
  data$grip.strength.b,
  main = '',
  xlab = 'Grip Strength',
  probability = T,
)
x <- seq(0, 70, length = 1000)
lines(x,
      theta[2] / 100 * dnorm(x, grs1[2], sqrt(1 / tau.grs1[2])) + (1 - theta[2] /
                                                                     100) * dnorm(x, grs2[2], sqrt(1 / tau.grs2[2])))
hist(
  data$gait.b,
  main = '',
  xlab = 'Gait Speed',
  probability = T,
  ylim = c(0, 1.35)
)
x <- seq(0, 2, length = 1000)
lines(x,
      theta[2] / 100 * dnorm(x, gas1[2], sqrt(1 / tau.gas1[2])) + (1 - theta[2] /
                                                                     100) * dnorm(x, gas2[2], sqrt(1 / tau.gas2[2])))


## ----L6S21_1, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=4.5,fig.height=2.5----
par(
  mfrow = c(1, 3),
  mar = c(3.5, 1, 1.5, 0),
  mgp = c(1, 0.2, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.02
)
densplot(test.1[, c("epsilon[1]", "epsilon[2]", "epsilon[5]")], col =
           2)


## ----L6S21_2, echo = FALSE, message=FALSE, warning=FALSE-----------------
cbind(summ.epsilon[[1]][-3, c(1, 2)], summ.epsilon[[2]][-3, ])


## ----L6S21_2a, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=4.5,fig.height=2----
par(
  mfrow = c(1, 3),
  mar = c(3.5, 1, 1.5, 0),
  mgp = c(1, 0.2, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.02
)
densplot(test.1[, c("epsilon[1]", "epsilon[2]")], col = 2)
densplot(test.1[, c("theta")])


## ----L6S21_2b, echo = FALSE, message=FALSE, warning=FALSE----------------
rbind(cbind(summ.epsilon[[1]][-3, c(1, 2)], summ.epsilon[[2]][-3, ]),
      theta = round(c(summ.test.1[[1]]["theta", c("Mean", "SD")], summ.test.1[[2]]["theta", c("2.5%", "25%", "50%", "75%", "97.5%")]), 2) * 100)
median.eps1 <- summ.epsilon[[2]]["epsilon[1]", "50%"]


## ----L6S23_3, message=FALSE, warning=FALSE-------------------------------
test <- kmeans(na.omit(data[, c("grip.strength.b", "gait.b")]), 2)
# Cluster means
test[[2]]
# Clustering vector
head(test[[1]])


## ----L6S27_1, include = FALSE, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----
data.list <-
  list(
    N = nrow(data),
    gr.s = data$grip.strength.b,
    ga.s = data$gait.b
  )


## ----L6S27_2, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----
model.1 <- "
  model
  {
    for( i in 1 : N ) {
      gr.s[i] ~ dnorm(mu.grs[epsilon[i]], tau.grs[epsilon[i]])
      ga.s[i] ~ dnorm(mu.gas[epsilon[i]], tau.gas[epsilon[i]])
      epsilon[i] ~ dcat(theta[])
    }
    theta[1:3] ~ ddirch(alpha[])

    for(i in 1:3){
      mu.grs[i]~ dnorm(0.0, .0000001)
      mu.gas[i]~ dnorm(0.0, .0000001)
      tau.grs[i] ~ dgamma(1,1)
      tau.gas[i] ~ dgamma(1,1)
      alpha[i] <- 1
    }
  }
"
library(rjags)
jags.1 <-
  jags.model(textConnection(model.1), data = data.list, n.adapt = 1500)
update(jags.1, 1500)
test.1 <- coda.samples(jags.1,
                       c('mu.grs', 'mu.gas', 'theta'),
                       n.adapt = 1500,
                       n.iter = 1500)


## ----L6S29_1, echo = FALSE, message=FALSE, warning=FALSE,fig.align="center",fig.width=4.5,fig.height=3.2----
par(
  mfrow = c(3, 3),
  mar = c(1.7, 2, 1, 0.5),
  mgp = c(1, 0.3, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.02
)
traceplot(test.1)


## ----L6S30_1, echo = FALSE, message=FALSE, warning=FALSE-----------------
summary(test.1)
summ.test.1 <- summary(test.1)

grs1 <-
  round(as.vector(summ.test.1[[2]]["mu.grs[1]", c("2.5%", "50%", "97.5%")]), 2)
gas1 <-
  round(as.vector(summ.test.1[[2]]["mu.gas[1]", c("2.5%", "50%", "97.5%")]), 2)

grs2 <-
  round(as.vector(summ.test.1[[2]]["mu.grs[2]", c("2.5%", "50%", "97.5%")]), 2)
gas2 <-
  round(as.vector(summ.test.1[[2]]["mu.gas[2]", c("2.5%", "50%", "97.5%")]), 2)

grs3 <-
  round(as.vector(summ.test.1[[2]]["mu.grs[3]", c("2.5%", "50%", "97.5%")]), 2)
gas3 <-
  round(as.vector(summ.test.1[[2]]["mu.gas[3]", c("2.5%", "50%", "97.5%")]), 2)

theta1 <-
  round(as.vector(summ.test.1[[2]]["theta[1]", c("2.5%", "50%", "97.5%")]), 2) * 100
theta2 <-
  round(as.vector(summ.test.1[[2]]["theta[2]", c("2.5%", "50%", "97.5%")]), 2) * 100

out.grs <- c(grs1[2], grs2[2], grs3[2])
weaker.3 <- which(out.grs == min(out.grs))
max.3 <- which(out.grs == max(out.grs))


## ----L6S31_1, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=4.5,fig.height=2----
par(
  mfrow = c(1, 2),
  mar = c(3.5, 1, 1.5, 0),
  mgp = c(1, 0.2, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.02
)
test.1 <- coda.samples(jags.1, c('epsilon'),
                       n.adapt = 1500, n.iter = 1500)

densplot(test.1[, c("epsilon[1]", "epsilon[9]")], col = 2)


## ----L6S34_1, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=2,fig.height=2----
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
x <- c(1:5)

mu.0  <- c(0.1, 2)
mu <- c(0.2, 0.8)
n.subj <- 100

h <- 1 + rbinom(100, 1, 0.2) ## two groups
alpha <- rnorm(100, 0, 1)
y.data <- c()
ind <- c()
for (i in 1:100) {
  ind <- rbind(ind, rep(i, 5))
  y.data <- rbind(y.data,
                  c(mu.0[h[i]] + mu[h[i]] * x + alpha[i] + rnorm(5, 0, 1)))
}

plot(
  x,
  y.data[1, ],
  ylim = c(-10, 10),
  xlab = 'Time',
  ylab = 'Outcome',
  type = 'n'
)
for (i in 1:100) {
  lines(x, y.data[i, ], col = h[i])
}

data.traj <- list(
  N = 500,
  n.subj = n.subj,
  h = h,
  y = y.data,
  x = c(1, 2, 3, 4, 5)
)


## ----L6S36_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----
model.1 <- "
  model
  {    for(i in 1:n.subj){
    epsilon[ i ] ~ dbin(theta, 1)
    w[i] <- 1+epsilon[i]
    for(j in 1:5){
    y[i,j] ~ dnorm( mu[i,j], tau)
    mu[i,j] <- b0[ w[i] ]+b1[ w[i] ]* x[j]
    }
  }

  ## cluster effects and fixed effects
  for(i in 1:2){
    b1[i] ~ dnorm(0,0.01)
    }
  b0[1] ~ dnorm(0,0.001) ## 0; ## constraint parameters for identifiability
  b0[2] <- b0[1]+ delta;
  delta ~ dgamma(1,1)

  ### variance components
  theta ~ dbeta(1,1)
  tau ~ dgamma(1,1)
  }
"

jags.1 <-
  jags.model(textConnection(model.1), data = data.traj, n.adapt = 1500)
update(jags.1, 1500)
test.1 <-
  coda.samples(jags.1,
               c('b0', 'b1', 'theta', 'epsilon'),
               n.adapt = 1500,
               n.iter = 1500)


## ----L6S37_1, eval = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=2,fig.height=2----
## par(mfrow=c(1,1),mar=c(3,2,1.5,0),mgp=c(1,0.2,0),cex.lab=0.5,cex.axis=0.5,cex.main=0.5, tck=-0.02)
##  set.seed(10)
##  set.seed(10)
##  x <- c(1:5);
##  mu.0  <- c(0.1, 2)
##     mu <- c(0.2, 0.8)
##  n.subj <- 100
##
##       h <- 1+rbinom(100, 1, 0.2) ## two groups
##   alpha <- rnorm(100,0,1)
##  y.data <- c(); ind <- c()
##     for(i in 1:100){
##        ind <- rbind(ind, rep(i, 5))
##     y.data <- rbind(y.data,
##     c( mu.0[h[i]]+mu[h[i]]*x+alpha[i]+rnorm(5,0,1)))}
##
## plot(x, y.data[1,], ylim=c(-10,10),xlab='Time',ylab='Outcome',type='n')
##   for(i in 1:100){
##   lines(x, y.data[i,],col=h[i])
##     }
##
##    data.traj <- list(N= 500, n.subj = n.subj,
##               h=h,
##               y= y.data,
##               x=c(1,2,3,4,5))


## ----L6S37_2, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=2,fig.height=2----
par(
  mfrow = c(1, 1),
  mar = c(3, 2, 1.5, 0),
  mgp = c(1, 0.2, 0),
  cex.lab = 0.5,
  cex.axis = 0.5,
  cex.main = 0.5,
  tck = -0.02
)
plot(
  x,
  y.data[1, ],
  ylim = c(-10, 10),
  xlab = 'Time',
  ylab = 'Outcome',
  type = 'n'
)
for (i in 1:100) {
  lines(x, y.data[i, ], col = h[i])
}


## ----L6S38_1, echo = FALSE, message=FALSE, warning=FALSE-----------------
summ.test.1 <-
  summary(test.1[, c("b0[1]", "b0[2]", "b1[1]", "b1[2]", "theta")])

summ.test.1[[1]]
summ.test.1[[2]]
geweke.diag(test.1[, c("b0[1]", "b0[2]", "b1[1]", "b1[2]", "theta")], frac1 =
              0.1, frac2 = 0.5)


b0.1 <-
  round(as.vector(summ.test.1[[2]]["b0[1]", c("2.5%", "50%", "97.5%")]), 2)
b0.2 <-
  round(as.vector(summ.test.1[[2]]["b0[2]", c("2.5%", "50%", "97.5%")]), 2)

b1.1 <-
  round(as.vector(summ.test.1[[2]]["b1[1]", c("2.5%", "50%", "97.5%")]), 2)
b1.2 <-
  round(as.vector(summ.test.1[[2]]["b1[2]", c("2.5%", "50%", "97.5%")]), 2)

theta <-
  round(as.vector(summ.test.1[[2]]["theta", c("2.5%", "50%", "97.5%")]), 2) * 100


## ----L6S38_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
par(mfrow = c(3, 2))
traceplot(test.1[, c("b0[1]", "b0[2]", "b1[1]", "b1[2]", "theta")])


## ----L6S46_1, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----
model.1 <- "
model
  {for(t  in 2 : N ){
    epsilon[t] ~ dcat(p.epsilon[ epsilon[t-1] ,] )
    mu.eps[ t ] <- theta.1*sin(2*pi*t/52)+theta.2*cos( 2*pi*t/52) +
    equals(1,epsilon[t]) * beta.1+
    equals(2,epsilon[t]) * (beta.1+beta.2) +
    equals(3,epsilon[t]) *( beta.1+beta.2+beta.3)
    x[t] ~ dnorm( mu.eps[ t], tau)
  }

  p.epsilon[1,1:3]  ~  ddirch( alpha.1[]);
  p.epsilon[2,1:3]  ~  ddirch( alpha.1[]);
  p.epsilon[3,1:3]  ~  ddirch( alpha.1[])

  theta.1 ~ dnorm( 1,0.1);
  theta.2 ~ dnorm( 1,0.1)
  beta.1 ~ dnorm(150, 0.01);
  beta.2 ~ dnorm( 0, 0.01)
  beta.3 ~ dnorm( 0, 0.01)
  tau ~dgamma(0.01,0.01)
}
"


## ----L6S47_1, echo = FALSE, message=FALSE, warning=FALSE-----------------
list(beta.1 = 5.5,
     theta.1 = 1,
     theta.2 = 1)

  ##### data
 data.1 <-list(  N=393,   pi=3.141593,
          alpha.1=c(1,1,1),
          epsilon = c(1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
x =c(240, 211, 237, 222, 244, 230, 276, 289, 326, 277, 271, 298, 
307, 387, 456, 437, 475, 355, 318, 300, 370, 367, 427, 380, 386, 
467, 524, 514, 370, 307, 306, 333, 367, 380, 381, 426, 373, 333, 
279, 350, 316, 337, 280, 281, 253, 236, 218, 282, 304, 294, 267, 
238, 256, 234, 226, 211, 200, 172, 199, 230, 189, 162, 151, 228, 
196, 266, 309, 262, 260, 279, 277, 309, 367, 377, 491, 586, 552, 
465, 379, 462, 375, 364, 391, 445, 416, 423, 523, 523, 405, 404, 
387, 351, 300, 271, 270, 280, 260, 238, 216, 264, 298, 301, 217, 
219, 189, 197, 179, 160, 166, 187, 165, 184, 235, 193, 219, 240, 
296, 357, 346, 332, 329, 272, 310, 300, 306, 318, 310, 352, 345, 
324, 387, 442, 460, 404, 483, 486, 465, 428, 385, 334, 280, 305, 
261, 259, 237, 252, 240, 253, 209, 208, 204, 227, 227, 227, 201, 
158, 194, 168, 194, 187, 173, 183, 167, 164, 170, 155, 165, 141, 
194, 196, 237, 252, 264, 214, 264, 273, 323, 320, 352, 338, 345, 
322, 342, 410, 394, 322, 376, 459, 474, 565, 456, 480, 319, 291, 
293, 266, 243, 253, 265, 270, 245, 228, 247, 354, 324, 261, 280, 
192, 214, 211, 209, 192, 183, 174, 167, 196, 181, 195, 188, 238, 
195, 221, 328, 376, 276, 267, 265, 293, 282, 348, 318, 344, 332, 
361, 413, 531, 486, 455, 343, 341, 272, 342, 411, 373, 372, 338, 
295, 312, 315, 275, 308, 239, 266, 253, 233, 286, 282, 249, 284, 
239, 195, 187, 183, 186, 158, 153, 174, 159, 169, 175, 169, 185, 
194, 224, 219, 248, 241, 303, 263, 219, 244, 284, 275, 289, 287, 
311, 297, 328, 427, 482, 465, 514, 522, 476, 438, 384, 337, 264, 
270, 252, 259, 304, 306, 296, 253, 237, 324, 348, 264, 263, 251, 
233, 206, 189, 144, 145, 166, 141, 161, 173, 198, 187, 200, 195, 
180, 223, 245, 278, 292, 280, 265, 268, 263, 315, 322, 326, 294, 
311, 328, 357, 437, 411, 376, 421, 496, 426, 421, 387, 386, 279, 
290, 274, 259, 281, 249, 230, 234, 182, 200, 276, 230, 227, 249, 
163, 198, 199, 172, 204, 202, 161, 183, 170, 188, 180, 195, 184, 
196, 219, 231, 234, 280, 264, 282, 260, 257, 273, 306, 281, 342, 
300, 349, 348, 338)
)
 
 str(data.1)


## ----L6S48_1, echo = FALSE, results = "hide", fig.show="hide", message=FALSE, warning=FALSE----
jags.1 <- jags.model(textConnection(model.1),data=data.1, n.adapt=1500)
update(jags.1, 1500)
test.1 <- coda.samples(jags.1, c('beta.1','beta.2','beta.3','p.epsilon', 'theta.1', 'theta.2', 'epsilon'),
                       n.adapt=1500, n.iter=10000)

summ.test1 <- summary(test.1)
p.epsilon11 <- round(as.vector(summ.test1[[2]]["p.epsilon[1,1]", c("2.5%", "50%", "97.5%")]), 2)
p.epsilon12 <- round(as.vector(summ.test1[[2]]["p.epsilon[1,2]", c("2.5%", "50%", "97.5%")]), 2)
p.epsilon13 <- round(as.vector(summ.test1[[2]]["p.epsilon[1,3]", c("2.5%", "50%", "97.5%")]), 2)
p.epsilon21 <- round(as.vector(summ.test1[[2]]["p.epsilon[2,1]", c("2.5%", "50%", "97.5%")]), 2)
p.epsilon22 <- round(as.vector(summ.test1[[2]]["p.epsilon[2,2]", c("2.5%", "50%", "97.5%")]), 2)
p.epsilon23 <- round(as.vector(summ.test1[[2]]["p.epsilon[2,3]", c("2.5%", "50%", "97.5%")]), 2)
p.epsilon31 <- round(as.vector(summ.test1[[2]]["p.epsilon[3,1]", c("2.5%", "50%", "97.5%")]), 2)
p.epsilon32 <- round(as.vector(summ.test1[[2]]["p.epsilon[3,2]", c("2.5%", "50%", "97.5%")]), 2)
p.epsilon33 <- round(as.vector(summ.test1[[2]]["p.epsilon[3,3]", c("2.5%", "50%", "97.5%")]), 2)


## ----L6S48_2, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center",fig.width=4.5,fig.height=2----
par(mfrow=c(1,3),mar=c(3.5,1,1.5,0),mgp=c(1,0.2,0),cex.lab=0.5,cex.axis=0.5,cex.main=0.5, tck=-0.02)
traceplot(test.1[, c("p.epsilon[1,1]", "p.epsilon[1,2]", "p.epsilon[1,3]")])


## ----L6S49_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
summary(test.1[, c("theta.1","theta.2","beta.1","beta.2","beta.3")])


## ----L6S50_1, echo = FALSE, message=FALSE, warning=FALSE,fig.align="center",fig.width=4.5,fig.height=3.2----
par(mfrow=c(3,3),mar=c(1.7,2,1,0.5),mgp=c(1,0.3,0),cex.lab=0.5,cex.axis=0.5, cex.main=0.5,tck=-0.02)
densplot(test.1[, c("epsilon[2]", "epsilon[3]", "epsilon[4]", "epsilon[5]", "epsilon[6]", "epsilon[7]", "epsilon[8]", "epsilon[9]","epsilon[10]")], col=2)


## ----L6S51_1, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
names = paste0("epsilon[", 2:25,"]")

summary(test.1[, names])[[1]]


## ----L6S51_2, echo = FALSE, message=FALSE, warning=FALSE, out.height = '50%', out.width = '50%', fig.align="center"----
names = paste0("epsilon[", 2:25,"]")

summary(test.1[, names])[[2]]

