library("rjags")
library("coda")
library("dplyr")

## Exercise 1
# a. Read dataset and create new family index variable
llfs.data <- read.csv("/home/elkip/Datasets/LLFS.subset.csv")

# b. How many families are there?
length(unique(llfs.data$pedid))

# c. Implement a Bayesian model describing the genetic association
# between each SNP and the outcome using logistic regression adjusted
# for 2 PCs and Sex. Use a hierarchical model with random intercept.
llfs.model1 <- "model{
  mu.pc1 <- mean(PC1[])
  mu.pc2 <- mean(PC2[])
  for (i in 1 : N) {
    pc1[i] <- PC1[i]
    pc2[i] <- PC2[i]
    outcome[i] ~ dbin(p[i], 1)
    logit(p[i]) <- beta.i[fam[i]] + beta.sex*Sex[i] + beta.pc1*(pc1[i] - mu.pc1) 
      + beta.pc2*(pc2[i] - mu.pc2) + beta.snp*snp[i]
  }
  ## Random effect of intercept per family
  for (i in 1:F) {
    beta.i[i] ~ dnorm(beta.0, tau)
  }
  # Priors:
  beta.0 ~ dnorm(0, .01)
  beta.sex ~ dnorm(0, .01)
  beta.pc1 ~ dnorm(0, .01)
  beta.pc2 ~ dnorm(0, .01)
  beta.snp ~ dnorm(0, .01)
  tau ~ gamma(1, 1)
}"