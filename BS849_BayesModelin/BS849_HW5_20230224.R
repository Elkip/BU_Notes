library("rjags")
library("coda")
library("dplyr")

## Exercise 1
# a. Read dataset and create new family index variable
llfs.data <- na.omit(read.csv("/home/elkip/Datasets/LLFS.subset.csv"))
N <- nrow(llfs.data)
SNP <- llfs.data[,grep("rs", colnames(llfs.data))]
fam.index <- rep(NA, length(llfs.data[,"pedid"]))
for (i in 1:length(unique(llfs.data$pedid))) {
  fam.index[which(llfs.data$pedid == unique(llfs.data$pedid)[i])] <- rep(i, length(which(llfs.data$pedid == unique(llfs.data$pedid)[i])))
}
llfs.data <- llfs.data %>% 
  mutate(findex = fam.index, sex = (sex-1))
N_f <- length(unique(llfs.data$findex))

# b. How many families of 1 are there?
llfs.data %>% 
  count(findex) %>%
  count(n == 1)

# c. Implement a Bayesian model describing the genetic association
# between each SNP and the outcome using logistic regression adjusted
# for 2 PCs and Sex. Use a hierarchical model with random intercept.
llfs.model1 <- "model {
  mu.pc1 <- mean(PC1[])
  mu.pc2 <- mean(PC2[])
  for (i in 1 : N) {
    pc1[i] <- PC1[i]
    pc2[i] <- PC2[i]
    outcome[i] ~ dbin(p[i], 1)
    logit(p[i]) <- b.i[fam[i]] + beta.sex*Sex[i] + beta.pc1*(pc1[i] - mu.pc1) + beta.pc2*(pc2[i] - mu.pc2) + beta.snp*snp[i]
  }
  ## Random effect of intercept per family
  for (i in 1 : N_f) {
    b.i[i] ~ dnorm(beta.0, tau)
  }
  # Priors:
  beta.0 ~ dnorm(0, .01)
  beta.sex ~ dnorm(0, .01)
  beta.pc1 ~ dnorm(0, .01)
  beta.pc2 ~ dnorm(0, .01)
  beta.snp ~ dnorm(0, .01)
  tau ~ dgamma(1, 1)
}"

writeLines(llfs.model1, "llfs_mod.txt")

Y = llfs.data$outcome
l.pc1 = llfs.data$pc1
l.pc2 = llfs.data$pc2
sex = llfs.data$sex

gen_snp_model <- function(i, ...) {
  print(paste("SNP", i))
  snp.list <- list(N = N, N_f = N_f, outcome = Y, snp = SNP[,i], 
                   PC1 = l.pc1, PC2 = l.pc2, Sex = sex, 
                   fam = fam.index)
  jags.snp <- jags.model("llfs_mod.txt", data = snp.list, 
                         n.adapt = 1500, n.chains = 3)
  update(jags.snp, 1000)
  test.snp <- coda.samples(jags.snp, c('beta.snp'), n.adapt = 1000, n.iter = 10000)
  print(summary(test.snp))
  return(as.matrix(test.snp)[,1])
}

t <- Sys.time()
res <- sapply(1:ncol(SNP), gen_snp_model)
Sys.time() - t

t <- Sys.time()
library(snowfall)
sfInit(parallel = TRUE, cpus = 8)
sfLibrary(coda)
sfLibrary(rjags)
sfExport('N', 'N_f', 'Y', 'SNP', 'l.pc1', 'l.pc2', 'sex', 'fam.index')
res <- do.call('cbind', (sfLapply(1:ncol(SNP), gen_snp_model)))
sfStop()
Sys.time() - t

for (i in 1:ncol(SNP)) {
  print(paste("SNP", i))
  boxplot(res[,i])
  OR <- exp(summary(res[,i])[[4]]) # Odds Ratio from mean beta.snp
  low.ci <- quantile(res[,i], .025)
  high.ci <- quantile(res[,i], .975)
  print(OR)
  print(paste0("95% CI: [", exp(low.ci), ", ", exp(high.ci), "]"))
  # f. Geweke Statistic
  print(paste("GWEKE: ", geweke.diag(res[,i])))
}

# g. Gelman-Rubin Statistic using 3 chains for SNP 2
snp.list2 <- list(N = N, N_f = N_f, outcome = Y, snp = SNP[,2], 
                 PC1 = l.pc1, PC2 = l.pc2, Sex = sex, 
                 fam = fam.index)
jags.snp2 <- jags.model("llfs_mod.txt", data = snp.list2, n.adapt = 1500, n.chains = 3)
test.snp2 <- coda.samples(jags.snp2, c('beta.snp'), n.iter = 10000)

gelman.diag(test.snp2)
gelman.plot(test.snp2)

# h. Compute Gelman Ruban for SNP 5
snp.list3 <- list(N = N, N_f = N_f, outcome = Y, snp = SNP[,5], 
                  PC1 = l.pc1, PC2 = l.pc2, Sex = sex, fam = fam.index)
jags.snp3 <- jags.model("llfs_mod.txt", data = snp.list3, n.adapt = 1500, n.chains = 3)
test.snp3 <- coda.samples(jags.snp3, c('beta.snp'), n.iter = 10000)

gelman.diag(test.snp3)
gelman.plot(test.snp3)