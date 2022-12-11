tau <- read.csv("/home/elkip/Datasets/project_sib_pheno_and_RV_data.csv")
n <- nrow(tau)
head(tau)

# 1a. Compute a statistic for heritability
# h2 = 2xICC = sum((x_1i – x_bar)(x_2i – x_bar))/((N-1) SD(X)^2)
sib1 <- tau[,c("famid", "DEM1", "TAU1")]
sib2 <- tau[,c("famid", "DEM2", "TAU2")]
colnames(sib1) <- c("famid", "DEM", "TAU")
colnames(sib2) <- c("famid", "DEM", "TAU")
sibs <- rbind(sib1, sib2)
n_cases <- nrow(tau[which(tau$DEM1 == 2),])
n_controls <- n - n_cases
control_to_cases <- n_controls / n_cases
sample_prevalance <- n_cases / nrow(tau)


# For TAU
mean_tau <- mean(sibs$TAU)
sd_tau <- sd(sibs$TAU)
adj_tau1 <- sib1$TAU - mean_tau
adj_tau2 <- sib2$TAU - mean_tau
icc_tau <- sum(adj_tau1*adj_tau2)/(sd_tau^2*(n-1))
h2_tau <- 2*icc_tau
h2_tau

# For dementia
mean_dem <- mean(sibs$DEM)
sd_dem <- sd(sibs$DEM)
adj_dem1 <- sib1$DEM - mean_dem
adj_dem2 <- sib2$DEM - mean_dem
icc_dem <- sum(adj_dem1*adj_dem2)/(sd_dem^2*(n-1))
h2_dem <- 2*icc_dem
h2_dem

# 1b. Compute a statistic for familiality
# rcc = P(Disease | affected sib) / prop of affected individual
# Total pop prevalence
k <- (sum(tau$DEM1 == 2)) / (n)
# Proportion of effected relatives with effected relatives
k_r = (sum(tau$DEM1 == 2 & tau$DEM2 == 2)) / (sum(tau$DEM1 == 2))
# recurrence risk ratio
lambda <- k_r / k
lambda

# 5 Determine the power of your study in the associations with total-tau
# a Concert the effect size of SNPs to SD Units
tauEff_sd <- 1.41
tauEff_a <- c(-.54/tauEff_sd, -.53/tauEff_sd, .68/tauEff_sd)

# b 
# h2 = 2pqa^2
effFreq <- c(.02, .11, .97)
effFreq_ <- 1 - effFreq
tau_h <- 2*effFreq*effFreq_*tauEff_a^2
tau_h

# c
# NCP = N*h
ncp <- tau_h * n
f <- qf(1-.05, 1, n-1)
pf(f, 1, n-1, ncp, lower.tail = F)

# 6 Determine the power to detect association
# a
# SNP rs111836296
(1/3)/(388/2880)     # f2/f0
(19/177)/(388/2880)  # f1/f0
(1/3)/(19/117)       # f2/f1
# SNP rs74710969
(1/35)/(344/2386)     # f2/f0
(64/579)/(344/2386)  # f1/f0
(1/35)/(64/579)       # f2/f1
# SNP rs111836296
(0)/(399/2831)     # f2/f0
(10/167)/(399/2831)  # f1/f0

# 7 Convert the phenotype in the csv file to a qtrait plink file
write.table(data.frame(tau$famid, tau$famid, tau$TAU1), 
            file = "/home/elkip/Datasets/project.qtrait", quote = FALSE, 
            sep = " ", row.names = FALSE, col.names = FALSE)

# 9
# a Determine whether there is an association with rare variants and total tau
maf <- apply(tau[,11:32], 2, function(i) sum(i)/(2*length(i)))
wieghts <- 1/sqrt(3*maf*(1-maf))
wieghts
wieght_mat <- sapply(1:22, function(i) tau[,i+10]*wieghts[i])
tau$CMC <- apply(tau[,11:32], 1, sum)
tau$CAST <- ifelse(tau$CMC > 0, 1, 0)
tau$MB <- apply(wieght_mat, 1, sum)
lin.reg1 <- lm(TAU1 ~ CMC, data = tau)
lin.reg2 <- lm(TAU1 ~ CAST, data = tau)
lin.reg3 <- lm(TAU1 ~ MB, data = tau)
summary(lin.reg1)
summary(lin.reg2)
summary(lin.reg3)

# 11 
# b Are rare variants associated with dementia?
tau$DEM1 = tau$DEM1 - 1
log.reg1 <- glm(DEM1 ~ CMC, binomial("logit"), data = tau)
log.reg2 <- glm(DEM1 ~ CAST, binomial("logit"), data = tau)
log.reg3 <- glm(DEM1 ~ MB, binomial("logit"), data = tau)
summary(log.reg1)
summary(log.reg2)
summary(log.reg3)

