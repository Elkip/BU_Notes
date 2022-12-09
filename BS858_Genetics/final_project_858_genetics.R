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
n_cases <- nrow(sibs[which(sibs$DEM == 1),])
n_controls <- n*2 - n_cases
control_to_cases <- n_controls / n_cases
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
k <- (sum(tau$DEM1 == 2) + sum(tau$DEM2 == 2)) / (n*2)
# Proportion or effected relatives with effected relatives
k_r = (sum(tau$DEM1 == 2 & tau$DEM2 == 2)) / (sum(tau$DEM1 == 2 | tau$DEM2 == 2))
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