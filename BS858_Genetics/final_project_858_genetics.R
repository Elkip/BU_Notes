tau <- read.csv("/home/elkip/Datasets/project_sib_pheno_and_RV_data.csv")
head(tau)

# 1a. Compute a statistic for heritability
# h2 = 2xICC = sum((x_1i – x_bar)(x_2i – x_bar))/((N-1) SD(X)^2)
sib1 <- tau[,c("famid", "DEM1", "TAU1")]
sib2 <- tau[,c("famid", "DEM2", "TAU2")]
colnames(sib1) <- c("famid", "DEM", "TAU")
colnames(sib2) <- c("famid", "DEM", "TAU")
sibs <- rbind(sib1, sib2)
n <- nrow(sibs)
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
# Lambda = P(Disease | affected sib) / prop of affected individual
k = sum(sib2$DEM == 2) / length(sibs$DEM)
k_p <- sum(tau$DEM1 == 2 & tau$DEM2 == 2)/ sum(tau$DEM1 == 2)
lambda <- k_p / k 
lambda

# 2. Determine and report the allele frequencies for each of
# the three SNPs in the study.
