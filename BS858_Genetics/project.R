tau <- read.csv("/home/elkip/Datasets/project_sib_pheno_and_RV_data.csv")
head(tau)

# 1a. Compute a statistic for heritability

# 1b. Compute a statistic for familiality
# Lambda = P(Disease | affected sib) / prop of affected indiv.
k = sum(tau$DEM1 == 2) / length(tau$DEM1)
k = (sum(tau$DEM1 == 2) + sum(tau$DEM2 == 2)) / length(tau$DEM1)*2
k_p <- sum(tau$DEM1 == 2 & tau$DEM2 == 2)/ sum(tau$DEM1 == 2)
lambda <- k_p / k 
