
fam <- read.csv("/home/elkip/Datasets/HW1_family1.csv",as.is=T,na=".")

data <- read.csv("/home/elkip/Datasets/HW1_data.csv",as.is=T,na=".")
n_ind <- nrow(data)
n_fam <- length(table(data$famid))
n_pheno <- sum(data$aff != 0)
n_noPheno <- sum(data$aff == 0)
n_geno <- sum(!is.na(data$rs12075_Al1))
n_noGeno <- sum(is.na(data$rs12075_Al1))
prop_geno <- n_geno / n_ind


# hmm
n_parents <- sum(data$mo == 0 | data$fa == 0)
prop_noGeno_parents = sum(is.na(data$rs12075_Al1) & data$mo == 0)/n_parents
mothers <- data$mo