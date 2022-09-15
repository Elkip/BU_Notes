
fam <- read.csv("/home/elkip/Datasets/HW1_family1.csv",as.is=T,na=".")

data <- read.csv("/home/elkip/Datasets/HW1_data.csv",as.is=T,na=".")
n_ind <- nrow(data)
n_fam <- length(table(data$famid))
n_pheno <- sum(data$aff != 0)
n_noPheno <- sum(data$aff == 0)
n_geno <- sum(!is.na(data$rs12075_Al1))
n_noGeno <- sum(is.na(data$rs12075_Al1))
prop_geno <- n_geno / n_ind


n_parents <- sum(data$mo == 0 | data$fa == 0)
prop_noGeno_parents = sum(is.na(data$rs12075_Al1) & data$mo == 0)/n_parents
n_mothers <- sum(data$mo == 0 & data$sex == 2)
prop_mothers_noGeno <- sum(data$mo == 0 & data$sex == 2 & is.na(data$rs12075_Al1))/n_mothers
prop_children_noGeno <- sum(data$aff==0 & data$mo!=0)/ sum(data$mo !=0)
n_ind_pheno_geno <- sum(data$aff != 0 & !is.na(data$rs12075_Al1))

n_sibs <- tapply(data$mo!=0, data$famid,sum)
avg_sibs <- mean(n_sibs)

n_sib_types <- tapply(data$mo!=0 & data$aff!=0 & !is.na(data$rs12075_Al1),
                          data$famid,sum)
avf_sibs_types <- mean(n_sib_types)

aff_child_per_fam <- tapply(data$mo!=0 & data$aff!=0 & !is.na(data$rs12075_Al1),
                          data$famid,sum)
n_1affchild <- length(aff_child_per_fam)
n_2aff_child <- sum(aff_child_per_fam >= 2)

n_parents_pheno <- sum(data$mo == 0 & data$aff != 0)
n_parents_pheno_affected <- sum(data$mo == 0 & data$aff == 2)
n_parents_pheno_affected/n_parents_pheno
