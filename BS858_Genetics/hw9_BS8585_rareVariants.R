geno <- read.csv("/home/elkip/Datasets/pheno_geno_hw9_2022.csv")
snp <- read.csv("/home/elkip/Datasets/snp_info_hw9_2022.csv")

maf <- apply(geno[,2:32], 2, function(i) sum(i)/(2*length(i)))
maf
