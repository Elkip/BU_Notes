geno <- read.csv("/home/elkip/Datasets/pheno_geno_hw9_2022.csv")
snp <- read.csv("/home/elkip/Datasets/snp_info_hw9_2022.csv")

maf <- apply(geno[,2:25], 2, function(i) sum(i)/(2*length(i)))
maf

snp$maf <- maf
snp

rare <- which(snp$maf <= .01)
rare_snp = snp[rare,]
rare_geno = geno[,rare+1]
common <- which(snp$maf > .01)

# 3. Create weighted sum MB scores
# a. MB Weights all variants
dim(geno)
weights <- 1/sqrt(1000*snp$maf*(1-snp$maf))
weights
geno_weighted <- sapply(1:24, function(i) geno[,i+1]*weights[i])
dim(geno_weighted)
geno$MB <- apply(geno_weighted, 1, sum)
summary(geno$MB)
sd(geno$MB)

# Test for assocation with qt adj for age sex and pop
m1 <- lm(qt ~ sex + age + pop1 + pop2 + pop3 + MB, data = geno)
summary(m1)

# b. MB weights variants maf < .01
dim(rare_snp)
weights <- 1/sqrt(1000*rare_snp$maf*(1-rare_snp$maf))
weights
rare_geno_weighted <- sapply(1:19, function(i) rare_geno[,i]*weights[i])
dim(rare_geno_weighted)
geno$MB <- apply(rare_geno_weighted, 1, sum)
summary(geno$MB)
sd(geno$MB)

# Test for association with qt adj for age sex and pop
m2 <- lm(qt ~ sex + age + pop1 + pop2 + pop3 + MB, data = geno)
summary(m2)

# 4. Create CMC scores with the indicated variants
# a. maf < .01 SNPs
geno$CMC <- apply(geno[,rare+1], 1, sum)
table(geno$CMC)

# Test for association with qt adj for age sex and pop
m3 <- lm(qt ~ sex + age + pop1 + pop2 + pop3 + CMC, data = geno)
summary(m3)

# b. Nonsynonymous variants
nonsyn = which(snp$vartype == "Nonsynonymous")
nonsyn_snp <- snp[nonsyn,]
nonsyn_geno <- geno[,nonsyn+1]
geno$CMC <- apply(nonsyn_geno, 1, sum)
table(geno$CMC)

# Test for association with qt adj for age sex and pop
m4 <- lm(qt ~ sex + age + pop1 + pop2 + pop3 + CMC, data = geno)
summary(m4)

# c. nonsynonymous variants with maf < .01
nonsyn_rare = which(snp$vartype == "Nonsynonymous" & snp$maf <= .01)
nonsyn_rare_snp <- snp[nonsyn_rare,]
nonsyn_rare_geno <- geno[,nonsyn_rare+1]
geno$CMC <- apply(nonsyn_rare_geno, 1, sum)
table(geno$CMC)

# Test for association with qt adj for age sex and pop
m5 <- lm(qt ~ sex + age + pop1 + pop2 + pop3 + CMC, data = geno)
summary(m5)

# 5. Using CMC score and nonsynonymous variants with maf < .01
# test for association with qt adjusting for age and sex
m6 <- lm(qt ~ sex + age + CMC, data = geno)
summary(m6)
