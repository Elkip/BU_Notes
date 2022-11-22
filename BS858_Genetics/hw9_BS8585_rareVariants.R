geno <- read.csv("/home/elkip/Datasets/pheno_geno_hw9_2022.csv")
snp <- read.csv("/home/elkip/Datasets/snp_info_hw9_2022.csv")

maf <- apply(geno[,2:25], 2, function(i) sum(i)/(2*length(i)))
maf

snp$maf <- maf
snp

rare <- which(snp$maf <= .01)
common <- which(snp$maf > .01)

# CMC & CAST with maf < .01 SNPs
geno$CMC <- apply(geno[,rare+1], 1, sum)
geno$CAST <- ifelse(geno$CMC>0,1,0)
table(geno$CMC)
table(geno$CAST)

# MB Weights
dim(geno)
weights <- 1/sqrt(1000*snp$maf*(1-snp$maf))
weights
geno_weighted <- sapply(1:24, function(i) geno[,i+1]*weights[i])
dim(geno_weighted)
geno$MB <- apply(geno_weighted, 1, sum)
summary(geno$MB)
sd(geno$MB)

# Logistic regression analysis
m1 <- glm(affected ~ sex + age + pop1 + pop2 + pop3 + CAST, family = binomial("logit"), data = geno)
m2 <- glm(affected ~ sex + age + pop1 + pop2 + pop3 + CMC, family = binomial("logit"), data = geno)
m3 <- glm(affected ~ sex + age + pop1 + pop2 + pop3 + MB, family = binomial("logit"), data = geno)
summary(m1)
summary(m2)
summary(m3)
