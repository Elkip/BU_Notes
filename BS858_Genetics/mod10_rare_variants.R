

snpinfo<-read.csv("snp_info.csv",header=T,as.is=T)
geno<-read.csv("pheno_geno.csv",header=T,as.is=T)

##calculate MAF
## The snps in geno are in the same order as in snpinfo, but in 
## columns 2-36 instead of 1-35

maf<-apply(geno[,2:36],2,function(i)sum(i)/(2*length(i)))

snpinfo$maf <- maf

snpinfo
## 25 rare variants maf<=0.01
## 10 common variants  
## 20 nonsynonymous variants

rare<-which(snpinfo$maf<=0.01)
common<-which(snpinfo$maf>0.01)

rare
##snps 1-17 and 21-28 are maf<=0.01. 

## The snps in geno are in the same order as in snpinfo, but in 
## columns 2-36 instead of 1-35
## So, we need to add 1 to the 
## locations in "rare" to get the right column numbers

###  CMC with maf<=0.01 SNPs  ###
geno$CMC<-apply(geno[,rare+1],1,sum)

### CAST for maf<=0.01 SNPs ###
## CAST is just an indicator of whether or not the individual 
## carries at least 1 rare allele:
geno$CAST<-ifelse(geno$CMC>0,1,0)

## what do the new CMC and CAST variables look like?
table(geno$CMC)
table(geno$CAST)

###  Weighted  -- Madsen Browning weights, but using full sample
### maf weights rather than unaffected only maf, and using ALL 
### snps, not just the maf<=0.01 SNPs:

dim(geno)

##note:  no missing data, so sample size is 697 for all SNPs
weights<-1/sqrt(697*snpinfo$maf*(1-snpinfo$maf))

weights

##xxx is a matrix of weighted genotypes:
xxx<-sapply(1:35,function(i)geno[,i+1]*weights[i])
dim(xxx)

geno$MB<-apply(xxx,1,sum)

## Analyses using case status (logistic regression)

m1<-glm(AFFECTED~SEX+AGE+SMOKE+CAST,binomial("logit"),data=geno)
m2<-glm(AFFECTED~SEX+AGE+SMOKE+CMC,binomial("logit"),data=geno)
m3<-glm(AFFECTED~SEX+AGE+SMOKE+MB,binomial("logit"),data=geno)

summary(m1)
summary(m2)
summary(m3)
