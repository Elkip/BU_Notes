#Change directory first

genodata <- read.csv("/home/elkip/Datasets/class1_genotypes.csv",as.is=T)

print(genodata[1:2,])
print(summary(genodata))

snp.alleles <- c(genodata$SNPal1,genodata$SNPal2)
str.alleles <- c(genodata$STRal1,genodata$STRal2)

print(table(snp.alleles))
print(table(str.alleles))

str.genotypes<-ifelse(genodata$STRal1<genodata$STRal2,
      paste(genodata$STRal1,genodata$STRal2,sep="/"),
      paste(genodata$STRal2,genodata$STRal1,sep="/"))

snp.genotypes<-ifelse(genodata$SNPal1<genodata$SNPal2,
      paste(genodata$SNPal1,genodata$SNPal2,sep="/"),
      paste(genodata$SNPal2,genodata$SNPal1,sep="/"))

print(table(snp.genotypes))
print(table(str.genotypes))


