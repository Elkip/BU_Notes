#Change directory first

famdata <- read.csv("/home/elkip/Datasets/family_data.csv",as.is=T,na=".")

print(famdata[1:20,])
print(summary(famdata))

Nind <- nrow(famdata)
print("Number of indivdiduals")
print(Nind)

Nfam <- length(table(famdata$famid))
print("Number of families")
print(Nfam)

Nchildren <- sum(famdata$mo !=0)
print("Number of children")
print(Nchildren)

N.nonmiss.pheno <- sum(famdata$aff!=0)
print("Number with non missing phenotype")
print(N.nonmiss.pheno)

prop.miss.pheno <- sum(famdata$aff==0)/Nind
print("Proportion with missing phenotype")
print(prop.miss.pheno)

N.nonmiss.pheno.kid <- sum(famdata$aff!=0 & famdata$mo!=0)
print("Number of children with non missing phenotype")
print(N.nonmiss.pheno.kid)

prop.miss.pheno.kid <- sum(famdata$aff==0 & famdata$mo!=0)/Nchildren
print("Proportion of children with missing phenotype")
print(prop.miss.pheno.kid)

N.DNA <- sum(!is.na(famdata$DNAdate))
print("Number with DNA")
print(N.DNA)

prop.noDNA <- sum(is.na(famdata$DNAdate))/Nind
print("Proportion without DNA")
print(prop.noDNA)

prop.kid.noDNA <- sum(is.na(famdata$DNAdate) & famdata$mo!=0)/sum(famdata$mo!=0)
print("Proportion children without DNA")
print(prop.kid.noDNA)

N.both.DNA.pheno <- sum(!is.na(famdata$DNAdate) & famdata$aff!=0)
print("Number with both DNA and phenotype")
print(N.both.DNA.pheno)

Nsibs <- tapply(famdata$mo!=0,famdata$famid,sum)
mean.sibship <- mean(Nsibs)
print("Average sibship size")
print(mean.sibship)

Nsibs.DNA.pheno <- tapply(famdata$mo!=0 & famdata$aff!=0 & !is.na(famdata$DNAdate),
famdata$famid,sum)
mean.sibship.DNA.pheno <- mean(Nsibs.DNA.pheno)
print("Average sibship size (with DNA and phenotype available)")
print(mean.sibship.DNA.pheno)

Nkid.aff <- tapply(famdata$mo!=0 & famdata$aff==2,famdata$famid,sum)
print("Number of affected children per families")
print(table(Nkid.aff))
