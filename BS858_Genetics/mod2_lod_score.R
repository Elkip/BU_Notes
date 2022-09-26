##Linkage Analysis with Sibling Pairs

sibpair <- read.csv("/home/elkip/Datasets/sibling_data.csv")

print(sibpair[1:10,])

n <- table(sibpair$ibd)

print(n)
print(n/sum(n))

table(sibpair$kid1disease,sibpair$kid2disease)

asp <- sibpair[sibpair$kid1disease==2 & sibpair$kid2disease==2,]
print(asp[1:10,])

ibd.count <- table(asp$ibd)
print(ibd.count)
print(ibd.count/sum(ibd.count))

goodness.test <- chisq.test(ibd.count,p=c(0.25,0.5,0.25))
print(goodness.test)
print(goodness.test$p.value/2)

N <- nrow(asp)
n2 <- sum(asp$ibd==2)
n1 <- sum(asp$ibd==1)
T2 <- sqrt(2*N)*(n1/N + 2*n2/N - 1)
print(T2)
pval2 <- pt(T2,N-1,lower=F)
print(pval2)


#Haseman_Elson Approach

sibpair$Yd <- (sibpair$kid1trait - sibpair$kid2trait)^2
sibpair$pi <- sibpair$ibd/2

print(sibpair[1:10,])

original.he <- lm(Yd ~ pi,data=sibpair)
print(summary(original.he))

tvalue <- summary(original.he)$coef[2,3]
onesidedp <- pt(tvalue,original.he$df)
print(onesidedp)

lod <- tvalue^2/(2*log(10))
print(lod)

popmean <- mean(c(sibpair$kid1trait,sibpair$kid2trait))

sibpair$Yp <- (sibpair$kid1trait - popmean)*(sibpair$kid2trait-popmean)

revised.he <- lm(Yp ~ pi,data=sibpair)

print(summary(revised.he))

revised.tvalue <- summary(revised.he)$coef[2,3]
revised.onesidedp <- pt(revised.tvalue,revised.he$df,lower=F)
print(revised.onesidedp)

revised.lod <- revised.tvalue^2/(2*log(10))
print(revised.lod)
