library("xlsx") # Requires "default-jre" on ubuntu 22.04
library("kinship2")

# Q2
fam <- read.xlsx("/home/elkip/Datasets/HW2_family.xlsx", 1, header = T)
attach(fam)
pedAll <- pedigree(id = id, dadid = fa, momid = mo, sex = sex, affected = disease, 
                   famid = famid, missid = 0)
ped1 <- pedAll["1"] # Select family id 1
print(ped1)
plot.pedigree(ped1)

# Q2 Part d/e
N = 8
R = 1
theta = .1
lod <- log((theta^R * (1 - theta)^N) / .5^(R+N))
lod
detach(fam)

# Q3
sibs <- read.csv("/home/elkip/Datasets/HW2_sibpairs.csv")
aff <- sibs[sibs$disease1==2 & sibs$disease2==2,]
# Q3 part a
n_aff <- table(aff$ibd)
print(n_aff)
print(n_aff/sum(n_aff))

# Q3 part b
gof <- chisq.test(n_aff, p = c(.25, .5, .25))
gof
print(gof$p.value/2)

# Q4
# Q4 part a
sibs$Yd <- (sibs$resid1 - sibs$resid2)^2
sibs$pi <- sibs$ibd/2
original.he <- lm(Yd ~ pi,data=sibs)
print(summary(original.he))
tvalue <- summary(original.he)$coef[2,3]
onesidedp <- pt(tvalue,original.he$df, lower.tail = T)
print(onesidedp)
tvalue^2 / (2*log(10))

# Q4 part b
popmean <- mean(c(sibs$resid1, sibs$resid2))
sibs$Yp <- (sibs$resid1 - popmean) * (sibs$resid2 - popmean)
sibs$pi <- sibs$ibd/2
revised.he <- lm(Yp ~ pi, data = sibs)
summary(revised.he)
revised.tvalue <- summary(revised.he)$coef[2,3]
revised.tvalue
revised.onesidedp <- pt(revised.tvalue,revised.he$df,lower=F)
revised.onesidedp
revised.tvalue^2 / (2*log(10))
