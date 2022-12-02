### ANOVA
###	smoking 

# reading data and data exploration
data.1 <- read.csv("smoking.csv", header=T)
head(data.1, n=3)
summary(data.1)
plot(data.1[,2], data.1[,1])
hist(data.1[,1])

# Hand cacluation
g.mean <- mean(data.1$Serum.T)	# overall mean
SST <- sum((data.1$Serum.T-g.mean)^2)	# total Sum of square
   
means <- tapply(data.1$Serum.T,data.1$Smoking.hx,FUN=mean)	# group means
means

# within Sum of square
Tlev <- data.1$Serum.T
dim(Tlev) <- c(10,4)
SSW <- cbind( (Tlev[,1]-means[4])^2,(Tlev[,2]-means[1])^2,
                (Tlev[,3]-means[3])^2,(Tlev[,4]-means[2])^2)
sum(SSW)

# between Sum of square
SSB <- 10*( sum((means -g.mean)^2))
sum(SSB)

### using aov() function
mod.1 <- aov(data.1$Serum.T~data.1$Smoking.hx)
summary(mod.1)
fitted.values(mod.1)
tapply(data.1$Serum.T,data.1$Smoking.hx,FUN=mean)
TukeyHSD(mod.1)

## ANOVA as regression
mod.1 <- lm(data.1$Serum.T~data.1$Smoking.hx)
summary(mod.1)
fitted.values(mod.1)
anova(mod.1)


### change reference group (relevel)
levels(data.1$Smoking.hx)
data.1$Smoking.hx <- relevel(data.1$Smoking.hx, ref="Nonsmokers")
mod.1 <- lm(data.1$Serum.T~data.1$Smoking.hx)
summary(mod.1)
fitted.values(mod.1)

####### Two-way ANOVA
##### drug dataset
drug <- read.csv("anova.csv", header=T)
drug

hbf <- c(t(drug[,3:5]))
SNP <- c(rep("No",12), rep("Yes", 12))
Drug <- c(rep(0,3), rep(10,3), rep(20,3), rep(30,3),
               rep(0,3), rep(10,3), rep(20,3), rep(30,3))
data.drug <- data.frame(hbf,SNP, Drug)
head(data.drug)

### summaries
overall.mean <- mean(hbf); overall.mean
drug.means <- tapply(hbf, Drug, mean); drug.means
snps.means <- tapply(hbf, SNP, mean); snps.means
cell.means <- tapply(hbf, interaction(Drug,SNP), mean); cell.means

dim(cell.means) <- c(4,2); cell.means
cell.means <- data.frame(cell.means)
row.names(cell.means) <- levels(factor(Drug))
names(cell.means) <- levels(factor(SNP))
cell.means 

## Visualization
#install.packages("gplots")
library(gplots)
plotmeans(hbf~Drug, data=data.drug,xlab="Drug", 
		ylab="HbF",  main="Mean Plot\n with 95% CI")
plotmeans(hbf~SNP, data=data.drug,xlab="SNP", 
		ylab="HbF",  main="Mean Plot\n with 95% CI")
plotmeans(hbf~interaction(Drug,SNP), data=data.drug,
		xlab="SNP",  connect=list(1:4,5:8),ylab="HbF", 
          	main="Interaction Plot\nwith 95% CI")

interaction.plot(factor(Drug), factor(SNP), hbf, type="b",  
		xlab="Drug", ylab="hbf", main="Interaction Plot")


### two-way anova with balanced design
mod <- aov(hbf~as.factor(Drug)*SNP, data=data.drug)
summary(mod)
table(mod$fitted.values)

# TukeyHSD(mod)
mod <- lm(hbf~as.factor(Drug)*SNP, data=data.drug)
summary(mod)
anova(mod)
table(mod$fitted.values)

##### Exercise with balance design

mod.a <- aov(hbf~as.factor(Drug)*SNP, data=data.drug)
summary(mod.a)		# anova(mod.a)
mod.lma <- lm(hbf~as.factor(Drug)*SNP, data=data.drug)
anova(mod.lma)

mod.b <- aov(hbf~SNP*as.factor(Drug), data=data.drug)
summary(mod.b)		# anova(mod.b)
mod.lmb <- lm(hbf~SNP*as.factor(Drug), data=data.drug)
anova(mod.lmb)


##### Exercise with unbalance design
data.drug.1 <- read.csv("data.drug.1.csv", header=T)
mod.1a <- aov(hbf~as.factor(Drug)*SNP, data=data.drug.1)
summary(mod.1a)
mod.1lma <- lm(hbf~as.factor(Drug)*SNP, data=data.drug.1)
anova(mod.1lma)


mod.1b <- aov(hbf~SNP*as.factor(Drug), data=data.drug.1)
summary(mod.1b)
mod.1lmb <- lm(hbf~SNP*as.factor(Drug), data=data.drug.1)
anova(mod.1lmb)
