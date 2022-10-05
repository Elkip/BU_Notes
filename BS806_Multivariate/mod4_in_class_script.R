insurance <- read.csv("insurance.csv")
head(insurance)
by(insurance, insurance$Type, summary)

par(mfrow=c(1,2))
plot(Number ~ Type, insurance)
plot(Number ~ Size, pch=as.character(Type), insurance)

t.test(insurance$Number[1:10], insurance$Number[11:20])
g <- lm(Number ~ Size + Type, insurance)
summary(g)

(model.matrix(g))
head(model.matrix(g))

#### model with interaction
g1 <- lm(Number ~ Size + Type + Size:Type, insurance)
summary(g1)

plot(Number ~ Size, pch=as.character(Type), insurance)
abline(g1$coefficients[1], g1$coefficients[2])
abline(g1$coefficients[1]+g1$coefficients[3], 
	g1$coefficients[2] + g1$coefficients[4], lty=2)

## plot without interaction
summary(g)
plot(Number ~ Size, pch=as.character(Type), insurance)
abline(g$coefficients[1], g$coefficients[2])
abline(g$coefficients[1]+g$coefficients[3], g$coefficients[2], lty=2)

## confidence interval
confint(g)[3,]
8.055469 + qt(0.975, 17)*1.459106
8.055469 - qt(0.975, 17)*1.459106

t.test(insurance$Number[1:10], insurance$Number[11:20])
tapply(insurance$Number, (insurance$Type), mean)
tapply(insurance$Size, (insurance$Type), mean)
mean(insurance$Size)


#install.packages("lsmeans")
library(lsmeans)
lsmeans(g, ~Type)

summary(g)$coefficients
summary(g)$coefficients[1] + 
	summary(g)$coefficients[2]*mean(insurance$Size)
summary(g)$coefficients[1] + 
	summary(g)$coefficients[2]*mean(insurance$Size)+ 
	summary(g)$coefficients[3]

# ANCOVA
fev <- read.table("fev.dat")
colnames(fev) <- c("age", "fev", "ht", "sex", "smoke")
# compare the fev between smoker and non-smoker
# raw data compaison without accounting for covariates
t.test(fev$fev~ fev$smoke)
tapply(fev$fev, fev$smoke, mean)
tapply(fev$age, fev$smoke, mean)
# ancova
r.fev <- lm(fev ~ smoke + age, data = fev)
summary(r.fev)

#install.packages("lsmeans")
library(lsmeans)
lsmeans(r.fev, ~smoke)

# manually calculate it
summary(r.fev)$coefficients
# lsmeans for nonsmoker: smoke=0
summary(r.fev)$coefficients[1] + 
  summary(r.fev)$coefficients[3]*mean(fev$age)

# lsmeans for smoker: smoker=1
summary(r.fev)$coefficients[1] + 
  summary(r.fev)$coefficients[3]*mean(fev$age)+ 
  summary(r.fev)$coefficients[2]
