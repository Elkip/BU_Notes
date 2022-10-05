depress <- read.csv("/home/elkip/Datasets/depress2.csv")

# 1.1
head(depress)
by(depress, depress$publicassist, summary)

sum(is.na(depress))
sd(depress$age)
sd(depress$cesd)
hist(depress$age)
hist(depress$cesd)

assist <- depress[which(depress$publicassist == 1),]
noassist <- depress[which(depress$publicassist == 0),]

summary(assist)
sd(assist$age)
sd(assist$cesd)
hist(assist$age)
hist(assist$cesd)

summary(noassist)
sd(noassist$age)
sd(noassist$cesd)
hist(noassist$age)
hist(noassist$cesd)

# 1.2
par(mfrow=c(1,3))
boxplot(cesd ~ publicassist, depress)
plot(cesd ~ age, pch=as.character(publicassist), noassist)
plot(cesd ~ age, pch=as.character(publicassist), assist)

# 1.3
t.test(assist$cesd, noassist$cesd)

# 1.4a
reg <- lm(cesd ~ publicassist + age, data = depress)
summary(reg)
head(model.matrix(reg))


# 1.4b


# 1.4c


# 1.4d

# 5
summary(reg)$coefficients
summary(reg)$coefficients[1] + summary(reg)$coefficients[3]*mean(depress$age)
summary(reg)$coefficients[1] + summary(reg)$coefficients[3]*mean(depress$age) +summary(reg)$coefficients[2] 

library(lsmeans)
lsmeans(reg, ~publicassist)


# 1.6a
reg_x <- lm(cesd ~ publicassist + age + publicassist:age, data = depress)
summary(reg_x)

# 1.6b
