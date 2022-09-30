sex <- read.csv("/home/elkip/Datasets/sexab.csv")

summary(sex)
sex_ab <- sex[which(sex$csa == "Abused"),]
sex_na <- sex[which(sex$csa == "NotAbused"),]

lm(ptsd ~ cpa, data=sex_ab)
lm(ptsd ~ cpa, data=sex_na)

t.test(sex$ptsd ~ sex$csa, data = sex)

gl <- lm(ptsd ~ cpa + csa + cpa:csa, data = sex)
summary(gl)

sex_lm <- lm(ptsd ~ ., data =sex)
summary(sex_lm)

plot(ptsd~cpa, pch=as.character(csa), data=sex)
abline(gl$coefficients[1], gl$coefficients[2])
abline(gl$coefficients[1] + gl$coefficients[3],
       gl$coefficients[2])
