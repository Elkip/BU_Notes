bp <- read.csv("/home/elkip/Datasets/bp.csv")
edu <- read.csv("/home/elkip/Datasets/education.csv")

bp <- na.omit(bp)
edu <- na.omit(edu)

# A
# 1 
bp_full <- lm(sbp1 ~ age1 + sex + bmi1 + smoke + drink + exercise, data = bp)
forward <- ~ age1 + sex + bmi1 + smoke + drink + exercise
bp_1 <- lm(sbp1 ~ 1, data=bp)
bp_2 <- lm(sbp1 ~ age1 + sex + bmi1, data=bp)
bp_3 <- lm(sbp1 ~ age1 + sex + bmi1 + smoke, data=bp)
bp_4 <- lm(sbp1 ~ age1 + sex + bmi1 + smoke + drink, data=bp)
bp_5 <- lm(sbp1 ~ age1 + sex + bmi1 + exercise, data=bp)
bp_n <- nrow(bp)
bp.forward.BIC <- step(bp_1, scope=forward, direction = "forward", k = log(bp_n))
summary(bp.forward.BIC)

extractAIC(bp_5,k=log(bp_n))

# BIC = n*log(RSS/n) + p'*log*n)
bp_n*log(162.1228/bp_n)+6*log(bp_n)
summary(bp.forward.BIC)$coefficients

# B
# 1i.
edu_reg <- lm(write ~ read + female, data = edu)
summary(edu_reg)
# 2
edu_type <- lm(write ~ read + female + schtyp, data = edu)
anova(edu_reg, edu_type)

# 3
summary(edu_reg)$coefficients
summary(edu_reg)$coefficients[1] + summary(edu_reg)$coefficients[2]*mean(edu$read)
summary(edu_reg)$coefficients[1] + summary(edu_reg)$coefficients[2]*mean(edu$read) +summary(edu_reg)$coefficients[3] 

library(lsmeans)
lsmeans(edu_reg, ~female)
