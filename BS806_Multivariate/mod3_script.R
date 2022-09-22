### class 3
install.packages("alr4")
library(alr4)

### Fuel consumption dataset
?fuel2001

fuel2001$Dlic <- 1000*fuel2001$Drivers/fuel2001$Pop
fuel2001$Fuel <- 1000*fuel2001$FuelC/fuel2001$Pop
fuel2001$Incomes <- fuel2001$Income/1000
fuel1 <- lm(formula = Fuel ~ Tax + Dlic + Incomes + log(Miles), data = fuel2001)
plot(Effect("Tax", fuel1))
summary(fuel1)

### Berkeley guidance data
### Data from the Berkeley guidance study 
### of children born in 1928-29 in Berkeley, CA
?BGSgirls
BGSgirls$DW9 <- BGSgirls$WT9 - BGSgirls$WT2
BGSgirls$DW18 <- BGSgirls$WT18 - BGSgirls$WT9
BGSgirls$DW218 <- BGSgirls$WT18 - BGSgirls$WT2

pairs(BGSgirls[,c("BMI18", "WT2", "WT9", "WT18")])

m1 <- lm(BMI18 ~ WT2 + WT9 + WT18 , BGSgirls)
m2 <- lm(BMI18 ~ WT2 + DW9 + DW18 , BGSgirls)
m3 <- lm(BMI18 ~ WT2 + WT9 + WT18 + DW9 + DW18, BGSgirls)


summary(m1)$coefficients
summary(m2)$coefficients
summary(m3)$coefficients


summary(m1)$r.squared
summary(m2)$r.squared
summary(m3)$r.squared
summary(m1)


#### Inference with Fuel consumption data
m0 <- lm(Fuel ~ 1, fuel2001)
m1 <- update(m0, ~ Tax)
m2 <- update(m1, ~ . + Dlic)
m3 <- update(m2, ~ . + Incomes + log(Miles))
m0
m3

## the overall test H0: m0 v.s. H1: m3
## calculate it directly without using 
## provided results in R
SYY <- sum((fuel2001$Fuel-mean(fuel2001$Fuel))^2) ; SYY
RSS <- deviance(m3); RSS
df.residual(m3)
num <- (SYY - RSS)/(df.residual(m0)-df.residual(m3))
den <- (RSS/df.residual(m3))
fstat <- num/den; fstat

1-pf(fstat, df.residual(m0)-df.residual(m3), df.residual(m3))

## approach 2
summary(m3)

## approach 3: using anova() function
anova(m0, m3)

###### in m3 model, test whether log(Miles)'s beta =0
m4 <- lm(Fuel ~ Tax + Dlic + Incomes, fuel2001)

##  Use the summary directly
summary(m3)

# anova approach
anova(m3, m4)

# calculate it directly without using provided results in R
RSS4 <- deviance(m4); RSS4
RSS <- deviance(m3); RSS
df.residual(m3)
num <- (RSS4 - RSS)/(df.residual(m4)-df.residual(m3))
den <- (RSS/df.residual(m3))
fstat <- num/den; fstat

1-pf(fstat, df.residual(m4)-df.residual(m3), df.residual(m3))


#### testing the betas for Tax and Incomes are the same 
#### in full model
m5 <- lm(Fuel ~ I(Tax+Incomes) + Dlic + log(Miles), fuel2001)


# anova approach
anova(m3, m5)

# calculate it directly without using provided results in R
RSS5 <- deviance(m5); RSS5
RSS <- deviance(m3); RSS
df.residual(m3)
num <- (RSS5 - RSS)/(df.residual(m5)-df.residual(m3))
den <- (RSS/df.residual(m3))
fstat <- num/den; fstat

1-pf(fstat, df.residual(m5)-df.residual(m3), df.residual(m3))


###
fuel2001$TI <- fuel2001$Tax + fuel2001$Incomes
m50 <- lm(Fuel ~ TI + Dlic + log(Miles), fuel2001)
summary(m50)
summary(m5)
