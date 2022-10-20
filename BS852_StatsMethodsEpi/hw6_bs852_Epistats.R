library(survival)
estrodat <- read.csv("/home/elkip/Datasets/estrodat.csv", header=T, na.strings=".")
estro <- subset(estrodat, !is.na(age) & !is.na(gall) & !is.na(hyper) &
                  !is.na(obesity) & !is.na(estro) & !is.na(dose) & !is.na(othdrug))

# a
# Conditional Logistic Regression
clr_crude <- clogit(case ~ estro + strata(match), data = estro)
summary(clr_crude)

clr_ajd1 <- clogit(case ~ estro + obesity + strata(match), data = estro)
summary(clr_ajd1)

# b
clr_adj2 <- clogit(case ~ estro + age + othdrug + hyper + gall + strata(match), data = estro)
summary(clr_adj2)

clr_adj3 <- clogit(case ~ estro + obesity+ age + othdrug + hyper + gall + strata(match), data = estro)
summary(clr_adj3)
