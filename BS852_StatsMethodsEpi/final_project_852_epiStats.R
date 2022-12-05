library(survival)
library(tidyverse)

fram <- read.csv("/home/elkip/Datasets/framdat4.csv")
colnames(fram)
sum(fram$SEX)
colSums(is.na(fram))

## Clean Missing Data
# add BMI classification
fram = fram %>%
  mutate(BMI_cat = case_when(BMI4 >= 30 ~ 1, BMI4 < 30 ~ 0))

sum(fram$SEX)
colSums(is.na(fram))

## Which variables are correlated, can we drop 1
# Pearson's corr for :
#BMI vs wgt
# Blood pressures + hypertension
# smoking and cigs
# diabetes and BMI
# pulmonary function and smoking
# obesity cat and weight

# Figure out if any parameters should be logistic

# create new dataset from selected variables and omit na's

# Variable selection - stepwise, AIC, BIC

# Two prop t test to determine difference in prop of CDH in men 
# and women vs smokers and nonsmokers, obese and non obese, cholestrol
# any significant interaction requires interaction term

# Outliers

## 1) Is obesity associated with CHD?

# Kaplan-Meier Plot
chd.km <- survfit(Surv(CHD_SURV, CHD) ~ BMI_cat, data=fram)
summary(chd.km)
plot(chd.km, col=c(1,2), lwd=2, ylim=c(0,1),
     xlab="Time (Years)", ylab="CHD free survival", cex.axis=1.5, cex.lab=1.5)
legend(x=1, y=0.40, legend=c("Low BMI","High BMI"),
       col=c(1,2), lwd =2, cex=1.2)


## 2) What factors can confound the association between obesity and CHD?

## 3) Is the association between obesity and CHD the same in males and females?

## 4) Is obesity associated with mortality?

## 5) What factors can confound the association between obesity and mortality?

## 6) Is the association between obesity and mortality the same in males and females?

## 7) Is it more informative to use BMI as a continuous or dichotomous variable?