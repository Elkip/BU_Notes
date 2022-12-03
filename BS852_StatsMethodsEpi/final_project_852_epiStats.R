library(survival)
library(tidyverse)

fram <- read.csv("/home/elkip/Datasets/framdat4.csv")
colnames(fram)
sum(fram$SEX)
colSums(is.na(fram))


## Clean Missing Data
# Set MENO4 to 0 for men
fram = fram %>% 
  mutate(MENO4 = ifelse((is.na(MENO4) & SEX == 1), 0, MENO4)) 
# omit remaining na's & add BMI classification
fram = na.omit(fram) %>%
  mutate(BMI_cat = ifelse(BMI4 >= 30, 1, 0))

sum(fram$SEX)
colSums(is.na(fram))

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