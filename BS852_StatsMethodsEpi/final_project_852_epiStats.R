library(survival)
library(tidyverse)

fram <- read.csv("/home/elkip/Datasets/framdat4.csv")
colnames(fram)
sum(fram$SEX)
colSums(is.na(fram))

sum(fram$SEX)
colSums(is.na(fram))

# Pearson's corr:
res <- cor(fram, use="complete.obs", method = "pearson")
col_names <- colnames(res)
## List correlated variables
for (i in col_names) {
  for (j in col_names) {
      if (!is.na(res[i,j]) & i != j & abs(res[i,j]) > .5) {
        print(paste("Cor between",i,"and",j,"is",res[i,j]))
      }
    }
}

# Drop 1 of the following correlated pairs:
# BMI vs WGHT
# DPF vs SPF blood pressure
# smoking and cigs
# pulmonary function and smoking
remove_list <- c("WGT4", "SPF4", "CIGS4")
# Drop the uninformative variables:
# Diabetes /  Diabetic survival is not of interest
# Menstruation logically has no bearing on survival
remove_list <- c(remove_list, "T2D", "T2D_SURV", "MENO4")

# Drop selected columns, add BMI Category
fram2 = fram %>%
  select(-remove_list) %>%
  mutate(BMI_CAT = case_when(BMI4 >= 30 ~ 1, BMI4 < 30 ~ 0)) %>%
  na.omit(fram)

col_names = colnames(fram2)
n_rows = nrow(fram2)

# Figure out if any parameters should be log
par(mfrow=c(2,2))
for (c in col_names) {
  # Only non-probability continuous parameters
  if (length(unique(fram2[,c])) > 2 & max(fram2[,c], na.rm = T) > 1) {
    hist(fram2[,c], xlab = c, main = "Regular")
    hist(log(fram2[,c]), xlab = c, main = "Log")
    boxplot(fram2[,c], horizontal = T)
    boxplot(log(fram2[,c]), horizontal = T)
  }
}

# Variable selection 
# Forward AIC
base <- glm(CHD ~ 1, data = fram2)
final <-  ~ SEX + AGE4 + CHOL4 + DPF4 + FVC4 + BMI4 +BMI_CAT + HTN4 + SMOKE 
forward.AIC <- step(base, scope = final, direction = "forward", k = 2)
extractAIC(forward, k=2)
summary(forward)$coefficients

# Forwards BIC
forward.BIC <- step(base, scope = final, direction = "forward", k = log(n_rows), trace = F)
summary(forward.BIC)$coefficients

# Stepwise 
step <- step(base, scope = final, direction = "both", trace = F)
summary(step)$coefficients

# create dataset from selected variables and omit na's
remove_list <- c(remove_list, "FVC4", "BMI_CAT", "DPF4")
fram = fram %>%
  select(-remove_list) %>%
  mutate(BMI_CAT = case_when(BMI4 >= 30 ~ 1, BMI4 < 30 ~ 0)) %>%
  na.omit(fram)

col_names = colnames(fram)
n_row = nrow(fram)

# Outliers

# Two prop t test to determine difference in prop of CDH in men 
# and women vs smokers and nonsmokers, obese and non obese, cholestrol
# any significant interaction requires interaction term


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