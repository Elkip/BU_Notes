library(survival)
library(tidyverse)

fram <- read.csv("/home/elkip/Datasets/framdat4.csv")
colnames(fram)
sum(fram$SEX)
colSums(is.na(fram))

# Pearson's corr:
res <- cor(fram, use="complete.obs", method = "pearson")
col_names <- colnames(res)
## List correlated variables
for (i in col_names) {
  for (j in col_names) {
      if (!is.na(res[i,j]) & i != j & abs(res[i,j]) > .65) {
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
# Menopause has no correlation with survival
remove_list <- c(remove_list, "T2D", "T2D_SURV", "MENO4")

# Drop selected columns, add BMI Category
fram2 = fram %>%
  select(-all_of(remove_list)) %>%
  mutate(OBESE = case_when(BMI4 >= 30 ~ 1, BMI4 < 30 ~ 0)) %>%
  mutate(HIGH_CHOL = case_when(CHOL4 >= 240 ~ 1, CHOL4 < 240 ~ 0))

colSums(is.na(fram2))
fram2 = na.omit(fram2)
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

fram2 = fram2 %>%
  mutate(LOG_DPF = log(DPF4)) %>%
  mutate(LOG_BMI = log(BMI4)) %>%
  mutate(LOG_CHOL = log(CHOL4))

# Variable selection 
# Forward AIC
base <- glm(CHD ~ 1, data = fram2)
final <-  ~ SEX + AGE4 + CHOL4 + DPF4 + FVC4 + BMI4 + OBESE + HTN4 + SMOKE 
final_log <- ~ SEX + AGE4 + LOG_CHOL + LOG_DPF + FVC4 + LOG_BMI + OBESE + HTN4 + SMOKE 
forward.AIC <- step(base, scope = final, direction = "forward", k = 2)
forward.AIC_log <- step(base, scope = final_log, direction = "forward", k = 2)
extractAIC(forward.AIC, k=2)
extractAIC(forward.AIC_log, k=2)
summary(forward.AIC)$coefficients
summary(forward.AIC_log)$coefficients

# Forwards BIC
forward.BIC <- step(base, scope = final, direction = "forward", k = log(nrow(fram2)), trace = F)
forward.BIC_log <- step(base, scope = final_log, direction = "forward", k = log(nrow(fram2)), trace = F)
summary(forward.BIC)$coefficients
summary(forward.BIC_log)$coefficients

# Stepwise 
step <- step(base, scope = final, direction = "both", trace = F)
step_log <- step(base, scope = final_log, direction = "both", trace = F)
summary(step)$coefficients
summary(step_log)$coefficients

# create dataset from selected variables
remove_list <- c(remove_list, "FVC4", "DPF4")
fram = fram %>%
  select(-all_of(remove_list)) %>%
  mutate(OBESE = case_when(BMI4 >= 30 ~ 1, BMI4 < 30 ~ 0)) %>%
  mutate(HIGH_CHOL = case_when(CHOL4 >= 240 ~ 1, CHOL4 < 240 ~ 0))

# recode sex
fram$SEX = recode_factor(fram$SEX, `1` = 0, `2` = 1)

# omit na's?
colSums(is.na(fram))
fram = na.omit(fram)
col_names = colnames(fram)
n_row = nrow(fram)

# Some plot to observe trends and outliers?

## Testing for interactions
interaction <- function(var1, var2, out) {
  n11 <- nrow(fram %>% filter(get(var1) == 1 & get(var2) == 1))
  p11 <- nrow(fram %>% filter(get(var1) == 1 & get(var2) == 1 & get(out) == 1)) / n11
  n01 <- nrow(fram %>% filter(get(var1) == 0 & get(var2) == 1))
  p01 <- nrow(fram %>% filter(get(var1) == 0 & get(var2) == 1 & get(out) == 1)) / n01
  n10 <- nrow(fram %>% filter(get(var1) == 1 & get(var2) == 0))
  p10 <- nrow(fram %>% filter(get(var1) == 1 & get(var2) == 0 & get(out) == 1)) / n10
  n00<- nrow(fram %>% filter(get(var1) == 0 & get(var2) == 0))
  p00 <-  nrow(fram %>% filter(get(var1) == 0 & get(var2) == 0 & get(out) == 1)) / n00
    
  
  t <- p11 - p01 - p10 + p00
  var_t = ((p00*(1-p00))/n00) + ((p01*(1-p01))/n01) +
    ((p10*(1-p10))/n10) + ((p11*(1-p11))/n11)
  z = t / sqrt(var_t)
  rr11 <- p11 / p00
  rr10 <- p10 / p00
  rr01 <- p01 / p00
  rr_ratio_add <- rr11 - rr10 - rr01 + 1
  rr_ratio_mult <- rr11 /  (rr10*rr01)
  
  print(paste("T:", t))
  print(paste("Z:", z))
  print(paste("Excess Risk due to interaction:", rr_ratio_add))
  print(paste("Multiplicative interaction:", rr_ratio_mult))
}

interaction("HTN4", "SMOKE", "CHD")
interaction("HTN4", "OBESE", "CHD")
interaction("SMOKE", "OBESE", "CHD")
interaction("SEX", "OBESE", "CHD")
interaction("SEX", "SMOKE", "CHD")
interaction("SEX", "HTN4", "CHD")
interaction("HIGH_CHOL", "HTN4", "CHD")
interaction("HIGH_CHOL", "SEX", "CHD")
interaction("HIGH_CHOL", "OBESE", "CHD")
interaction("HIGH_CHOL", "SMOKE", "CHD")

## 1) Is obesity associated with CHD?

# Kaplan-Meier Plot
chd.km <- survfit(Surv(CHD_SURV, CHD) ~ OBESE, data=fram)
summary(chd.km)
plot(chd.km, col=c(1,2), lwd=2, ylim=c(0,1),
     xlab="Time (Years)", ylab="CHD free survival", cex.axis=1.5, cex.lab=1.5)
legend(x=1, y=0.40, legend=c("Low BMI","High BMI"),
       col=c(1,2), lwd =2, cex=1.2)

# Log-Rank Test
survdiff(Surv(CHD_SURV, CHD) ~ OBESE, data=fram)

# Cox Proportional Models 
fit.cox <- coxph(Surv(CHD_SURV, CHD) ~ OBESE + AGE4 + CHOL4 + HTN4 + SEX , data=fram)
summary(fit.cox)
fit.cox2 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX, data=fram)
summary(fit.cox2)

# Should I add SEX*HTN4 or SEX*OBESE or SEX*SMOKE or HTN*OBESE or CHOL4*HTN4?
fit.coxx1 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SEX*HTN4, data=fram)
summary(fit.coxx1)
fit.coxx2 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SEX*BMI4, data=fram)
summary(fit.coxx2)
fit.coxx3 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SEX*SMOKE, data=fram)
summary(fit.coxx3)
fit.coxx4 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + HTN4*BMI4, data=fram)
summary(fit.coxx4)
fit.coxx5 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + CHOL4*HTN4, data=fram)
summary(fit.coxx5)

# Test proportional hazard
scho <- cox.zph(fit.coxx5)
scho
par(mfrow = c(4,2))
plot(scho)

## 2) What factors can confound the association between obesity and CHD?
# Difference in crude and adjusted odds ratios
crude.cox <- coxph(Surv(CHD_SURV, CHD) ~ OBESE, data=fram)
summary(crude.cox)
abs(1.703 - 1.398) / 1.703

## 3) Is the association between obesity and CHD the same in males and females?
# Fit stratified model using strata() option
cox.strat <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + strata(SEX), 
               data=fram)
summary(cox.strat)
cox.zph(cox.strat)

## 4) Is obesity associated with mortality?

## 5) What factors can confound the association between obesity and mortality?

## 6) Is the association between obesity and mortality the same in males and females?

## 7) Is it more informative to use BMI as a continuous or dichotomous variable?