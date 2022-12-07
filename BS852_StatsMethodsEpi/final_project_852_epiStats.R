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
remove <- c("WGT4", "SPF4", "CIGS4")
# Drop the uninformative variables:
# Diabetes /  Diabetic survival is not of interest
# Menopause has no correlation with survival
remove <- c(remove, "T2D", "T2D_SURV", "MENO4")

# Drop selected columns, add BMI Category
fram2 = fram %>%
  select(-all_of(remove)) %>%
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

### CHD Analysis
# Variable selection 
# Forward AIC
base_chd <- coxph(Surv(CHD_SURV, CHD) ~ 1, data = fram2)
final <-  ~ BMI4 + SEX + AGE4 + CHOL4 + DPF4 + FVC4 + OBESE + HTN4 + SMOKE 
final_log <- ~  LOG_BMI + SEX + AGE4 + LOG_CHOL + LOG_DPF + FVC4 +  OBESE + HTN4 + SMOKE 
forward.AIC_chd <- step(base_chd, scope = final, direction = "forward", k = 2)
forward.AIC_log_chd <- step(base_chd, scope = final_log, direction = "forward", k = 2)
extractAIC(forward.AIC_chd, k=2)
extractAIC(forward.AIC_log_chd, k=2)
summary(forward.AIC_chd)$coefficients
summary(forward.AIC_log_chd)$coefficients

# Forwards BIC
forward.BIC_chd <- step(base_chd, scope = final, direction = "forward", k = log(nrow(fram2)), trace = F)
forward.BIC_log_chd <- step(base_chd, scope = final_log, direction = "forward", k = log(nrow(fram2)), trace = F)
summary(forward.BIC_chd)$coefficients
summary(forward.BIC_log_chd)$coefficients

# Stepwise 
step_chd <- step(base_chd, scope = final, direction = "both", trace = F)
step_log_chd <- step(base_chd, scope = final_log, direction = "both", trace = F)
summary(step_chd)$coefficients
summary(step_log_chd)$coefficients

# create dataset from selected variables
remove_chd <- c(remove, "FVC4", "DPF4")
fram_chd = fram %>%
  select(-all_of(remove_chd)) %>%
  mutate(OBESE = case_when(BMI4 >= 30 ~ 1, BMI4 < 30 ~ 0)) %>%
  mutate(HIGH_CHOL = case_when(CHOL4 >= 240 ~ 1, CHOL4 < 240 ~ 0))

# recode sex
fram_chd$SEX = recode_factor(fram_chd$SEX, `1` = 0, `2` = 1)

# omit na's?
colSums(is.na(fram_chd))
fram_chd = na.omit(fram_chd)
col_names = colnames(fram_chd)
n_row = nrow(fram_chd)

## Testing for interactions
interaction <- function(var1, var2, out) {
  n11 <- nrow(fram_chd %>% filter(get(var1) == 1 & get(var2) == 1))
  p11 <- nrow(fram_chd %>% filter(get(var1) == 1 & get(var2) == 1 & get(out) == 1)) / n11
  n01 <- nrow(fram_chd %>% filter(get(var1) == 0 & get(var2) == 1))
  p01 <- nrow(fram_chd %>% filter(get(var1) == 0 & get(var2) == 1 & get(out) == 1)) / n01
  n10 <- nrow(fram_chd %>% filter(get(var1) == 1 & get(var2) == 0))
  p10 <- nrow(fram_chd %>% filter(get(var1) == 1 & get(var2) == 0 & get(out) == 1)) / n10
  n00<- nrow(fram_chd %>% filter(get(var1) == 0 & get(var2) == 0))
  p00 <-  nrow(fram_chd %>% filter(get(var1) == 0 & get(var2) == 0 & get(out) == 1)) / n00
    
  
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
par(mfrow=c(1,1))
chd.km <- survfit(Surv(CHD_SURV, CHD) ~ OBESE, data=fram_chd)
summary(chd.km)
plot(chd.km, col=c(1,2), lwd=2, ylim=c(0,1),
     xlab="Time (Years)", ylab="CHD free survival", cex.axis=1.5, cex.lab=1.5)
legend(x=1, y=0.40, legend=c("Low BMI","High BMI"),
       col=c(1,2), lwd =2, cex=1.2)

# Log-Rank Test
survdiff(Surv(CHD_SURV, CHD) ~ OBESE, data=fram_chd)

# Cox Proportional Models 
chd_fit.cox <- coxph(Surv(CHD_SURV, CHD) ~ OBESE + AGE4 + CHOL4 + HTN4 + SEX , data=fram_chd)
summary(chd_fit.cox)
chd_fit.cox2 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX, data=fram_chd)
summary(chd_fit.cox2)

# Should I add SEX*HTN4 or SEX*OBESE or SEX*SMOKE or HTN*OBESE or CHOL4*HTN4?
chd_fit.coxx1 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SEX*HTN4, data=fram_chd)
summary(chd_fit.coxx1)
chd_fit.coxx2 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SEX*BMI4, data=fram_chd)
summary(chd_fit.coxx2)
chd_fit.coxx3 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SEX*SMOKE, data=fram_chd)
summary(chd_fit.coxx3)
chd_fit.coxx4 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + HTN4*BMI4, data=fram_chd)
summary(chd_fit.coxx4)
chd_fit.coxx5 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + CHOL4*HTN4, data=fram_chd)
summary(chd_fit.coxx5)

# Test proportional hazard
chd_scho <- cox.zph(chd_fit.coxx5)
chd_scho
par(mfrow = c(4,2))
plot(chd_scho)

## 2) What factors can confound the association between obesity and CHD?
# Difference in crude and adjusted odds ratios
chd_crude.cox <- coxph(Surv(CHD_SURV, CHD) ~ OBESE, data=fram_chd)
chd_cox_crude.or <- summary(chd_crude.cox)$coefficients[2]
chd_cox_full.or <- summary(chd_fit.coxx5)$coefficients[1,2]
# Crude vs all selected variables
abs(chd_cox_crude.or - chd_cox_full.or) / chd_cox_crude.or

forEachTestConfoundingChd <- function(columns) {
  for (c in columns) {
    chd_adj.cox <- coxph(Surv(CHD_SURV, CHD) ~ OBESE + get(c), data=fram_chd)
    chd_cox_adj.or <- summary(chd_adj.cox)$coefficients[1,2]
    print(abs(chd_cox_crude.or - chd_cox_adj.or) / chd_cox_crude.or)
  }
}

forEachTestConfoundingChd(c("SEX", "AGE4", "CHOL4", "HTN4"))

## 3) Is the association between obesity and CHD the same in males and females?
# Fit stratified model using strata() option
men <- fram_chd %>% 
  filter(SEX == 0)

women <- fram_chd %>%
  filter(SEX == 1)

chd_cox.strat <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + strata(SEX), 
               data=fram_chd)
summary(chd_cox.strat)
cox.zph(chd_cox.strat)

chd_cox.men <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4, 
                 data=men)
summary(chd_cox.men)
cox.zph(chd_cox.men)

chd_cox.women <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4, 
                   data=women)
summary(chd_cox.women)
cox.zph(chd_cox.women)

### DTH Analysis
# Variable selection 
# Forward AIC
base_dth <- coxph(Surv(SURV, DTH) ~ 1, data = fram2)
forward.AIC_dth <- step(base_dth, scope = final, direction = "forward", k = 2)
forward.AIC_log_dth <- step(base_dth, scope = final_log, direction = "forward", k = 2)
extractAIC(forward.AIC_dth, k=2)
extractAIC(forward.AIC_log_dth, k=2)
summary(forward.AIC_dth)$coefficients
summary(forward.AIC_log_dth)$coefficients

# Forwards BIC
forward.BIC_dth <- step(base_dth, scope = final, direction = "forward", k = log(nrow(fram2)), trace = F)
forward.BIC_log_dth <- step(base_dth, scope = final_log, direction = "forward", k = log(nrow(fram2)), trace = F)
summary(forward.BIC_dth)$coefficients
summary(forward.BIC_log_dth)$coefficients

# Stepwise 
step_dth <- step(base_dth, scope = final, direction = "both", trace = F)
step_log_dth <- step(base_dth, scope = final_log, direction = "both", trace = F)
summary(step_dth)$coefficients
summary(step_log_dth)$coefficients

# create dataset from selected variables
remove_dth <- c(remove, "HTN4", "CHOL4", "CHD", "CHD_SURV")
fram_dth = fram %>%
  select(-all_of(remove_dth))

# recode sex
fram_dth$SEX = recode_factor(fram_dth$SEX, `1` = 0, `2` = 1)

# omit na's?
colSums(is.na(fram_dth))
fram_dth = na.omit(fram_dth)
col_names = colnames(fram_dth)
n_row = nrow(fram_dth)

## Testing for interactions
interaction("SMOKE", "OBESE", "DTH")
interaction("SEX", "OBESE", "DTH")
interaction("SEX", "SMOKE", "DTH")
interaction("HIGH_CHOL", "HTN4", "DTH")
interaction("HIGH_CHOL", "SEX", "DTH")

## 4) Is obesity associated with mortality?

# Kaplan-Meier Plot
dth.km <- survfit(Surv(SURV, DTH) ~ OBESE, data=fram_dth)
summary(dth.km)
plot(dth.km, col=c(1,2), lwd=2, ylim=c(0,1),
     xlab="Time (Years)", ylab="Survival", cex.axis=1.5, cex.lab=1.5)
legend(x=1, y=0.40, legend=c("Low BMI","High BMI"),
       col=c(1,2), lwd =2, cex=1.2)

# Log-Rank Test
survdiff(Surv(SURV, DTH) ~ OBESE, data=fram_dth)

# Cox Proportional Models 
dth_fit.cox <- coxph(Surv(SURV, DTH) ~ OBESE + AGE4 + CHOL4 + HTN4 + SEX , data=fram_dth)
summary(fit.cox)
dth_fit.cox2 <- coxph(Surv(SURV, DTH) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX, data=fram_dth)
summary(fit.cox2)

# Should I add SEX*HTN4 or SEX*OBESE or SEX*SMOKE or HTN*OBESE or CHOL4*HTN4?
dth_fit.coxx1 <- coxph(Surv(SURV, DTH) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SEX*HTN4, data=fram_dth)
summary(dth_fit.coxx1)
dth_fit.coxx2 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SEX*BMI4, data=fram_dth)
summary(fit.coxx2)
dth_fit.coxx3 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SEX*SMOKE, data=fram_dth)
summary(dth_fit.coxx3)
dth_fit.coxx4 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + HTN4*BMI4, data=fram_dth)
summary(dth_fit.coxx4)
fit.coxx5 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + CHOL4*HTN4, data=fram_dth)
summary(dth_fit.coxx5)

# Test proportional hazard
dth_scho <- cox.zph(dth_fit.coxx5)
dth_scho
par(mfrow = c(4,2))
plot(dth_scho)

## 5) What factors can confound the association between obesity and mortality?
# Difference in crude and adjusted odds ratios
dth_crude.cox <- coxph(Surv(SURV, DTH) ~ OBESE, data=fram_dth)
names(summary(dth_crude.cox))
dth_cox_crude.or <- summary(dth_crude.cox)$coefficients[2]
dth_cox_full.or <- summary(dth_fit.coxx5)$coefficients[1,2]
# Crude vs all selected variables
abs(dth_cox_crude.or - dth_cox_full.or) / dth_cox_crude.or

forEachTestConfoundingDth <- function(columns) {
  for (c in columns) {
    dth_adj.cox <- coxph(Surv(SURV, DTH) ~ OBESE + get(c), data=fram_dth)
    dth_cox_adj.or <- summary(dth_adj.cox)$coefficients[1,2]
    print(abs(dth_cox_crude.or - dth_cox_adj.or) / dth_cox_crude.or)
  }
}

forEachTestConfoundingDth(c("SEX", "AGE4", "CHOL4", "HTN4"))

## 6) Is the association between obesity and mortality the same in males and females?
# Fit stratified model using strata() option
dth_cox.strat <- coxph(Surv(SURV, DTH) ~ BMI4 + AGE4 + CHOL4 + HTN4 + strata(SEX), 
                   data=fram_dth)
summary(dth_cox.strat)
cox.zph(dth_cox.strat)

dth_cox.men <- coxph(Surv(SURV, DTH) ~ BMI4 + AGE4 + CHOL4 + HTN4, data=men)
summary(dth_cox.men)
cox.zph(dth_cox.men)

dth_cox.women <- coxph(Surv(SURV, DTH) ~ BMI4 + AGE4 + CHOL4 + HTN4, data=women)
summary(dth_cox.women)
cox.zph(dth_cox.women)