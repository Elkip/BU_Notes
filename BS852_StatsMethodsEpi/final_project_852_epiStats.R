library(tidyverse)
library(survival)
library(reshape2)
library(ggplot2)

fram <- read.csv("/home/elkip/Datasets/framdat4.csv")
col_names <- colnames(fram)
sum(fram$SEX)
colSums(is.na(fram))

# Pearson's corr:
corr <- round(cor(fram, use="pairwise.complete.obs", method = "pearson"), 2)
melted_corr <- melt(corr)

ggplot(data = melted_corr, aes(x=Var1, Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label=value), color = "white") +
  ggtitle("Figure 1.1 - Correlation")

## List correlated variables
for (i in col_names) {
  for (j in col_names) {
      if (!is.na(corr[i,j]) & i != j & abs(corr[i,j]) > .65) {
        print(paste("Cor between",i,"and",j,"is",corr[i,j]))
      }
    }
}

# Drop 1 of the correlated pairs, and uninformative variables:
remove <- c("WGT4", "SPF4", "CIGS4", "MENO4", "T2D", "T2D_SURV")

# Drop selected columns, add BMI Category
fram2 = fram %>%
  select(-all_of(remove)) %>%
  mutate(OBESE = case_when(BMI4 >= 30 ~ 1, BMI4 < 30 ~ 0)) %>%
  mutate(HIGH_CHOL = case_when(CHOL4 >= 240 ~ 1, CHOL4 < 240 ~ 0))

colSums(is.na(fram2))
fram2 = na.omit(fram2)
fram2$SEX = fram2$SEX - 1
col_names = colnames(fram2)
n_rows = nrow(fram2)

# Characteristics of 1732 Participants by obesity
prop_cols <- c("OBESE","SEX", "HTN4", "SMOKE", "DTH", "CHD", "HIGH_CHOL")
prop_per = fram2  %>%
  select(c(prop_cols)) %>% 
  group_by(OBESE) %>%
  summarise_all(list(sum = sum, mean = mean))

mean_sd = fram2 %>%
  select(-prop_cols, "OBESE") %>%
  group_by(OBESE) %>%
  summarise_all(list(mean = mean, sd = sd))
  
charcts <- data.frame(matrix(ncol=6, nrow = 0))
colnames(charcts) <- c("Characteristic", "Obese (n=259)", "", "Non-Obese (n=1473)", "", "p")
for (c in prop_cols[-1]) {
  col1 <- paste(c, "_sum", sep = "")
  col2 <- paste(c, "_mean", sep = "")
  p = t.test(get(c) ~ OBESE, data = fram2)$p.value
  crow <- c(paste(c, ", n (%)", sep = ""), prop_per[1,col1], prop_per[1,col2], prop_per[2,col1], prop_per[2,col2], p)
  charcts[nrow(charcts) + 1,] = crow
}
for (c in colnames(select(fram2, -prop_cols))) {
  col1 <- paste(c, "_mean", sep = "")
  col2 <- paste(c, "_sd", sep = "")
  p = t.test(get(c) ~ OBESE, data = fram2)$p.value
  crow <- c(paste(c, ", mean (sd)", sep = ""), mean_sd[1,col1], mean_sd[1,col2], mean_sd[2,col1], mean_sd[2,col2], p)
  charcts[nrow(charcts) + 1,] = crow
}
print(charcts)

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
final <-  ~ BMI4 + SEX + AGE4 + CHOL4 + DPF4 + FVC4 + OBESE + HTN4 + SMOKE + HIGH_CHOL
final_log <- ~  LOG_BMI + SEX + AGE4 + LOG_CHOL + LOG_DPF + FVC4 +  OBESE + HTN4 + SMOKE + HIGH_CHOL
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
# Variable Selection: AGE + SEX + CHOL4 + BMI4 + HTN4 + SMOKE

## Testing for interactions
interaction <- function(var1, var2, out) {
  n11 <- nrow(fram2 %>% filter(get(var1) == 1 & get(var2) == 1))
  p11 <- nrow(fram2 %>% filter(get(var1) == 1 & get(var2) == 1 & get(out) == 1)) / n11
  n01 <- nrow(fram2 %>% filter(get(var1) == 0 & get(var2) == 1))
  p01 <- nrow(fram2 %>% filter(get(var1) == 0 & get(var2) == 1 & get(out) == 1)) / n01
  n10 <- nrow(fram2 %>% filter(get(var1) == 1 & get(var2) == 0))
  p10 <- nrow(fram2 %>% filter(get(var1) == 1 & get(var2) == 0 & get(out) == 1)) / n10
  n00 <- nrow(fram2  %>% filter(get(var1) == 0 & get(var2) == 0))
  p00 <-nrow(fram2 %>% filter(get(var1) == 0 & get(var2) == 0 & get(out) == 1)) / n00
    
  
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
interaction("HIGH_CHOL", "SMOKE", "CHD")
interaction("HIGH_CHOL", "OBESE", "CHD")

## 1) Is obesity associated with CHD?
# Kaplan-Meier Plot
par(mfrow=c(1,1))
chd.km <- survfit(Surv(CHD_SURV, CHD) ~ OBESE, data=fram2)
summary(chd.km)
plot(chd.km, col=c(1,2), lwd=2, ylim=c(0,1),
     xlab="Time (Years)", ylab="CHD free survival", cex.axis=1.5, cex.lab=1.5)
legend(x=1, y=0.40, legend=c("Low BMI","High BMI"),
       col=c(1,2), lwd =2, cex=1.2)
title("Figure 2.1")

# Log-Rank Test
survdiff(Surv(CHD_SURV, CHD) ~ OBESE, data=fram2)

# Crude Cox
chd_crd.cox <- coxph(Surv(CHD_SURV, CHD) ~ OBESE, data=fram2)
summary(chd_crd.cox)
chd_crd.cox2 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4, data=fram2)
summary(chd_crd.cox2)
# Adjusted Cox Proportional Models 
chd_fit.cox <- coxph(Surv(CHD_SURV, CHD) ~ OBESE + AGE4 + CHOL4 + HTN4 + SEX + SMOKE, data=fram2)
summary(chd_fit.cox)
chd_fit.cox2 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + HTN4 + SEX + SMOKE, data=fram2)
summary(chd_fit.cox2)
chd_fit.cox3 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + HIGH_CHOL + HTN4 + SEX + SMOKE, data=fram2)
summary(chd_fit.cox3)

# Test proportional hazard
chd_scho <- cox.zph(chd_fit.cox3)
chd_scho
par(mfrow = c(4,2))
plot(chd_scho)

# Add stratifier for age
age.group <- rep(1, nrow(fram2))
age.group[ which(fram2$AGE4 >44 & fram2$AGE4 <55)] <- 2
age.group[ which(fram2$AGE4 >54 & fram2$AGE4 <65)] <- 3
age.group[ which(fram2$AGE4 >64 & fram2$AGE4 <75)] <- 4
fram2$AGE_CAT = age.group

# Add time-varying variaable for Bmi
chd_fit.cox5 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + tt(BMI4) + strata(AGE_CAT) + HIGH_CHOL + HTN4 + SEX + SMOKE, data=fram2)
summary(chd_fit.cox5)

# Should I add SEX*HTN4 or SEX*OBESE or SEX*SMOKE or HTN*OBESE?
chd_fit.coxx1 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + tt(BMI4) + strata(AGE_CAT) + HIGH_CHOL + HTN4 + SEX + SMOKE + SEX*BMI4, data=fram2)
summary(chd_fit.coxx1)
chd_fit.coxx2 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + HIGH_CHOL + HTN4 + SEX + SMOKE + SMOKE*BMI4, data=fram2)
summary(chd_fit.coxx2)
chd_fit.coxx3 <- coxph(Surv(CHD_SURV, CHD) ~ OBESE + AGE4 + HIGH_CHOL + HTN4 + SEX + SMOKE + OBESE*SEX, data=fram2)
summary(chd_fit.coxx3)
chd_fit.coxx4 <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + HIGH_CHOL + HTN4 + SEX + SMOKE + HTN4*BMI4, data=fram2)
summary(chd_fit.coxx4)

## 2) What factors can confound the association between obesity and CHD?
# Difference in crude and adjusted hazard ratio
crude.cox <- coxph(Surv(CHD_SURV, CHD) ~ BMI4, data=fram2)
cox_crude.or <- summary(crude.cox)$coefficients[2]
cox_full.or <- summary(chd_fit.cox5)$coefficients[1,2]
# Crude vs all selected variables
abs(cox_crude.or - cox_full.or) / cox_crude.or

forEachTestConfounding <- function(columns, out, surv, risk) {
  for (c in columns) {
    crude.cox <- coxph(Surv(get(surv), get(risk)) ~ get(out), data=fram2)
    cox_crude.or <- summary(crude.cox)$coefficients[2]
    adj.cox <- coxph(Surv(get(surv), get(risk)) ~ get(out) + get(c), data=fram2)
    cox_adj.or <- summary(adj.cox)$coefficients[1,2]
    print(paste(c,abs(cox_crude.or - cox_adj.or) / cox_crude.or))
  }
}

forEachTestConfounding(c("SEX", "AGE4", "SMOKE", "HTN4", "HIGH_CHOL"), "BMI4", "CHD_SURV", "CHD")
forEachTestConfounding(c("SEX", "AGE4", "SMOKE", "HTN4", "HIGH_CHOL"), "OBESE", "CHD_SURV", "CHD")

## 3) Is the association between obesity and CHD the same in males and females?
# Fit stratified model using strata() option
men <- fram2 %>% 
  filter(SEX == 0)

women <- fram2 %>%
  filter(SEX == 1)

chd_cox.strat <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + SMOKE + HTN4 + strata(SEX), 
               data=fram2)
summary(chd_cox.strat)
cox.zph(chd_cox.strat)

chd_cox.men <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + SMOKE + HTN4, 
                 data=men)
summary(chd_cox.men)
cox.zph(chd_cox.men)

chd_cox.women <- coxph(Surv(CHD_SURV, CHD) ~ BMI4 + AGE4 + CHOL4 + SMOKE + HTN4, 
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

# Selected variables: AGE, SEX, DPF, SMOKE, BMI

## Testing for interactions
interaction("SMOKE", "OBESE", "DTH")
interaction("SEX", "OBESE", "DTH")
interaction("SEX", "SMOKE", "DTH")

## 4) Is obesity associated with mortality?
# Kaplan-Meier Plot
dth.km <- survfit(Surv(SURV, DTH) ~ OBESE, data=fram2)
summary(dth.km)
plot(dth.km, col=c(1,2), lwd=2, ylim=c(0,1),
     xlab="Time (Years)", ylab="Survival", cex.axis=1.5, cex.lab=1.5)
legend(x=1, y=0.40, legend=c("Non-Obese","Obese"),
       col=c(1,2), lwd =2, cex=1.2)
title("Figure 3.1")
# Log-Rank Test
survdiff(Surv(SURV, DTH) ~ OBESE, data=fram2)

# Cox crude
dth_crd.cox <- coxph(Surv(SURV, DTH) ~ BMI4, data = fram2)
summary(dth_crd.cox)
dth_crd.cox2 <- coxph(Surv(SURV, DTH) ~ OBESE, data = fram2)
summary(dth_crd.cox2)
# Cox Proportional Models 
dth_fit.cox <- coxph(Surv(SURV, DTH) ~ OBESE + AGE4 + DPF4 + FVC4 + SEX, data=fram2)
summary(dth_fit.cox)
dth_fit.cox2 <- coxph(Surv(SURV, DTH) ~ BMI4 + AGE4 + DPF4 + FVC4 + SEX, data=fram2)
summary(dth_fit.cox2)
dth_fit.cox3 <- coxph(Surv(SURV, DTH) ~ BMI4 + AGE4 + HTN4 + SMOKE + SEX, data=fram2)
summary(dth_fit.cox3)
dth_fit.cox4 <- coxph(Surv(SURV, DTH) ~ OBESE + AGE4 + HTN4 + SMOKE + SEX, data=fram2)
summary(dth_fit.cox4)

# Should I add SEX*HTN4 or SEX*OBESE or SEX*SMOKE or HTN*OBESE or CHOL4*HTN4?
dth_fit.coxx1 <- coxph(Surv(SURV, DTH) ~ BMI4 + AGE4 + HTN4 + SMOKE + SEX + SMOKE + SEX*SMOKE, data=fram2)
summary(dth_fit.coxx1)
dth_fit.coxx2 <- coxph(Surv(SURV, DTH) ~ BMI4 + AGE4 + HTN4 + SMOKE + SEX + SEX*BMI4, data=fram2)
summary(dth_fit.coxx2)
dth_fit.coxx3 <- coxph(Surv(SURV, DTH) ~ OBESE + AGE4 + HTN4 + SMOKE + SEX + SEX*OBESE, data=fram2)
summary(dth_fit.coxx3)

# Test proportional hazard
dth_scho <- cox.zph(dth_fit.cox4)
dth_scho
par(mfrow = c(4,2))
plot(dth_scho)

## What factors can confound the association between obesity and mortality?
# Difference in crude and adjusted odds ratios
crude.cox <- coxph(Surv(SURV, DTH) ~ OBESE, data=fram2)
cox_crude.or <- summary(crude.cox)$coefficients[2]
cox_full.or <- summary(dth_fit.cox4)$coefficients[1,2]
# Crude vs all selected variables
abs(cox_crude.or - cox_full.or) / cox_crude.or

forEachTestConfounding(c("SEX", "AGE4", "DPF4", "FVC4", "SMOKE"), "OBESE", "SURV", "DTH")
forEachTestConfounding(c("SEX", "AGE4", "DPF4", "FVC4", "SMOKE"), "BMI4", "SURV", "DTH")

