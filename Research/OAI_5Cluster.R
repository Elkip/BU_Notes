options(scipen=999)

DATAPATH <- Sys.getenv("OAI_DATA")

if(!exists('bsln')) {
    # LoadData file must be in the same directory
    source("OAI_LoadData.R", chdir = T)
}

data_k5 <- getCompleteData(DATAPATH, "K.5.Clusters")

# REMOVE NA
print(paste("Number of NA:", sum(is.na(data_k5))))
data_k5 <- na.omit(data_k5)

# Multinomial Distribution with event as the outcome
library(nnet)
# Base Model
mod_base <- multinom(EVNT ~ AGE + SEX + WOMKP + BMI, data=data_k5)
summary(mod_base)
dev_base <- deviance(mod_base)
df_base <- mod_base$edf

# Full Model
mod_full <- multinom(EVNT ~ AGE + SEX + RACE_NW  + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB  + MEDINS + PASE + WOMADL + WOMKP + WOMSTF + BMI + HEIGHT  + WEIGHT + COMORBSCORE + DPRSD + NSAID + NARC + P01OAGRD_Severe + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible  + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + EDCV_GradDeg + EDCV_UGDeg + EDCV_HSDeg + V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, data=data_k5, maxit = 1000)
summary(mod_full)
dev_full <- deviance(mod_full)
df_full <- mod_full$edf
pchisq(dev_base - dev_full, df_full - df_base,lower = F)

### Step-wise selection to choose the best predictors
# Forward
beststep_forward <- step(mod_base, direction = 'forward', scope=formula(mod_full), trace=0, scale = 'glm')
summary(beststep_forward)
beststep_forward$anova
beststep_forward$coefnames
dev_for <- deviance(beststep_forward)
df_for <- beststep_forward$edf
pchisq(dev_for-dev_full, df_full - df_for,lower = F)
# Backward
beststep_backward <- step(mod_full, direction = 'backward', scope=formula(mod_full), trace=0, scale = 'glm')
summary(beststep_backward)
beststep_backward$anova
beststep_backward$coefnames
dev_bac <- deviance(beststep_backward)
df_bac <- beststep_backward$edf
pchisq(dev_bac-dev_full, df_full - df_bac, lower = F)
# Both Direction
beststep_both <- step(mod_base, direction = 'both', scope=formula(mod_full), trace=0, scale = 'glm')
summary(beststep_both)
beststep_both$anova
beststep_both$coefnames
dev_both <- deviance(beststep_both)
df_both <- beststep_both$edf
pchisq(dev_both-dev_full, df_full - df_both, lower = F)

### Best Model
predictors_best <- c("AGE", "SEX", "RACE_AA", "RACE_NW", "CEMPLOY_NWH", "PASE", "WOMKP","WOMSTF", "BMI", "WEIGHT", "NSAID", "P01OAGRD_Severe", "P01OAGRD_Moderate", "P01OAGRD_Mild", "P01OAGRD_Possible", "EDCV_HSDeg","EDCV_GradDeg", "EDCV_UGDeg", "V00WTMAXKG", "Surg_Inj_Hist")

# The model with both step selection has the lowest AIC
eqtn_best <- formula(paste("EVNT ~ ", paste(predictors_best, collapse = " + ")))
mod_best <- multinom(eqtn_best, data=data_k5, maxit = 1000)
summary(mod_best)
z <- summary(mod_best)$coefficients/summary(mod_best)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Plot the probabilities for each predictor
library(ggeffects)
lapply(predictors_best, function(x) plot(ggpredict(mod_best, terms=paste(x, "[all]"))))

# Plot CI of Odds for Each Predictor
odds <- exp(coef(mod_best))
param_ci <- confint(mod_best, level = .90)
odds_ci <- exp(param_ci)
p <- list()
for (i in 1:nrow(odds)) {
    odds_df <- data.frame(boxOdds = odds[i,-1],
                          boxCILow = odds_ci[,1,i][-1],
                          boxCIHigh = odds_ci[,2,i][-1])
    ci_min = min(odds_df$boxCILow)
    ci_max = max(odds_df$boxCIHigh)
    plt <- ggplot(odds_df,  aes(x = boxOdds, y = predictors_best)) + 
        ylab("Predictor") +
        xlab("Odds Ratio") +
        ggtitle(paste("Odds of", outcomeLabel(i+1))) + 
        geom_point() + 
        geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow)) +
        scale_y_discrete(labels = predictors_best) +
        scale_x_continuous(breaks = seq(.1, ci_max + .1, signif((ci_max - ci_min) / 10, 3)),
                           labels = seq(.1, ci_max + .1, signif((ci_max - ci_min) / 10, 3)), 
                           limits = c(ci_min, ci_max)) +
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed")
    
    p[[i]] <- plt
}
p

# Output the full dataset for verification in SAS
library(foreign)
predictors_all <- c("ID", "AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP", "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", "DPRSD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist", "CEMPLOY_NWOR", "CEMPLOY_NWH", "CEMPLOY_FB", "EDCV_GradDeg", "EDCV_UGDeg", "EDCV_HSDeg", "P01OAGRD_Severe", "P01OAGRD_Moderate", "P01OAGRD_Mild", "P01OAGRD_Possible", "P02JBMPCV_NEW_None", "P02JBMPCV_NEW_One", "RACE_AA", "RACE_NW", "EVNT", "EVNT_VST")
# rename columns to be 8 characters
names(data_k5) <- c("ID", "AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP", "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", "DPRSD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist", "CEMP_NWOR", "CEMP_NWH", "CEMP_FB", "EDCV_GradDeg", "EDCV_UGDeg", "EDCV_HSDeg", "GRD_Severe", "GRD_Moderate", "GRD_Mild", "GRD_Possible", "BMP_None", "BMP_One", "RACE_AA", "RACE_NW", "EVNT", "EVNT_VST")

write.foreign(data_k5, paste(DATAPATH, "data_k5.txt", sep=""), 
              paste(DATAPATH, "load_data.sas", sep=""), package = "SAS")

# Rename to normal
names(data_k5) <- predictors_all