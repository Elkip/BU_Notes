library(tidyverse)
options(scipen=999)

DATAPATH <- Sys.getenv("OAI_DATA")

getBaselineData <- function(path) {
    print("Loading Files...")
    enrollees_raw <- read.csv(file.path(path, "Enrollees.txt"), header = T, sep = "|")
    clinical0_raw <- read.csv(file.path(path, "AllClinical00.txt"), header = T, sep = "|")
    
    print("Formatting Enrollees Data...")
    enrollees <- data.frame(enrollees_raw) %>% 
        mutate_all(list(~gsub(":.*", "", .))) %>%
        mutate_all(na_if, "") %>% 
        mutate_all(na_if, ".")
    
    e_df <- enrollees %>%
        select(ID = ID, SEX = P02SEX, RACE = P02RACE, ETHNICITY = P02HISP)
    remove(enrollees_raw)
    
    print("Formatting Clinical 0 Data...")
    clinical0 <- data.frame(clinical0_raw) %>% 
        mutate_all(list(~gsub(":.*", "", .))) %>%
        mutate_all(na_if, "") %>%
        mutate_all(na_if, ".")
    
    c0_df <- clinical0  %>%
        mutate(P01OAGRD = pmax(P01OAGRDL, P01OAGRDR), 
               P02JBMPCV_NEW = case_when(
                   P02JBMPCV == 0 ~ 0,
                   P02JBMPCV == 3 ~ 2,
                   TRUE ~ 1),
               WOMADL = pmax(V00WOMADLL, V00WOMADLR),
               WOMKP = pmax(V00WOMKPL, V00WOMKPR),
               WOMSTF = pmax(V00WOMSTFL, V00WOMSTFR),
               Surg_Inj_Hist = pmax(P02KSURG, P02KINJ),
               .keep = "all")
    remove(clinical0_raw)
    
    print("Joining dataframes...")
    data_baseline <- inner_join(e_df, c0_df, by = "ID")  %>%
        select(ID = ID, AGE = V00AGE, SEX = SEX, CEMPLOY = V00CEMPLOY, EDCV = V00EDCV, 
               MEDINS = V00MEDINS, PASE = V00PASE, P01OAGRD = P01OAGRD,
               P02JBMPCV_NEW = P02JBMPCV_NEW, WOMADL = WOMADL, WOMKP = WOMKP, 
               WOMSTF = WOMSTF, V00WTMAXKG = V00WTMAXKG,  V00WTMINKG = V00WTMINKG, 
               BMI = P01BMI, HEIGHT = P01HEIGHT, WEIGHT = P01WEIGHT, 
               COMORBSCORE = V00COMORB, CESD = V00CESD, NSAID = V00RXNSAID, NARC = V00RXNARC,
               RACE=RACE, ETHNICITY = ETHNICITY, Surg_Inj_Hist = Surg_Inj_Hist)
    remove(c0_df) 
    
    print("Creating factor columns...")
    data_baseline <- data_baseline %>% 
        mutate(CEMPLOY_NWOR = coalesce(if_any(CEMPLOY, `==`, 4), 0),
               CEMPLOY_NWH = coalesce(if_any(CEMPLOY, `==`, 3), 0),
               CEMPLOY_FB = coalesce(if_any(CEMPLOY, `==`, 2), 0),
               EDCV_GradDeg = coalesce(if_any(EDCV, `==`, 5), 0),
               EDCV_SomeGrad = coalesce(if_any(EDCV, `==`, 4), 0),
               EDCV_UGDeg = coalesce(if_any(EDCV, `==`, 3), 0),
               EDCV_SomeUG = coalesce(if_any(EDCV, `==`, 2), 0),
               EDCV_HSDeg = coalesce(if_any(EDCV, `==`, 1), 0),
               P01OAGRD_Severe = coalesce(if_any(P01OAGRD, `==`, 4), 0),
               P01OAGRD_Moderate = coalesce(if_any(P01OAGRD, `==`, 3), 0),
               P01OAGRD_Mild = coalesce(if_any(P01OAGRD, `==`, 2), 0),
               P01OAGRD_Possible = coalesce(if_any(P01OAGRD, `==`, 1), 0),
               P02JBMPCV_NEW_None = coalesce(if_any(P02JBMPCV_NEW, `==`, 0), 0),
               P02JBMPCV_NEW_One = coalesce(if_any(P02JBMPCV_NEW, `==`, 1), 0),
               RACE_AA = coalesce(if_any(RACE, `==`, 2), 0),
               RACE_NW = coalesce(if_any(RACE, `>`, 2), 0),
               DPRSD = coalesce(if_any(CESD, `>=`, 16), 0)) %>% 
        select(-c(CEMPLOY, EDCV, P01OAGRD, P02JBMPCV_NEW, RACE, CESD)) 
    
    print("Converting column types...")
    num_col <- c(1:2, 5:15)
    fac_col <- c(3, 4, 16:35)
    data_baseline[,num_col] <- sapply(data_baseline[,num_col], as.numeric)
    for (i in fac_col) {
        data_baseline[,i] <- sapply(data_baseline[,i], as.factor) 
    }
    print("Baseline Data Loading Complete")
    return(data_baseline)
}

bsln <- getBaselineData(DATAPATH)

# For each patient assign an outcome:
# 1: No event, no death
# 2: Left study before event
# 3: Death
# 4...8: Five-Level Random Forest Clusters
outcomeLabel <- function(i) {
  switch(as.character(i),
         '1' = {"No Event"},
         '2' = {"Drop Out"},
         '3' = {"Death"},
         (paste("Knee Replacement Cluster", i-3))
  )
}

getEvents <- function(path) {
  outcomes_raw <- read.csv(file.path(DATAPATH, "Outcomes99.txt"), header = T, sep = "|")
  outcomes <- data.frame(outcomes_raw) %>% 
    mutate_all(list(~gsub(":.*", "", .))) %>%
    mutate_all(na_if, ".") %>%
    replace(is.na(.), 0)  %>% 
    mutate_all(na_if, "") %>% 
    mutate(ID = id, DTH = V99EDDCF, # Death
           DTH_DT = V99EDDDATE,     # Date of Death
           DTH_VST = V99EDDVSPR,    # Closest OAI contact prior to death
           KNEE_RPLC_PRE = pmax(V99ERKBLRP, V99ELKBLRP), # Knee replacement at baseline
           KNEE_RPLC = pmax(V99ERKRPSN, V99ELKRPSN),     # Knee replacement seen on follow-up
           KNEE_OUTCM = pmax(V99ERKTLPR, V99ELKTLPR),    # Total or Partial Follow-up knee replacement
           KNEE_CONF = pmax(V99ELKRPCF, V99ERKRPCF),     # Replacement Confirmation 
           KNEE_RPLC_DT = pmin(V99ERKDATE, V99ERKDATE, na.rm = T),
           KNEE_RPLC_VST = pmin(V99ELKVSPR, V99ERKVSPR, na.rm = T), # Closest OAI prior to replacement
           LAST_CONTACT = V99RNTCNT,
           .keep = "none")
  events <- outcomes %>% mutate(ID = as.numeric(ID), 
                                EVNT = as.factor(case_when(
                                  KNEE_CONF != 0 ~ 4,
                                  (LAST_CONTACT != 11 & DTH != 0) ~ 3,
                                  LAST_CONTACT != 11 ~ 2,
                                  TRUE ~ 1
                                )),
                                EVNT_VST = case_when(
                                  EVNT == 3 ~ KNEE_RPLC_VST,
                                  EVNT == 4 ~ DTH_VST,
                                  TRUE ~ LAST_CONTACT
                                ),
                                .keep = "none")
  return(events)
}

evnt <- getEvents(DATAPATH)

nrow(evnt[which(evnt$EVNT == 4),])
nrow(evnt[which(evnt$EVNT == 3),])
nrow(evnt[which(evnt$EVNT == 2),])
nrow(evnt[which(evnt$EVNT == 1),])

# Compare to Brooke's Clusters
clstrs_bsln_info <- read.csv(file.path(DATAPATH, "OAI_Clust_Assignments_w_info_V5.csv"), header = T, sep = ",")
case_evnts_brk <- evnt[evnt$ID %in% clstrs_bsln_info$ID,]
case_evnts_mine <- evnt[which(evnt$EVNT == 4),]
case_evnts_diff <- case_evnts_mine[!(case_evnts_mine$ID %in% case_evnts_brk$ID),]
case_bsln_diff <- na.omit(bsln[bsln$ID %in% case_evnts_diff$ID,]) # still 12 unaccounted replacements

remove(clstrs_bsln_info, case_evnts_mine, case_evnts_brk, case_evnts_diff, case_bsln_diff)

# Attach predicted RF cluster ID to baseline data
getCompleteData <- function(path, cluster) {
    clstrs_bsln_info <- read.csv(file.path(path, "OAI_Clust_Assignments_w_info_V5.csv"), header = T, sep = ",")
    
    # Attach predicted RF K=5 cluster ID to baseline data
    clstrs <- clstrs_bsln_info[,c("ID",cluster)]
    
    # Create the knee replacement event represented by 4 through 8
    clstrs[,cluster] <- clstrs[,cluster] - 1 # Start first cluster at 0
    
    complete_data <- bsln %>% full_join(evnt, by = NULL) %>%
        left_join(clstrs, by = NULL)
    
    complete_data[,cluster] <- complete_data[,cluster] %>% 
        replace_na(0)
    
    complete_data$EVNT <- rowSums(cbind(as.numeric(complete_data$EVNT), 
                                        as.numeric(complete_data[,cluster])))
    
    complete_data <- complete_data %>% select(-cluster)
    
    # SomeGrad and SomeUG were previously determined to be unimportant, drop them
    complete_data <- complete_data %>% 
        mutate(EDCV_UGDeg = as.factor((as.numeric(complete_data$EDCV_UGDeg)-1) + 
                                          (as.numeric(complete_data$EDCV_SomeGrad)-1))) %>% 
        mutate(EDCV_HSDeg = as.factor((as.numeric(complete_data$EDCV_HSDeg)-1) + 
                                          (as.numeric(complete_data$EDCV_SomeUG)-1))) %>%
        select(-c(EDCV_SomeUG, EDCV_SomeGrad))
    
    return(complete_data)
}

data_5clust <- getCompleteData(DATAPATH, "K.5.Clusters")
data_4clust <- getCompleteData(DATAPATH, "K.4.Clusters")

data_cases <- data_5clust[data_5clust$EVNT >= 4,]
data_cntrl <- data_5clust[data_5clust$EVNT < 4,]

# REMOVE NA
sum(!complete.cases(bsln)) # 359 non-complete at baseline
sum(!complete.cases(data_cases)) # 28 w Knee Replacements
sum(!complete.cases(data_cntrl)) # 331 Control
data_5clust <- na.omit(data_5clust)
data_4clust <- na.omit(data_4clust)

# Multinomial Distribution with event as the outcome
library(nnet)

# Base Model
mod_base <- multinom(EVNT ~ AGE + SEX + WOMKP + BMI, data=data_full)
summary(mod_base)
z <- summary(mod_base)$coefficients/summary(mod_base)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
exp(coef(mod_base))

# Saturated Model
mod_sat <- multinom(EVNT ~ AGE + SEX + RACE_NW
                 + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                 + MEDINS + PASE + WOMADL + WOMKP + WOMSTF + BMI + HEIGHT 
                 + WEIGHT + COMORBSCORE + DPRSD + NSAID + NARC + P01OAGRD_Severe
                 + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                 + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + EDCV_GradDeg
                 + EDCV_UGDeg + EDCV_HSDeg + V00WTMAXKG + V00WTMINKG 
                 + Surg_Inj_Hist, data=data_full)
summary(mod_sat)
z <- summary(mod_sat)$coefficients/summary(mod_sat)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
exp(coef(mod_sat))

# Step-wise selection to choose the best predictors
# beststep_forward <- step(mod_base, direction = 'forward', scope=formula(mod_sat), trace=0)
# beststep_forward$anova
#beststep_forward$coefnames
# beststep_backward <- step(mod_sat, direction = 'backward', scope=formula(mod_sat), trace=0)
# beststep_backward$anova
# beststep_backward$coefnames
beststep_both <- step(mod_base, direction = 'both', scope=formula(mod_sat), trace=0)
beststep_both$anova
beststep_both$coefnames

predictors_best <- c("AGE", "SEX", "RACE_AA", "CEMPLOY_NWH", "PASE", "WOMKP",
                "WOMSTF", "BMI", "WEIGHT", "DPRSD", "NSAID", "P01OAGRD_Severe", 
                "P01OAGRD_Moderate", "P01OAGRD_Mild", "P01OAGRD_Possible",
                "EDCV_HSDeg","EDCV_GradDeg", "EDCV_UGDeg", "V00WTMAXKG")

# The model with both step selection has the lowest AIC
eqtn_best <- formula(paste("EVNT ~ ", paste(predictors_best, collapse = " + ")))
mod_best <- multinom(eqtn_best, data=data_full)
summary(mod_best)
z <- summary(mod_best)$coefficients/summary(mod_best)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
odds <- exp(coef(mod_best))
param_ci <- confint(mod_best, level = .90)
odds_ci <- exp(param_ci)

# Plot the probabilities for each predictor
library(ggeffects)
lapply(predictors_best, function(x) plot(ggpredict(mod_best, terms=paste(x, "[all]"))))

# Plot CI of Odds for Each Predictor
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
predictors_all <- c("ID", "AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP",
                    "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT",
                    "COMORBSCORE", "DPRSD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist",
                    "CEMPLOY_NWOR", "CEMPLOY_NWH", "CEMPLOY_FB", "EDCV_GradDeg", 
                    "EDCV_UGDeg", "EDCV_HSDeg", "P01OAGRD_Severe", "P01OAGRD_Moderate", 
                    "P01OAGRD_Mild", "P01OAGRD_Possible", "P02JBMPCV_NEW_None", 
                    "P02JBMPCV_NEW_One", "RACE_AA", "RACE_NW", "EVNT", "EVNT_VST")
# rename columns to be 8 characters
names(data_full) <- c("ID", "AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP",
                      "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT",
                      "COMORBSCORE", "DPRSD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist",
                      "CEMP_NWOR", "CEMP_NWH", "CEMP_FB", "EDCV_GradDeg",
                      "EDCV_UGDeg", "EDCV_HSDeg", "GRD_Severe", "GRD_Moderate", 
                      "GRD_Mild", "GRD_Possible", "BMP_None", "BMP_One", "RACE_AA", 
                      "RACE_NW", "EVNT", "EVNT_VST")

write.foreign(data_full, paste(DATAPATH, "full_data.txt", sep=""), 
              paste(DATAPATH, "load_data.sas", sep=""), package = "SAS")

# Rename to normal
names(data_full) <- predictors_all

# Checking the accuracy of created clusters compared to original clusters
library(randomForest)
# trn_data <- rfImpute(data_cases[,2:35], data_cases[,36], data=data_cases)
set.seed(1)

smp_size <- floor(5*nrow(data_full)/6) # Five-fold
trn_ind <- sample(seq_len(nrow(data_full)), size = smp_size, replace = F)
trn_data <- data_full[trn_ind,]
tst_data <- data_full[-trn_ind,]

which(is.na(trn_data), arr.ind = TRUE) # 12 NA values in baseline cases
# colnames(trn_data)[1] <- "Cluster"

rf_full <- randomForest(as.factor(EVNT) ~  AGE + SEX + RACE_NW
                   + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                   + MEDINS + PASE + WOMADL + WOMKP + WOMSTF + BMI + HEIGHT 
                   + WEIGHT + COMORBSCORE + DPRSD + NSAID + NARC + P01OAGRD_Severe
                   + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                   + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + EDCV_GradDeg
                   + EDCV_SomeGrad + EDCV_UGDeg + EDCV_SomeUG 
                   + EDCV_HSDeg+ V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, 
                   data=trn_data, proximity=TRUE, localImp = TRUE)
plot(rf_full, log="y")
importance(rf_full)
varImpPlot(rf_full)
table(predict(rf_full), trn_data$EVNT)
getTree(rf_full, 1, labelVar = TRUE)
pred <- predict(rf_full, newdata = tst_data)
summary(pred)
table(pred, tst_data$EVNT)

eqtn_frst <- formula(paste("as.factor(EVNT) ~ ", paste(predictors, collapse = " + ")))
rf_best <- randomForest(eqtn_frst, data=trn_data, proximity=TRUE, localImp = TRUE)
plot(rf_best, log="y")
importance(rf_best)
varImpPlot(rf_best)
table(predict(rf_best), trn_data$EVNT)
getTree(rf_best, 1, labelVar = TRUE)
pred <- predict(rf_best, newdata = tst_data)
summary(pred)
table(pred, tst_data$EVNT)

library(randomForestExplainer)
explain_forest(rf_best, interactions = TRUE, data = trn_data)
