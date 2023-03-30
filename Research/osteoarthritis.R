library(tidyverse)
options(scipen=999)

data_path <- Sys.getenv("OAI_DATA")

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
           RACE_NW = coalesce(if_any(RACE, `>`, 2), 0)
    ) %>% 
    select(-c(CEMPLOY, EDCV, P01OAGRD, P02JBMPCV_NEW, RACE)) 
  
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

bsln <- getBaselineData(data_path)

sum(!complete.cases(bsln)) # 359 non-complete at baseline

# For each patient assign an outcome:
# 1: No event, no death
# 2: Left study before event
# 3: Death
# 4...8: Five-Level Random Forest Clusters
getEvents <- function(path) {
  outcomes_raw <- read.csv(file.path(data_path, "Outcomes99.txt"), header = T, sep = "|")
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

evnt <- getEvents(data_path)

nrow(evnt[which(evnt$EVNT == 4),])
nrow(evnt[which(evnt$EVNT == 3),])
nrow(evnt[which(evnt$EVNT == 2),])
nrow(evnt[which(evnt$EVNT == 1),])

# Compare to Brooke's Clusters
clstrs_bsln_info <- read.csv(file.path(data_path, "OAI_Clust_Assignments_w_info_V5.csv"), header = T, sep = ",")
fnl_cases <- clstrs_bsln_info[,c(2:36,44)]

case_evnts_brk <- evnt[evnt$ID %in% clstrs_bsln_info$ID,]
case_evnts_mine <- evnt[which(evnt$EVNT == 4),]
case_evnts_diff <- case_evnts_mine[!(case_evnts_mine$ID %in% case_evnts_brk$ID),]
case_bsln_diff <- bsln[bsln$ID %in% case_evnts_diff$ID,]
# cases_outcm_diff <- outcomes[outcomes$ID %in% case_bsln_diff$ID,]

# Attach predicted RF K=5 cluster ID to baseline data
clstrs <- clstrs_bsln_info[,c(2,44)]
clstrs[,2] <- clstrs[,2] - 1 

# Create the knee replacement event represented by 4 through 8
data_full <- bsln %>% full_join(evnt, by = NULL) %>%
  left_join(clstrs, by = NULL)
data_full$RF.5.Clusters <- data_full$RF.5.Clusters %>% 
  replace(is.na(.), 0)
data_full$EVNT <- rowSums(cbind(as.numeric(data_full$EVNT), as.numeric(data_full$RF.5.Clusters)))
data_full <- data_full[,-38]
data_cases <- data_full[data_full$EVNT >= 4,]
data_cntrl <- data_full[data_full$EVNT < 4,]

sum(!complete.cases(data_cases))
sum(!complete.cases(data_cntrl))

# Output the full dataset for verification in SAS
library(foreign)
# rename columns to be 8 characters
names(data_full) <- c("ID", "AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP",
                      "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT",
                      "COMORBSCORE", "CESD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist",
                      "CEMP_NWOR", "CEMP_NWH", "CEMP_FB", "EDCV_GradDeg",
                      "EDCV_SomeGrad", "EDCV_UGDeg", "EDCV_SomeUG", "EDCV_HSDeg",
                      "GRD_Severe", "GRD_Moderate", "GRD_Mild", "GRD_Possible", 
                      "BMP_None", "BMP_One", "RACE_AA", "RACE_NW", "EVNT", 
                      "EVNT_VST")
write.foreign(data_full, paste(data_path, "full_data.txt", sep=""), 
              paste(data_path, "load_data.sas", sep=""), package = "SAS")
# Rename to normal
names(data_full) <- c("ID", "AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP",
                      "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT",
                      "COMORBSCORE", "CESD", "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist",
                      "CEMPLOY_NWOR", "CEMPLOY_NWH", "CEMPLOY_FB", "EDCV_GradDeg",
                      "EDCV_SomeGrad", "EDCV_UGDeg", "EDCV_SomeUG", "EDCV_HSDeg",
                      "P01OAGRD_Severe", "P01OAGRD_Moderate", "P01OAGRD_Mild",
                      "P01OAGRD_Possible", "P02JBMPCV_NEW_None", "P02JBMPCV_NEW_One",
                      "RACE_AA", "RACE_NW", "EVNT", "EVNT_VST")
# Multinomial Distribution with event as the outcome
library(nnet)
# Saturated Model
mod1 <- multinom(EVNT ~ AGE + SEX + RACE_NW
                 + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                 + MEDINS + PASE + WOMADL + WOMKP + WOMSTF + BMI + HEIGHT 
                 + WEIGHT + COMORBSCORE + CESD + NSAID + NARC + P01OAGRD_Severe
                 + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                 + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + EDCV_GradDeg
                 + EDCV_SomeGrad + EDCV_UGDeg + EDCV_SomeUG 
                 + EDCV_HSDeg + V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, data=data_full)
summary(mod1)
z1 <- summary(mod1)$coefficients/summary(mod1)$standard.errors
p1 <- (1 - pnorm(abs(z1), 0, 1)) * 2
exp(coef(mod1))

# Remove non-significant terms; WOMDL and CESD
mod2 <- multinom(EVNT ~ AGE + SEX + RACE_NW
                 + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                 + MEDINS + PASE + WOMKP + WOMSTF + BMI + HEIGHT 
                 + WEIGHT + COMORBSCORE + NSAID + NARC + P01OAGRD_Severe
                 + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                 + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + EDCV_GradDeg
                 + EDCV_SomeGrad + EDCV_UGDeg + EDCV_SomeUG 
                 + EDCV_HSDeg + V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, data=data_full)
summary(mod2)
z2 <- summary(mod2)$coefficients/summary(mod2)$standard.errors
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
exp(coef(mod2))


# - V00WTMINKG
mod3 <- multinom(EVNT ~ AGE + SEX + RACE_NW
                 + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                 + MEDINS + PASE + WOMKP + WOMSTF + BMI + HEIGHT 
                 + WEIGHT + COMORBSCORE + NSAID + NARC + P01OAGRD_Severe
                 + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                 + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + EDCV_GradDeg
                 + EDCV_SomeGrad + EDCV_UGDeg + EDCV_SomeUG 
                 + EDCV_HSDeg + V00WTMAXKG + Surg_Inj_Hist, data=data_full)
summary(mod3)
z3 <- summary(mod3)$coefficients/summary(mod3)$standard.errors
p3 <- (1 - pnorm(abs(z3), 0, 1)) * 2
exp(coef(mod3))

# Remove more to find lowest AIC - BMI - COMORBSCORE
mod4 <- multinom(EVNT ~ AGE + SEX + RACE_NW
                 + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                 + MEDINS + PASE + WOMKP + WOMSTF + HEIGHT 
                 + WEIGHT + COMORBSCORE + NSAID + NARC + P01OAGRD_Severe
                 + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                 + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + EDCV_GradDeg
                 + EDCV_SomeGrad + EDCV_UGDeg + EDCV_SomeUG 
                 + EDCV_HSDeg + V00WTMAXKG + Surg_Inj_Hist, data=data_full)
summary(mod4)
z4 <- summary(mod4)$coefficients/summary(mod4)$standard.errors
p4 <- (1 - pnorm(abs(z4), 0, 1)) * 2
exp(coef(mod4))

# Testing Interactions
mod5 <- multinom(EVNT ~ AGE + SEX + RACE_NW
                 + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                 + MEDINS + PASE + WOMKP + WOMSTF + HEIGHT + HEIGHT*WEIGHT
                 + WEIGHT + COMORBSCORE + NSAID + NARC + P01OAGRD_Severe
                 + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                 + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + EDCV_GradDeg
                 + EDCV_SomeGrad + EDCV_UGDeg + EDCV_SomeUG 
                 + EDCV_HSDeg + V00WTMAXKG + Surg_Inj_Hist, data=data_full)
summary(mod5)
z5 <- summary(mod5)$coefficients/summary(mod5)$standard.errors
p5 <- (1 - pnorm(abs(z5), 0, 1)) * 2
exp(coef(mod5))

# Checking the accuracy of created clusters compared to original clusters
library(randomForest)
trn_data <- rfImpute(data_cases[,2:35], data_cases[,36], data=data_cases)
which(is.na(trn_data), arr.ind = TRUE) # 12 NA values in baseline cases
colnames(trn_data)[1] <- "Cluster"

set.seed(1)
rf <- randomForest(as.factor(Cluster) ~  AGE + SEX + RACE_NW
                   + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                   + MEDINS + PASE + WOMADL + WOMKP + WOMSTF + BMI + HEIGHT 
                   + WEIGHT + COMORBSCORE + CESD + NSAID + NARC + P01OAGRD_Severe
                   + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                   + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + EDCV_GradDeg
                   + EDCV_SomeGrad + EDCV_UGDeg + EDCV_SomeUG 
                   + EDCV_HSDeg+ V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, 
                   data=trn_data, proximity=TRUE, localImp = TRUE)
plot(rf, log="y")
importance(rf)
varImpPlot(rf)
table(predict(rf), trn_data$Cluster)
getTree(rf, 1, labelVar = TRUE)
pred <- predict(rf, newdata = data_bl_cntrl)
summary(pred)

library(randomForestExplainer)
explain_forest(rf, interactions = TRUE, data = trn_data)
