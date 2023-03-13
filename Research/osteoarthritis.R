library(tidyverse)

data_path <- "/home/elkip/Documents/BU/Research/OAI_Complete"
clstrs <- read.csv(file.path(data_path, "Cluster_Assignments_012823.csv"), header = T, sep = ",")
indv_info_clstr <- read.csv(file.path(data_path, "Individ_Info_w_Clusters_012823.csv"), header = T, sep = ",")
enrollees_raw <- read.csv(file.path(data_path, "Enrollees.txt"), header = T, sep = "|")
clinical0_raw <- read.csv(file.path(data_path, "AllClinical00.txt"), header = T, sep = "|")

# numeric_col <- c(1:60)[-c(1,15)] # Note Col 1 and 15 are non-numeric
enrollees <- data.frame(enrollees_raw) %>% 
  mutate_all(list(~gsub(":.*", "", .))) %>%
  na_if(".") # %>% drop_na() # Which missing data to drop?
e_df <- enrollees %>%
  select(ID = ID, SEX = P02SEX, RACE = P02RACE, ETHNICITY = P02HISP)
remove(enrollees_raw)

clinical0 <- data.frame(clinical0_raw) %>% 
  mutate_all(list(~gsub(":.*", "", .))) %>%
  na_if(".")
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

data_baseline <- inner_join(e_df, c0_df, by = "ID")  %>%
  select(ID = ID, AGE = V00AGE, SEX = SEX, CEMPLOY = V00CEMPLOY, EDCV = V00EDCV, 
         MEDINS = V00MEDINS, PASE = V00PASE, P01OAGRD = P01OAGRD,
         P02JBMPCV_NEW = P02JBMPCV_NEW, WOMADL = WOMADL, WOMKP = WOMKP, 
         WOMSTF = WOMSTF, V00WTMAXKG = V00WTMAXKG,  V00WTMINKG = V00WTMINKG, 
         BMI = P01BMI, HEIGHT = P01HEIGHT, WEIGHT = P01WEIGHT, 
         COMORBSCORE = V00COMORB, CESD = V00CESD, NSAID = V00RXNSAID, NARC = V00RXNARC,
         RACE=RACE, ETHNICITY = ETHNICITY, Surg_Inj_Hist = Surg_Inj_Hist)
remove(c0_df) 

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

num_col <- c(1:2, 5:15)
fac_col <- c(3, 4, 16:35)
data_baseline[,num_col] <- sapply(data_baseline[,num_col], as.numeric)
for (i in fac_col) {
  data_baseline[,i] <- sapply(data_baseline[,i], as.factor) 
}

# For each patient assign an outcome:
# 1 - No event, no death
# 2 - Left study before event
# 3 - Knee Replacement
# 4 - Death
outcomes_raw <- read.csv(file.path(data_path, "Outcomes99.txt"), header = T, sep = "|")
outcomes <- data.frame(outcomes_raw) %>% 
  mutate_all(list(~gsub(":.*", "", .))) %>%
  na_if(".") %>% 
  replace(is.na(.), 0) %>%
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
         .keep = "none") %>%
         replace(is.na(.), 0)
remove(outcomes_raw)
events <- outcomes %>% mutate(ID = as.numeric(ID), 
                              EVNT = as.factor(case_when(
                                  KNEE_OUTCM != 0 ~ 3,
                                  (LAST_CONTACT != 11 & DTH != 0) ~ 4,
                                  LAST_CONTACT != 11 ~ 2,
                                  TRUE ~ 1
                                )),
                              EVNT_VST = as.numeric(case_when(
                                  EVNT == 3 ~ KNEE_RPLC_VST,
                                  EVNT == 4 ~ DTH_VST,
                                  TRUE ~ LAST_CONTACT
                                )),
                              .keep = "none")
remove(outcomes)

nrow(events[which(events$EVNT == 4),])
nrow(events[which(events$EVNT == 3),])
nrow(events[which(events$EVNT == 2),])
nrow(events[which(events$EVNT == 1),])

# Attach predicted RF K=4 cluster ID to baseline data
data_bl_cases <- data_baseline[data_baseline$ID %in% indv_info_clstr$ID,] %>%
  full_join(indv_info_clstr[,c(2,43)], by=NULL)
data_bl_cntrl <- data_baseline[!(data_baseline$ID %in% indv_info_clstr$ID),]
data_fnl_cases <- indv_info_clstr[,c(2:36,43)]

# Create a full dataset of the time-varying predictors
data_full <- data_bl_cntrl  %>% 
    mutate(VISIT = 0, .keep = "all") %>%
    select(ID = ID, VISIT = VISIT, AGE = AGE, CEMPLOY_NWOR = CEMPLOY_NWOR,
      CEMPLOY_NWH, CEMPLOY_FB = CEMPLOY_FB, MEDINS = MEDINS, PASE = PASE, 
      WOMADL = WOMADL, WOMKP = WOMKP, WOMSTF = WOMSTF, CESD = CESD, NSAID = NSAID,  
      NARC = NARC)

# for (i in 1:11) {
for (i in c(1,3,5,6,8,10)) {
  visit_num <- if(i < 10) paste("0", i, sep="") else as.character(i)
  filename <- paste("AllClinical", visit_num, ".txt", sep="")
  clinical_raw <- read.csv(file.path(data_path, filename), header = T, sep = "|")
  
  # Define columns names
  vage <- paste("V", visit_num, "AGE", sep = "")
  vcemploy <- paste("V", visit_num, "CEMPLOY", sep = "")
  vcesd <- paste("V", visit_num, "CESD", sep = "")
  vmedins <- paste("V", visit_num, "MEDINS", sep = "")
  vnsaid <- paste("V", visit_num, "RXNSAID", sep = "")
  vnarc <- paste("V", visit_num, "RXNARC", sep = "")
  vpase <- paste("V", visit_num, "PASE", sep = "")
  vwomadll <- paste("V", visit_num, "WOMADLL", sep = "")
  vwomadlr <- paste("V", visit_num, "WOMADLR", sep = "")
  vwomkpl <- paste("V", visit_num, "WOMKPL", sep = "")
  vwomkpr <- paste("V", visit_num, "WOMKPR", sep = "")
  vwomstfl <- paste("V", visit_num, "WOMSTFL", sep = "")
  vwomstfr <- paste("V", visit_num, "WOMSTFR", sep = "")
  
  clinical <- data.frame(clinical_raw) %>% 
    mutate_all(list(~gsub(":.*", "", .))) %>%
    na_if(".")
  c_df <- clinical  %>%
    mutate(VISIT = as.numeric(visit_num), 
           WOMADL = pmax(get(vwomadll), get(vwomadlr)),
           WOMKP = pmax(get(vwomkpl), get(vwomkpr)),
           WOMSTF = pmax(get(vwomstfl), get(vwomstfr)),
           .keep = "all") %>%
    mutate(CEMPLOY_NWOR = coalesce(if_any(vcemploy, `==`, 4), 0),
           CEMPLOY_NWH = coalesce(if_any(vcemploy, `==`, 3), 0),
           CEMPLOY_FB = coalesce(if_any(vcemploy, `==`, 2), 0), .keep = "all") %>%
    select(ID = ID, VISIT = VISIT, AGE = vage, MEDINS = vmedins, 
           PASE = vpase, WOMADL = WOMADL, WOMKP = WOMKP, WOMSTF = WOMSTF, 
           CESD = vcesd, NSAID = vnsaid,  NARC = vnarc, CEMPLOY_NWOR = CEMPLOY_NWOR, 
           CEMPLOY_NWH = CEMPLOY_NWH, CEMPLOY_FB = CEMPLOY_FB)
  
  # Drop IDs that have already had an event of interest
  censored <- events[which(events$EVNT_VST < i),]
  c_df <- c_df[!(c_df$ID %in% censored$ID),]
  data_full <- data_full %>% rbind(c_df)
}

# Multinomial Distribution with event as the outcome
# library(mlogit)
# m_df <- dfidx(data_full, choice="EVNT", shape="wide")
# ml <- mlogit(EVNT ~  1 | AGE + SEX + RACE_NW
#                           + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB
#                           + MEDINS + PASE + WOMADL + WOMKP + WOMSTF + BMI + HEIGHT
#                           + WEIGHT + COMORBSCORE + CESD + NSAID + NARC + P01OAGRD_Severe
#                           + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible
#                           + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + V00EDCV_GradDeg
#                           + V00EDCV_SomeGrad + V00EDCV_UGDeg + V00EDCV_SomeUG
#                           + V00EDCV_HSDeg+ V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, data=m_df)
# summary(ml)

library(nnet)
mod1 <- multinom(EVNT ~ AGE + SEX + RACE_NW
                 + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                 + MEDINS + PASE + WOMADL + WOMKP + WOMSTF + BMI + HEIGHT 
                 + WEIGHT + COMORBSCORE + CESD + NSAID + NARC + P01OAGRD_Severe
                 + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                 + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + V00EDCV_GradDeg
                 + V00EDCV_SomeGrad + V00EDCV_UGDeg + V00EDCV_SomeUG 
                 + V00EDCV_HSDeg+ V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, data=data_full)
summary(mod1)

# Checking the accuracy of created clusters compared to orignial clusters
library(randomForest)
trn_data <- rfImpute(data_bl_cases[,2:35], data_bl_cases[,36], data=data_bl_cases)
which(is.na(trn_data), arr.ind = TRUE) # 12 NA values in baseline cases
colnames(trn_data)[1] <- "Cluster"

set.seed(1)
rf <- randomForest(as.factor(Cluster) ~  AGE + SEX + RACE_NW
                   + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                   + MEDINS + PASE + WOMADL + WOMKP + WOMSTF + BMI + HEIGHT 
                   + WEIGHT + COMORBSCORE + CESD + NSAID + NARC + P01OAGRD_Severe
                   + P01OAGRD_Moderate + P01OAGRD_Mild + P01OAGRD_Possible 
                   + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + V00EDCV_GradDeg
                   + V00EDCV_SomeGrad + V00EDCV_UGDeg + V00EDCV_SomeUG 
                   + V00EDCV_HSDeg+ V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, 
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