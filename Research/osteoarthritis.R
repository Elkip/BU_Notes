library(tidyverse)

data_path <- "/home/elkip/Documents/BU/Research/OAI_Complete/" # Sys.getenv("OAI_DATA")

getBaselineData <- function(path) {
  print("Loading Files...")
  enrollees_raw <- read.csv(file.path(path, "Enrollees.txt"), header = T, sep = "|")
  clinical0_raw <- read.csv(file.path(path, "AllClinical00.txt"), header = T, sep = "|")
  
  print("Formatting Enrollees Data...")
  enrollees <- data.frame(enrollees_raw) %>% 
    mutate_all(list(~gsub(":.*", "", .))) %>%
    mutate_all(list(~na_if(., "")))
  
  e_df <- enrollees %>%
    select(ID = ID, SEX = P02SEX, RACE = P02RACE, ETHNICITY = P02HISP)
  remove(enrollees_raw)
  
  print("Formatting Clinical 0 Data...")
  clinical0 <- data.frame(clinical0_raw) %>% 
    mutate_all(list(~gsub(":.*", "", .))) %>%
    mutate_all(list(~na_if(., "")))
  
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
  print("Loading Complete!")
  return(data_baseline)
}

start <- getBaselineData(data_path)

# For each patient assign an outcome:
# 1: No event, no death
# 2: Left study before event
# 3: Death
# 4 - 8: Five-Level Random Forest Clusters
getEvents <- function(path) {
  outcomes_raw <- read.csv(file.path(path, "Outcomes99.txt"), header = T, sep = "|")
  outcomes <- data.frame(outcomes_raw) %>% 
    mutate_all(list(~gsub(":.*", "", .))) %>%
    mutate_all(list(~na_if(., ""))) %>% 
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
  return(events)
}

end <- getEvents(data_path)

nrow(end[which(end$EVNT == 4),])
nrow(end[which(end$EVNT == 3),])
nrow(end[which(end$EVNT == 2),])
nrow(end[which(end$EVNT == 1),])

# Attach predicted RF K=4 cluster ID to baseline data
clstrs <- read.csv(file.path(path, "Cluster_Assignments_012823.csv"), header = T, sep = ",")
indv_info_clstr <- read.csv(file.path(path, "Individ_Info_w_Clusters_012823.csv"), header = T, sep = ",")
data_bl_cases <- data_baseline[data_baseline$ID %in% indv_info_clstr$ID,] %>%
  full_join(indv_info_clstr[,c(2,43)], by=NULL)
data_bl_cntrl <- data_baseline[!(data_baseline$ID %in% indv_info_clstr$ID),]
data_fnl_cases <- indv_info_clstr[,c(2:36,43)]

# Multinomial Distribution with event as the outcome
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

# Checking the accuracy of created clusters compared to original clusters
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
