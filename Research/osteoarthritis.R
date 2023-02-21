library(tidyverse)

data_path <- "/home/elkip/Documents/BU/Research/Data/"
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

data_baseline <- inner_join(e_df, c0_df, by = "ID")  %>%
  select(ID = ID, AGE = V00AGE, SEX = SEX, CEMPLOY = V00CEMPLOY, EDCV = V00EDCV, 
         MEDINS = V00MEDINS, PASE = V00PASE, P01OAGRD = P01OAGRD,
         P02JBMPCV_NEW = P02JBMPCV_NEW, WOMADL = WOMADL, WOMKP = WOMKP, 
         WOMSTF = WOMSTF, V00WTMAXKG = V00WTMAXKG,  V00WTMINKG = V00WTMINKG, 
         BMI = P01BMI, HEIGHT = P01HEIGHT, WEIGHT = P01WEIGHT, 
         COMORBSCORE = V00COMORB, CESD = V00CESD, NSAID = V00RXNSAID, NARC = V00RXNARC,
         RACE=RACE, ETHNICITY = ETHNICITY, Surg_Inj_Hist = Surg_Inj_Hist)

data_baseline <- data_baseline %>% 
  mutate(CEMPLOY_NWOR = coalesce(if_any(CEMPLOY, `==`, 4), 0),
         CEMPLOY_NWH = coalesce(if_any(CEMPLOY, `==`, 3), 0),
         CEMPLOY_FB = coalesce(if_any(CEMPLOY, `==`, 2), 0),
         V00EDCV_GradDeg = coalesce(if_any(EDCV, `==`, 5), 0),
         V00EDCV_SomeGrad = coalesce(if_any(EDCV, `==`, 4), 0),
         V00EDCV_UGDeg = coalesce(if_any(EDCV, `==`, 3), 0),
         V00EDCV_SomeUG = coalesce(if_any(EDCV, `==`, 2), 0),
         V00EDCV_HSDeg = coalesce(if_any(EDCV, `==`, 1), 0),
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

# Attach predicted cluster to baseline case data
data_bl_cases <- data_baseline[data_baseline$ID %in% indv_info_clstr$ID,] %>%
    inner_join(indv_info_clstr[,c(1,43)], by=id)
data_bl_cntrl <- data_baseline[!(data_baseline$ID %in% indv_info_clstr$ID),]
final_data_cases <- indv_info_clstr[,c(2:36,43)]


library(randomForest)
rf <- randomForest(as.factor(RF.4.Clusters) ~  AGE + SEX + RACE_NW
                   + RACE_AA + ETHNICITY + CEMPLOY_NWOR + CEMPLOY_NWH + CEMPLOY_FB 
                   + MEDINS + PASE + WOMADL + WOMKP + WOMSTF + BMI + HEIGHT 
                   + WEIGHT + COMORBSCORE + CESD + NSAID + NARC + P01OAGRD_Severe
                   + P01OAGRD_Moderate + P01OAGRD_Mild+ P01OAGRD_Possible 
                   + P02JBMPCV_NEW_None + P02JBMPCV_NEW_One + V00EDCV_GradDeg
                   + V00EDCV_SomeGrad + V00EDCV_UGDeg + V00EDCV_SomeUG 
                   + V00EDCV_HSDeg+ V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, 
                   data=trn_data)
table(predict(rf), trn_data$RF.4.Clusters)

# Alternative with as.factor
# rf <- randomForest(as.factor(RF.4.Clusters) ~  AGE + SEX +
#                      + as.factor(RACE) + as.factor(ETHNICITY) + as.factor(CEMPLOY) + MEDINS + PASE + WOMADL
#                    + WOMKP + WOMSTF + BMI + HEIGHT + WEIGHT + COMORBSCORE + CESD
#                    + NSAID + NARC + as.factor(P01OAGRD)
#                    + as.factor(P02JBMPCV) +  + as.factor(V00EDCV)
#                    + V00WTMAXKG + V00WTMINKG + Surg_Inj_Hist, 
#                    data=trn_data, na.action = na.omit)

pred <- predict(rf, newdata = data_bl_cntrl)
summary(pred)
