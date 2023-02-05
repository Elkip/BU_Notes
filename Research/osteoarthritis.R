library(tidyverse)

cluster_assignments <- read.csv("/home/elkip/Datasets/Cluster_Assignments_012823.csv", header = T, sep = ",")
indv_info_clstr <- read.csv("/home/elkip/Datasets/Individ_Info_w_Clusters_012823.csv", header = T, sep = ",")

enrollees_raw <- read.csv("/home/elkip/Datasets/Enrollees.txt", header = T, sep = "|")
# numeric_col <- c(1:60)[-c(1,15)]
enrollees <- data.frame(enrollees_raw) %>% 
  mutate_all(funs(gsub(":.*", "", .))) %>%
  na_if(".") # %>% drop_na() # Which missing data to drop?

e_df <- enrollees %>%
  select(ID = ID, SEX = P02SEX, RACE = P02RACE, ETHNICITY = P02HISP)

clinical0_raw <- read.csv("/home/elkip/Datasets/AllClinical00.txt", header = T, sep = "|")
clinical0 <- data.frame(clinical0_raw) %>% 
  mutate_all(funs(gsub(":.*", "", .))) %>%
  na_if(".")

c0_df <- clinical0 %>%
  select(ID = ID, AGE = V00AGE, CEMPLOY = V00CEMPLOY, EDCV = V00EDCV, 
         MEDINS = V00MEDINS, PASE = V00PASE, P01OAGRD = P01OAGRDL, 
         P02JBMPCV_NEW  = P02JBMPCV, V00WTMAXKG = V00WTMAXKG, 
         V00WTMINKG = V00WTMINKG, BMI = P01BMI, HEIGHT = P01STFID2, 
         WEIGHT = P01STFID1, COMORBSCORE = V00COMORB, CESD = V00CESD,
         NSAID = V00RXNSAID, NARC = V00RXNARC)

data_baseline <- inner_join(e_df, c0_df, by = "ID")
