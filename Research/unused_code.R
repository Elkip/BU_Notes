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