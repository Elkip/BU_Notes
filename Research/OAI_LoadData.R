require(tidyverse)

getBaselineData <- function(path) {
    print("Loading baseline data...")
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
        select(ID = ID, AGE = V00AGE, SEX = SEX, CEMPLOY = V00CEMPLOY, EDCV = V00EDCV,  MEDINS = V00MEDINS, PASE = V00PASE, P01OAGRD = P01OAGRD, P02JBMPCV_NEW = P02JBMPCV_NEW, WOMADL = WOMADL, WOMKP = WOMKP, WOMSTF = WOMSTF, V00WTMAXKG = V00WTMAXKG, V00WTMINKG = V00WTMINKG,BMI = P01BMI, HEIGHT = P01HEIGHT, WEIGHT = P01WEIGHT, COMORBSCORE = V00COMORB, CESD = V00CESD, NSAID = V00RXNSAID, NARC = V00RXNARC, RACE=RACE, ETHNICITY = ETHNICITY, Surg_Inj_Hist = Surg_Inj_Hist)
    remove(c0_df) 
    
    print("Converting numeric column type...")
    num_col <-  c("ID", "AGE", "PASE", "WOMADL", "WOMKP", "WOMSTF", "V00WTMAXKG", 
                  "V00WTMINKG", "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", "CESD")
    data_baseline[,num_col] <- sapply(data_baseline[,num_col], as.numeric)
    
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
        select(-c(CEMPLOY, EDCV, P01OAGRD, P02JBMPCV_NEW, RACE)) 
    
    print("Converting factor column types...")
    fac_col <-  c("SEX", "MEDINS", "DPRSD", "NSAID", "NARC", "ETHNICITY", 
                  "Surg_Inj_Hist", "CEMPLOY_NWOR", "CEMPLOY_NWH", "CEMPLOY_FB", 
                  "EDCV_GradDeg", "EDCV_SomeGrad", "EDCV_UGDeg", "EDCV_SomeUG", 
                  "EDCV_HSDeg", "P01OAGRD_Severe", "P01OAGRD_Moderate", 
                  "P01OAGRD_Mild", "P01OAGRD_Possible", "P02JBMPCV_NEW_None", 
                  "P02JBMPCV_NEW_One", "RACE_AA", "RACE_NW")
    for (i in fac_col) {
        data_baseline[,i] <- sapply(data_baseline[,i], as.factor) 
    }
    # These factors choose the incorrect reference
    data_baseline$P01OAGRD_Severe <- relevel(data_baseline$P01OAGRD_Severe, ref = "0")
    data_baseline$EDCV_GradDeg <- relevel(data_baseline$EDCV_GradDeg, ref = "0")
    data_baseline$Surg_Inj_Hist <- relevel(data_baseline$Surg_Inj_Hist, ref = "0")
    data_baseline$MEDINS <- relevel(data_baseline$MEDINS, ref = "0")
    
    print("Baseline Data Loading Complete")
    return(data_baseline)
}

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
    print("Loading events...")
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
                                  EVNT_VST = as.numeric(case_when(
                                      EVNT == 3 ~ KNEE_RPLC_VST,
                                      EVNT == 4 ~ DTH_VST,
                                      TRUE ~ LAST_CONTACT
                                  )),
                                  .keep = "none")
    print("Event Data Loading Complete")
    return(events)
}

# Attach predicted RF cluster ID to baseline data
getCompleteData <- function(path, cluster, exportSAS = FALSE) {
    print(paste("Loading Data with ", cluster, "...", sep = ""))
    clstrs_bsln_info <- read.csv(file.path(path, "OAI_Clust_Assignments_w_info_V5.csv"), header = T, sep = ",")
    
    # Attach predicted RF K=5 cluster ID to baseline data
    clstrs <- clstrs_bsln_info[,c("ID",cluster)]
    
    # Create the knee replacement event represented by 4 through 8
    clstrs[,cluster] <- clstrs[,cluster] - 1 # Start first cluster at 0
    
    complete_data <- bsln %>% full_join(evnt, by = NULL) %>%
        left_join(clstrs, by = NULL)
    
    complete_data[,cluster] <- complete_data[,cluster] %>% 
        replace_na(0)
    
    complete_data$EVNT <- as.factor(rowSums(cbind(as.numeric(complete_data$EVNT), 
                                        as.numeric(complete_data[,cluster]))))
    
    complete_data <- complete_data %>% select(-cluster)
    
    print("Combining/Dropping binary columns...")
    # Remove SomeGrad and SomeUG
    complete_data <- complete_data %>% 
        mutate(EDCV_UGDeg = as.factor((as.numeric(complete_data$EDCV_UGDeg)-1) + 
                                          (as.numeric(complete_data$EDCV_SomeGrad)-1))) %>% 
        mutate(EDCV_HSDeg = as.factor((as.numeric(complete_data$EDCV_HSDeg)-1) + 
                                          (as.numeric(complete_data$EDCV_SomeUG)-1))) %>%
        select(-c(EDCV_SomeUG, EDCV_SomeGrad))
    
    # Simplify working column
    complete_data <- complete_data %>% 
        mutate(CEMPLOY_NW = as.factor((as.numeric(complete_data$CEMPLOY_NWOR) - 1) + 
                                          (as.numeric(complete_data$CEMPLOY_NWH) - 1))) %>%
        select(-c(CEMPLOY_FB, CEMPLOY_NWH, CEMPLOY_NWOR))
    
    # Combine Race Categories into other non-white
    complete_data <- complete_data %>% 
      mutate(RACE_O = as.factor((as.numeric(complete_data$RACE_NW) - 1) + 
                                      (as.numeric(complete_data$RACE_AA) - 1))) %>%
      select(-c(RACE_NW, RACE_AA))
    
    print("Complete Data with Clusters Loaded")
    
    if (exportSAS) {
      print("Exporting Data to SAS format...")
      
      library(foreign)
      cnames_all <- c("ID", "AGE", "SEX", "MEDINS", "PASE", "WOMADL", "WOMKP", 
                          "WOMSTF", "V00WTMAXKG", "V00WTMINKG", "BMI", "HEIGHT", 
                          "WEIGHT", "COMORBSCORE", "CESD", "NSAID", "NARC", 
                          "ETHNICITY", "Surg_Inj_Hist", "EDCV_GradDeg",  
                          "EDCV_UGDeg", "EDCV_HSDeg", "P01OAGRD_Severe", 
                          "P01OAGRD_Moderate", "P01OAGRD_Mild", "P01OAGRD_Possible", 
                          "P02JBMPCV_NEW_None", "P02JBMPCV_NEW_One", 
                          "RACE_AA", "RACE_NW", "DPRSD","EVNT", "EVNT_VST", "CEMPLOY_NW")
      
      # rename columns to be 8 characters
      names(complete_data) <- c("ID", "AGE", "SEX", "MEDINS", "PASE", "WOMADL", 
                                "WOMKP", "WOMSTF", "V00WTMAXKG", "V00WTMINKG", 
                                "BMI", "HEIGHT", "WEIGHT", "COMORBSCORE", "CESD", 
                                "NSAID", "NARC", "ETHNICITY", "Surg_Inj_Hist", 
                                "EDCV_GradDeg", "EDCV_UGDeg", "EDCV_HSDeg", 
                                "GRD_Severe", "GRD_Moderate", "GRD_Mild", 
                                "GRD_Possible", "BMP_None", "BMP_One", 
                                "RACE_AA", "RACE_NW", "DPRSD","EVNT", "EVNT_VST", "CEMP_NW")
      
      write.foreign(complete_data, paste(DATAPATH, "data", cluster, ".txt", sep=""), 
                    paste(DATAPATH, "load_data", cluster, ".sas", sep=""), package = "SAS")
      
      # Rename to normal
      names(complete_data) <- cnames_all
      print("Data exported")
    }
    
    return(complete_data)
}