* Written by R;
*  write.foreign(data_full, paste(data_path, "full_data.txt", sep = ""),  ;

PROC FORMAT;
value SEX 
     1 = "1" 
     2 = "2";
value MEDINS 
     1 = "1" 
     2 = "0";
value NSAID 
     1 = "0" 
     2 = "1";
value NARC 
     1 = "0" 
     2 = "1";
value ETHNICIT 
     1 = "0" 
     2 = "1";
value Srg_In_H 
     1 = "1" 
     2 = "0";
value CEMP_NWO 
     1 = "0" 
     2 = "1";
value CEMP_NWH 
     1 = "0" 
     2 = "1";
value CEMP_FB 
     1 = "0" 
     2 = "1";
value EDCV_GrD 
     1 = "1" 
     2 = "0";
value EDCV_SmG 
     1 = "0" 
     2 = "1";
value EDCV_UGD 
     1 = "0" 
     2 = "1";
value EDCV_SUG 
     1 = "0" 
     2 = "1";
value EDCV_HSD 
     1 = "0"
     2 = "1";
value GRD_Sevr 
     1 = "1"
     2 = "0";
value GRD_Mdrt 
     1 = "0" 
     2 = "1";
value GRD_Mild 
     1 = "0"
     2 = "1";
value GRD_Pssb 
     1 = "0"
     2 = "1";
value BMP_None 
     1 = "1"
     2 = "0";
value BMP_One 
     1 = "0"
     2 = "1";
value RACE_AA 
     1 = "0"
     2 = "1";
value RACE_NW 
     1 = "0"
     2 = "1";

DATA  knees;
LENGTH
 EVNT_VST $ 2;

INFILE  "/home/elkip/Documents/BU/Research/OAI_Complete/full_data.txt" 
     DSD 
     LRECL= 112 ;
INPUT ID AGE SEX MEDINS PASE WOMADL WOMKP WOMSTF V00WTMAXKG V00WTMINKG
 BMI HEIGHT WEIGHT COMORBSCORE CESD NSAID NARC ETHNICITY Surg_Inj_Hist
 CEMP_NWOR CEMP_NWH CEMP_FB EDCV_GradDeg EDCV_SomeGrad EDCV_UGDeg
 EDCV_SomeUG EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
 BMP_None BMP_One RACE_AA RACE_NW EVNT EVNT_VST $ 
;
FORMAT SEX SEX. ;
FORMAT MEDINS MEDINS. ;
FORMAT NSAID NSAID. ;
FORMAT NARC NARC. ;
FORMAT ETHNICITY ETHNICIT. ;
FORMAT Surg_Inj_Hist Srg_In_H. ;
FORMAT CEMP_NWOR CEMP_NWO. ;
FORMAT CEMP_NWH CEMP_NWH. ;
FORMAT CEMP_FB CEMP_FB. ;
FORMAT EDCV_GradDeg EDCV_GrD. ;
FORMAT EDCV_SomeGrad EDCV_SmG. ;
FORMAT EDCV_UGDeg EDCV_UGD. ;
FORMAT EDCV_SomeUG EDCV_SUG. ;
FORMAT EDCV_HSDeg EDCV_HSD. ;
FORMAT GRD_Severe GRD_Sevr. ;
FORMAT GRD_Moderate GRD_Mdrt. ;
FORMAT GRD_Mild GRD_Mild. ;
FORMAT GRD_Possible GRD_Pssb. ;
FORMAT BMP_None BMP_None. ;
FORMAT BMP_One BMP_One. ;
FORMAT RACE_AA RACE_AA. ;
FORMAT RACE_NW RACE_NW. ;
RUN;

*Remove missing data;
data knees;
     set knees;
     if cmiss(of _all_) then delete;
run;

PROC LOGISTIC data=knees;
     class SEX NSAID NARC ETHNICITY Surg_Inj_Hist
          CEMP_NWOR CEMP_NWH CEMP_FB EDCV_GradDeg EDCV_SomeGrad EDCV_UGDeg
          EDCV_SomeUG EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          BMP_None BMP_One RACE_AA RACE_NW EVNT(ref="1") / param = ref;
     model EVNT = AGE SEX MEDINS PASE WOMADL WOMKP WOMSTF V00WTMAXKG V00WTMINKG
          BMI HEIGHT WEIGHT COMORBSCORE CESD NSAID NARC ETHNICITY Surg_Inj_Hist
          CEMP_NWOR CEMP_NWH CEMP_FB EDCV_GradDeg EDCV_SomeGrad EDCV_UGDeg
          EDCV_SomeUG EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          BMP_None BMP_One RACE_AA RACE_NW / link = glogit;
run;

/* Best model choosen through AIC stepwise selection */
PROC LOGISTIC data=knees;
     class SEX(ref="1") NSAID(ref="0") CEMP_NWH(ref="0") EDCV_GradDeg(ref="0") 
          EDCV_UGDeg(ref="0") GRD_Severe(ref="0") GRD_Moderate(ref="0") 
          GRD_Mild(ref="0") GRD_Possible(ref="0") RACE_AA(ref="0") EVNT(ref="1") / param = ref;
     model EVNT = AGE SEX PASE WOMKP WOMSTF V00WTMAXKG 
          BMI WEIGHT CESD NSAID CEMP_NWH EDCV_GradDeg EDCV_UGDeg
          EDCV_SomeUG GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          RACE_AA / link = glogit;
run;