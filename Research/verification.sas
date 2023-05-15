* Written by R;
*  write.foreign(data_full, paste(data_path, "full_data.txt", sep = ""),  ;

PROC FORMAT;
value SEX 
     1 = "1" 
     2 = "2" 
;

value MEDINS 
     1 = "0" 
     2 = "1" 
;

value NSAID 
     1 = "0" 
     2 = "1" 
;

value NARC 
     1 = "0" 
     2 = "1" 
;

value ETHNICIT 
     1 = "0" 
     2 = "1" 
;

value Srg_In_H 
     1 = "0" 
     2 = "1" 
;

value EDCV_GrD 
     1 = "0" 
     2 = "1" 
;

value EDCV_UGD 
     1 = "0" 
     2 = "1" 
;

value EDCV_HSD 
     1 = "0" 
     2 = "1" 
;

value GRD_Sevr 
     1 = "0" 
     2 = "1" 
;

value GRD_Mdrt 
     1 = "0" 
     2 = "1" 
;

value GRD_Mild 
     1 = "0" 
     2 = "1" 
;

value GRD_Pssb 
     1 = "0" 
     2 = "1" 
;

value BMP_None 
     1 = "0" 
     2 = "1" 
;

value BMP_One 
     1 = "0" 
     2 = "1" 
;

value DPRSD 
     1 = "0" 
     2 = "1" 
;

value EVNT 
     1 = "1" 
     2 = "2" 
     3 = "3" 
     4 = "4" 
     5 = "5" 
     6 = "6" 
     7 = "7" 
     8 = "8" 
;

value CEMP_NW 
     1 = "0" 
     2 = "1" 
;

value RACE_O 
     1 = "0" 
     2 = "1" 
;

DATA  knees ;
INFILE  "/home/elkip/Documents/BU/Research/OAI_Complete/dataK.5.Clusters.txt" 
     DSD 
     LRECL= 102 ;
INPUT
 ID AGE SEX MEDINS PASE WOMADL WOMKP WOMSTF V00WTMAXKG V00WTMINKG BMI
 HEIGHT WEIGHT COMORBSCORE CESD NSAID NARC ETHNICITY Surg_Inj_Hist
 EDCV_GradDeg EDCV_UGDeg EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild 
 GRD_Possible BMP_None BMP_One DPRSD EVNT EVNT_VST CEMP_NW RACE_O
;
FORMAT SEX SEX. ;
FORMAT MEDINS MEDINS. ;
FORMAT NSAID NSAID. ;
FORMAT NARC NARC. ;
FORMAT ETHNICITY ETHNICIT. ;
FORMAT Surg_Inj_Hist Srg_In_H. ;
FORMAT EDCV_GradDeg EDCV_GrD. ;
FORMAT EDCV_UGDeg EDCV_UGD. ;
FORMAT EDCV_HSDeg EDCV_HSD. ;
FORMAT GRD_Severe GRD_Sevr. ;
FORMAT GRD_Moderate GRD_Mdrt. ;
FORMAT GRD_Mild GRD_Mild. ;
FORMAT GRD_Possible GRD_Pssb. ;
FORMAT BMP_None BMP_None. ;
FORMAT BMP_One BMP_One. ;
FORMAT DPRSD DPRSD. ;
FORMAT EVNT EVNT. ;
FORMAT CEMP_NW CEMP_NW. ;
FORMAT RACE_O RACE_O. ;
RUN;


* Only best predictors / Remove missing data;
data knees_best;
     set knees;
     drop MEDINS WOMDL V00WTMINKG COMORBSCORE NARC ETHNICITY BMP_One BMP_None DPRSD;
     if cmiss(of _all_) then delete;
run;

* Full Main Effects Model;
PROC LOGISTIC data=knees;
     class SEX NSAID NARC ETHNICITY Surg_Inj_Hist
          CEMP_NW EDCV_GradDeg EDCV_SomeGrad EDCV_UGDeg
          EDCV_SomeUG EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          BMP_None BMP_One RACE_O EVNT(ref="1") / param = ref;
     model EVNT = AGE SEX MEDINS PASE WOMADL WOMKP WOMSTF V00WTMAXKG V00WTMINKG
          BMI HEIGHT WEIGHT COMORBSCORE CESD NSAID NARC ETHNICITY Surg_Inj_Hist
          CEMP_NW EDCV_GradDeg EDCV_UGDeg EDCV_HSDeg GRD_Severe GRD_Moderate 
          GRD_Mild GRD_Possible BMP_None BMP_One RACE_O / link = glogit;
run;

* Best model choosen through stepwise selection;
PROC LOGISTIC data=knees_best;
     class SEX(ref="1") NSAID(ref="0") EDCV_GradDeg(ref="0") 
          EDCV_UGDeg(ref="0") EDCV_HSDeg(ref="0") GRD_Severe(ref="0") GRD_Moderate(ref="0") 
          GRD_Mild(ref="0") GRD_Possible(ref="0") RACE_O(ref="0") EVNT(ref="1");
     model EVNT = AGE SEX PASE WOMKP WOMSTF V00WTMAXKG 
          WEIGHT | HEIGHT CESD NSAID EDCV_GradDeg EDCV_UGDeg
          EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          RACE_O Surg_Inj_Hist / link = glogit clodds=both;
run;

* Break down models;
title1 'Drop out vs. No Outcome';
PROC LOGISTIC data=knees;
     class SEX(ref="1") NSAID(ref="0") EDCV_GradDeg(ref="0") 
          EDCV_UGDeg(ref="0") EDCV_HSDeg(ref="0") GRD_Severe(ref="0") GRD_Moderate(ref="0") 
          GRD_Mild(ref="0") GRD_Possible(ref="0") RACE_O(ref="0") EVNT(ref="1");
     model EVNT = AGE SEX PASE WOMKP WOMSTF V00WTMAXKG 
          HEIGHT | WEIGHT CESD NSAID EDCV_GradDeg EDCV_UGDeg
          EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          RACE_O Surg_Inj_Hist /alpha=.1 firth clodds=wald link=logit;
     WHERE EVNT in (1,2);
run;quit;

title1 'Death vs. No Outcome';
PROC LOGISTIC data=knees;
     class SEX(ref="1") NSAID(ref="0") EDCV_GradDeg(ref="0") 
          EDCV_UGDeg(ref="0") EDCV_HSDeg(ref="0") GRD_Severe(ref="0") GRD_Moderate(ref="0") 
          GRD_Mild(ref="0") GRD_Possible(ref="0") RACE_O(ref="0") EVNT(ref="1");
     model EVNT = AGE SEX PASE WOMKP WOMSTF V00WTMAXKG 
          HEIGHT | WEIGHT CESD NSAID EDCV_GradDeg EDCV_UGDeg
          EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          RACE_O Surg_Inj_Hist / alpha=.1 firth clodds=wald link=logit;
     WHERE EVNT in (1,3);
run;quit;

title1 'Knee Cluster 1 vs. No Outcome';
PROC LOGISTIC data=knees;
     class SEX(ref="1") NSAID(ref="0") EDCV_GradDeg(ref="0") 
          EDCV_UGDeg(ref="0") EDCV_HSDeg(ref="0") GRD_Severe(ref="0") GRD_Moderate(ref="0") 
          GRD_Mild(ref="0") GRD_Possible(ref="0") RACE_O(ref="0") EVNT(ref="1");
     model EVNT = AGE SEX PASE WOMKP WOMSTF V00WTMAXKG 
          HEIGHT | WEIGHT CESD NSAID EDCV_GradDeg EDCV_UGDeg
          EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          RACE_O Surg_Inj_Hist / alpha=.1 firth clodds=wald link=logit;
     WHERE EVNT in (1,4);
run;quit;

title1 'Knee Cluster 2 vs. No Outcome';
PROC LOGISTIC data=knees;
     class SEX(ref="1") NSAID(ref="0") EDCV_GradDeg(ref="0") 
          EDCV_UGDeg(ref="0") EDCV_HSDeg(ref="0") GRD_Severe(ref="0") GRD_Moderate(ref="0") 
          GRD_Mild(ref="0") GRD_Possible(ref="0") RACE_O(ref="0") EVNT(ref="1");
     model EVNT = AGE SEX PASE WOMKP WOMSTF V00WTMAXKG 
          HEIGHT | WEIGHT CESD NSAID EDCV_GradDeg EDCV_UGDeg
          EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          RACE_O Surg_Inj_Hist / alpha=.1 firth clodds=wald link=logit;
     WHERE EVNT in (1,5);
run;quit;

title1 'Knee Cluster 3 vs. No Outcome';
PROC LOGISTIC data=knees;
     class SEX(ref="1") NSAID(ref="0") EDCV_GradDeg(ref="0") 
          EDCV_UGDeg(ref="0") EDCV_HSDeg(ref="0") GRD_Severe(ref="0") GRD_Moderate(ref="0") 
          GRD_Mild(ref="0") GRD_Possible(ref="0") RACE_O(ref="0") EVNT(ref="1");
     model EVNT = AGE SEX PASE WOMKP WOMSTF V00WTMAXKG 
          HEIGHT | WEIGHT CESD NSAID EDCV_GradDeg EDCV_UGDeg
          EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          RACE_O Surg_Inj_Hist / alpha=.1 firth clodds=wald link=logit;
     WHERE EVNT in (1,6);
run;quit;

title1 'Knee Cluster 4 vs. No Outcome';
PROC LOGISTIC data=knees;
     class SEX(ref="1") NSAID(ref="0") EDCV_GradDeg(ref="0") 
          EDCV_UGDeg(ref="0") EDCV_HSDeg(ref="0") GRD_Severe(ref="0") GRD_Moderate(ref="0") 
          GRD_Mild(ref="0") GRD_Possible(ref="0") RACE_O(ref="0") EVNT(ref="1");
     model EVNT = AGE SEX PASE WOMKP WOMSTF V00WTMAXKG 
          HEIGHT | WEIGHT CESD NSAID EDCV_GradDeg EDCV_UGDeg
          EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          RACE_O Surg_Inj_Hist / alpha=.1 firth clodds=wald link=logit;
     WHERE EVNT in (1,7);
run;quit;

title1 'Knee Cluster 5 vs. No Outcome';
PROC LOGISTIC data=knees;
     class SEX(ref="1") NSAID(ref="0") EDCV_GradDeg(ref="0") 
          EDCV_UGDeg(ref="0") EDCV_HSDeg(ref="0") GRD_Severe(ref="0") GRD_Moderate(ref="0") 
          GRD_Mild(ref="0") GRD_Possible(ref="0") RACE_O(ref="0") EVNT(ref="1");
     model EVNT = AGE SEX PASE WOMKP WOMSTF V00WTMAXKG 
          HEIGHT | WEIGHT CESD NSAID EDCV_GradDeg EDCV_UGDeg
          EDCV_HSDeg GRD_Severe GRD_Moderate GRD_Mild GRD_Possible
          RACE_O Surg_Inj_Hist / alpha=.1 firth clodds=wald link=logit;
     WHERE EVNT in (1,8);
run;quit;
