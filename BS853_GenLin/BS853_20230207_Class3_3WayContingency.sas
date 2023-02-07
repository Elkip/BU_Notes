   /*soh************************************************************************
   Boston University - Biostatistics Department
   PROGRAM NAME          : script - Lecture 3.sas
                          (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 3)
   PROJECT NAME          : Generalized Linear Models (BS853) 
                           - Lecture: LogLinear Models in SxR and three way tables

   DESCRIPTION           : 1. Read data
                           2. Loglinear Models for SxR tables and sets of SxR tables
                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.1
   INFRASTRUCTURE        : Windows 
   INPUT                 : 
   OUTPUT                : 
  -----------------------------------------------------------------------------
   Author : Gheorghe Doros 
   Last modified 01/27/2023               
 **eoh************************************************************************/


/* Sets of 2x2 table*/
/* Care and Infant survival in 2 clinics*/
options ps=60 ls=89 pageno=1 nodate;

title1 "Care and Infant survival in 2 clinics";
data care;
input clinic survival $ count care;
cards;
1 died	    3	0
1 died      4	1
1 lived	    176	0
1 lived     293	1
2 died	    17	0
2 died      2	1
2 lived	    196	0
2 lived     23	1
run;
proc freq data=care;
  table clinic*survival*care/cmh chisq relrisk;
  weight count;
run;

/* Data on Prevalence of a disease */
data Prev;
   input N P size$ age;
   ln = log(N);
   datalines;
450   39  small  1
1500  50  medium 1
67    1   large  1
457   70  small  2
630   95  medium 2
250   19  large  2
;
run;


/******************************************/
/*** Clinic, Care and Infant mortatlity ***/
/******************************************/

Title1 'Clinic, Care and Infant mortatlity';
data infmort; 
input Clinic Care Death Count; 
datalines; 
1 1 1 3
1 2 1 4
1 1 2 176
1 2 2 293
2 1 1 17
2 2 1 2
2 1 2 197
2 2 2 23
; 
run;

options ps=60 ls=89 pageno=1 nodate;

title2 'Model 1 saturated model';
ods select ModelFit;
proc genmod data=infmort; 
  class Clinic Care Death;
  model count=clinic|care|death/dist=poisson obstats type3; 
run;
title2 'Model 2 - all two way interactions';
ods select ModelFit;
proc genmod data=infmort; 
  class Clinic Care Death;
  model count=clinic|care clinic|death death|care/dist=poisson obstats type3; 
run;
title2 'Model 3 - conditional independence of Care and Clinic';
ods select ModelFit;
proc genmod data=infmort; 
  class Clinic Care Death;
  model count=clinic|death death|care/dist=poisson obstats type3; 
run;
title2 'Model 4 - conditional independence of Survival and Clinic';
ods select ModelFit;
proc genmod data=infmort; 
  class Clinic Care Death;
  model count=clinic|care death|care/dist=poisson obstats type3; 
run;
title2 'Model 5 - conditional independence of Survival and Care';
ods select ModelFit;
proc genmod data=infmort; 
  class Clinic Care Death;
  model count=clinic|care clinic|death/dist=poisson obstats type3; 
run;
title2 'Model 6 - joint independence of (Care and Survival) from Clinic';
ods select ModelFit;
proc genmod data=infmort; 
  class Clinic Care Death;
  model count=clinic death|care/dist=poisson obstats type3; 
run;
title2 'Model 7 - Mutual independence of Care, Clinic, and Survival';
ods select ModelFit;
proc genmod data=infmort; 
  class Clinic Care Death;
  model count=clinic death care/dist=poisson obstats type3; 
run;

/**************************************************************************************************/
/* Association among residence, income and satisfaction with total hip replacement (THR)          */
/**************************************************************************************************/

title1 'Association among residence, income and satisfaction with total hip replacement (THR)';
data THR; 
input Rural Income Satisf Count; 
datalines; 
1 1 1 48
1 1 2 12
1 2 1 96
1 2 2 94
2 1 1 55
2 1 2 135
2 2 1 7 
2 2 2 53
run;

options ps=60 ls=89 pageno=1 nodate;

title2 'Model 1 saturated model';
ods select ModelFit;
proc genmod data=THR; 
  class Rural Income Satisf;
  model count=Rural|Income|Satisf/dist=poisson obstats type3; 
run;
title2 'Model 2 - all two way interactions';
ods select ModelFit;
proc genmod data=THR; 
  class Rural Income Satisf;
  model count=Rural|Income Rural|Satisf Satisf|Income/dist=poisson obstats type3; 
run;
title2 'Model 3 - conditional independence of Income and Rural';
ods select ModelFit;
proc genmod data=THR; 
  class Rural Income Satisf;
  model count=Rural|Satisf Satisf|Income/dist=poisson obstats type3; 
run;
title2 'Model 4 - conditional independence of Satisfaction and Rural';
ods select ModelFit;
proc genmod data=THR; 
  class Rural Income Satisf;
  model count=Rural|Income Satisf|Income/dist=poisson obstats type3; 
run;
title2 'Model 5 - conditional independence of Satisfaction and Income';
ods select ModelFit;
proc genmod data=THR; 
  class Rural Income Satisf;
  model count=Rural|Income Rural|Satisf/dist=poisson obstats type3; 
run;
title2 'Model 6 - joint independence of (Income and Satisfaction) from Rural';
ods select ModelFit;
proc genmod data=THR; 
  class Rural Income Satisf;
  model count=Rural Satisf|Income/dist=poisson obstats type3; 
run;
title2 'Model 7 - joint independence of (Income and Rural) from Satisfaction ';
ods select ModelFit;
proc genmod data=THR; 
  class Rural Income Satisf;
  model count=Rural|Income Satisf /dist=poisson obstats type3; 
run;
title2 'Model 8 - joint independence of (Rural and Satisfaction) from Income ';
ods select ModelFit;
proc genmod data=THR; 
  class Rural Income Satisf;
  model count=Rural|Satisf Income/dist=poisson obstats type3; 
run;
title2 'Model 9 - Mutual independence of Income, Rural, and Satisfaction';
ods select ModelFit;
proc genmod data=THR; 
  class Rural Income Satisf;
  model count=Rural Satisf Income/dist=poisson obstats type3; 
run;
