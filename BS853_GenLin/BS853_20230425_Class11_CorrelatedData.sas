 /*soh************************************************************************                                                          
   Boston University - Biostatistics Department                                                                                         
   PROGRAM NAME          : script - Lecture 11.sas                                                                                      
                           (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 11)                                       
   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
                           - Lecture: Generalized Estimating Equations                                                                  
                                                                                                                                        
   DESCRIPTION           : 1. Read data                                                                                                 
                           2. GEE                                                                                                       
                                                                                                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3                                                                                              
   INFRASTRUCTURE        : Windows                                                                                                      
   INPUT                 :                                                                                                              
   OUTPUT                :                                                                                                              
  -----------------------------------------------------------------------------                                                         
   Author : Gheorghe Doros                                                                                                              
   Last modified                                                                                                          
 **eoh************************************************************************/       

filename in 'Z:\Documents\BS853\Class 11\seize.data.txt';
data seizures;
infile in;
input ID RESPONSE TIME TREATMENT BASELINE AGE;
logresponse=log(response+1);
logbase=log(baseline+1);
run;
proc sort data=seizures ;by id time;


goptions reset=all ftext='arial' htext=1.2 hsize=9.5in vsize=7in aspect=1 horigin=1.5in vorigin=0.3in ;
goptions device=emf rotate=landscape gsfname=TempOut1 gsfmode=replace;
filename TempOut1 "Z:\Documents\BS853\Spring 2016\Class 11\meanresponse.emf";  
axis1 label=('Time') minor=none order=(1 to 4 by 1) value=(tick=1 'Time 1' tick=2 'Time 2' tick=3 'Time 3' tick=4 'Time 4') ;
axis2 label=(a=90 'Log number of seizures') minor=none order=(0 to 4 by .5);
legend value=('Control' 'Treatment');

symbol1 i=std1mpj;
proc gplot data=seizures;;
plot logresponse*time=treatment/haxis=axis1 vaxis=axis2 legend=legend1;
run; 

filename TempOut1 "Z:\Documents\BS853\Spring 2016\Class 11\cases.emf";  
axis2 label=(a=90 'Log number of seizures') minor=none order=(0 to 4 by .5);

symbol1 i=j c=red r=3;
symbol2 i=j c=blue r=3;
symbol3 i=j c=red r=5;
symbol4 i=j c=blue r=2;

proc gplot data=seizures ;
 where ID lt 117 ;
plot logresponse*time=ID/haxis=axis1 vaxis=axis2;
run; 

/*********************************/
/*** Naive analysis ***/
/*********************************/
options ls=97 ps=60 pageno=1 nodate;

title1 ' Naive analyses treating time as categorical ';
proc genmod data=seizures ;
 class TIME TREATMENT ;
 model RESPONSE=TIME|TREATMENT BASELINE AGE/dist=p type3 scale=d;
run;
title1 ' Naive analyses treating time as continuous ';
proc genmod data=seizures ;
  class TREATMENT ;
  model RESPONSE=TIME|TREATMENT BASELINE AGE/dist=p type3 scale=d;
run;


/**********************************************/
/*** GEE ***/
/**********************************************/

/*** Estimating the corr structure ***/

proc transpose data=seizures out=wides(where=( col1 ne .));;
  by id;
  var logresponse;
  copy TREATMENT BASELINE AGE;
run;

options ls=97 ps=60 pageno=1 nodate;
title1 'Estimated correlation ';
proc corr data=wides;
  var col1-col4;
run;

options ls=97 ps=60 pageno=1 nodate;
title1 ' A GEE model with a time dependent treatment effect';
title2 ' Exchangeable correlation structure ';
ods output GEEFitCriteria=QIC;
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME|TREATMENT BASELINE AGE/d=poisson; 
  repeated subject = id / type=cs corrw covb; 
run; 
quit;

/***  Model Selection using QIC***/
%include 'Z:\Documents\BS853\Class 11\qic.sas';
 ods output GEEFitCriteria=QIC1;
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME|TREATMENT BASELINE AGE/d=poisson; 
  repeated subject = id / type=cs corrw covb; 
run; 
data QIC1;
length Model $50.;
set QIC1;
Model='TIME|TREATMENT BASELINE AGE';
run;

   
 ods output GEEFitCriteria=QIC2;
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME|TREATMENT AGE/d=poisson; 
  repeated subject = id / type=cs corrw covb; 
run; 
data QIC2;
length Model $50.;
set QIC2;
Model='TIME|TREATMENT AGE';
run;


 ods output GEEFitCriteria=QIC3;
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME|TREATMENT BASELINE/d=poisson; 
  repeated subject = id / type=cs corrw covb; 
run; 
data QIC3;
length Model $50.;
set QIC3;
Model='TIME|TREATMENT BASELINE';
run;


 ods output GEEFitCriteria=QIC4;
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME|TREATMENT/d=poisson; 
  repeated subject = id / type=cs corrw covb; 
run; 
data QIC4;
length Model $50.;
set QIC4;
Model='TIME|TREATMENT';
run;


 ods output GEEFitCriteria=QIC5;
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME TREATMENT BASELINE AGE/d=poisson; 
  repeated subject = id / type=cs corrw covb; 
run; 
data QIC5;
length Model $50.;
set QIC5;
Model='TIME TREATMENT BASELINE AGE';
run;

 
 ods output GEEFitCriteria=QIC6;
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME TREATMENT BASELINE/d=poisson; 
  repeated subject = id / type=cs corrw covb; 
run; 
data QIC6;
length Model $50.;
set QIC6;
Model='TIME TREATMENT BASELINE';
run;

 ods output GEEFitCriteria=QIC7;
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME TREATMENT AGE/d=poisson; 
  repeated subject = id / type=cs corrw covb; 
run; 
data QIC7;
length Model $50.;
set QIC7;
Model='TIME TREATMENT AGE';
run;

 ods output GEEFitCriteria=QIC8;
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME TREATMENT /d=poisson; 
  repeated subject = id / type=cs corrw covb; 
run; 

data QIC8;
length Model $50.;
set QIC8;
Model='TIME TREATMENT';
run;

data QIC;
set QIC1 - QIC8;
run;
proc sort data=QIC;
by criterion value;
run;

options ls=97 ps=60 pageno=1 nodate;
title1 'Summary of model selection process';
   proc print data=QIC noobs;
     run;

/***** Final Model *****/
options ls=97 ps=60 pageno=1 nodate;
title1 ' A GEE model with a time independent treatment effect';
title2 ' Exchangeable correlation structure ';
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME TREATMENT BASELINE AGE/d=poisson; 
  repeated subject = id / type=cs corrw; 
  estimate 'TRT 1 vs. 0'  TREATMENT -1 1/exp;
run; 
quit;

options ls=97 ps=60 pageno=1 nodate;
title1 ' A GEE model with a time independent treatment effect and LOG baseline';
title2 ' Exchangeable correlation structure ';
proc genmod data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME TREATMENT logBASE AGE/d=poisson; 
  repeated subject = id / type=cs corrw; 
  estimate 'TRT 1 vs. 0'  TREATMENT -1 1/exp;
run; 
quit;

/***** GLMM Model ***********/
options ls=97 ps=60 pageno=1 nodate;
title1 ' A GLMM model with a time independent treatment effect';
proc glimmix data=seizures;
  class  id time treatment; 
  model RESPONSE=TIME TREATMENT BASELINE AGE/ s d=poisson link=log; 
  random intercept /subject = id ; 
  estimate 'TRT 1 vs. 0'  TREATMENT -1 1/exp cl;
run; 
quit;
