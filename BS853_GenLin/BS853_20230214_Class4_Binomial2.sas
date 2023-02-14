  /*soh************************************************************************                                                        
   Boston University - Biostatistics Department                                                                                         
   PROGRAM NAME          : script - BootstrapLarge.sas                                                                                
                           (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 4)                                        
   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
                           - Lecture: Generalized Linear Models  - Assessing Predictive anility of binomial models                                                                       
                                                                                                                                        
   DESCRIPTION           : 1. Read data                                                                                                 
                           2. Logogistic regression models                                                    
                                                                                                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.1                                                                                              
   INFRASTRUCTURE        : Windows                                                                                                      
   INPUT                 :                                                                                                              
   OUTPUT                :                                                                                                              
  -----------------------------------------------------------------------------                                                         
   Author : Gheorghe Doros                                                                                                              
   Last modified 02/12/2019                                                                                                             
 **eoh************************************************************************/                                                         
options ps=60 ls=89 pageno=1 nodate;                      
%let p=30;  /* Number of Predictors */
%let N=1000;/* Number of Observations */
%let Nr=100;/* Number of Bootstrap Samples */

data one;
array x[&p] x1-x&p;
do i=1 to &N;
 do j=1 to &p;
   x[j]=rand('normal');
 end;
 y = (rand('uniform') < 0.5);
 output;
end;
run;

/* Get the c-statistic on the original data */
proc logistic data=one;
  model y (event="1") = x1-x&p / RSQUARE LACKFIT; 
  ods output Association=C0;
  score data=one fitstats;
run;
data C01 (keep=Co Dxyo);
    set C0 (keep=LABEL2 NVALUE2 rename=(NVALUE2=Co)); 
     if LABEL2='c'; /* Retain the c-statistics */
     Dxyo=2*(Co-0.5); /* Calculate Somers'D */ 
run;

/* Generate Nr Bootstrap datasets */
proc surveyselect data=one out=BOOTDAT
   rep=&Nr seed=10 method=URS samprate=1 outhits;
run;
/* Analyze Bootstrap data and retain the model*/
proc logistic data=BOOTDAT outmodel=aa;
  by replicate;
  model y(event='1')=x1-x&p;
  ods output Association=CB;
run;
data Cb1 (keep=replicate Cb);
  set Cb (keep=replicate LABEL2 NVALUE2 rename=(NVALUE2=Cb)); 
  if LABEL2='c';
run;
/* Generate Nr copies of the original data */
Data oone;
  set one;
  do Replicate=1 to &Nr;
   output;
  end;
run;
proc sort data=oone;
  by replicate;
run;

/* 'Score' the observed data for each model on the bootstrap data */
/* Here score means calculate statistics on the new data based on prediction 
of the model fit on a original data; statistics include: AIC, BIC, SC (Schwarz' criterion)
Brier's score, Error rate, AUC, RSquared, Max-Scaled RSquared */
ods output ScoreFitStat=BB(keep = replicate AUC rename=(AUC=Cbo)) ;
proc logistic inmodel=aa;
  by replicate;
  score data=oone out=AUC fitstat;
run;

/* Merge results from the analysis of boostrap data and 
   results on the observed */
data cb2;
  merge BB CB1;
  by replicate;
    diff=CB-CBo; /* Calculate the difference between boostraped C and C on original*/
/* Do the same for the Somers'D */
    Dxyb=2*(cB-0.5);
    Dxybo=2*(cBo-0.5);
    ddiff=DxyB-Dxybo;
run;

/* Summarize results in Macro variables */
proc sql;
  select mean(diff) into: Bias from cb2;
  select co  into: Co from c01;
  select co-&bias  into: Cor from c01;
  select mean(ddiff) into: dBias from cb2;
  select Dxyo  into: Dxyo from c01;
  select Dxyo-&dbias  into: Dxyor from c01;
quit;

/* Display results in the Log window */
%put NOTE: Original : &Dxyo, Corrected: &Dxyor, Optimism: &dbias;
%put NOTE: Original : &Co  , Corrected: &cor  , Optimism: &bias;
