  /*soh************************************************************************                                                        
   Boston University - Biostatistics Department                                                                                         
   PROGRAM NAME          : script - Lecture 9.sas                                                                                
                           (.\BS853\Class 9)                                        
   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
                           - Lecture: Gamma Regression                                                          
                                                                                                                                        
   DESCRIPTION           : 1. Simulation investigating operating characteristics  
                              of model selection test in Gamma regression                                                 
                                                                                                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3                                                                                              
   INFRASTRUCTURE        : Windows                                                                                                      
   INPUT                 :                                                                                                              
   OUTPUT                :                                                                                                              
  -----------------------------------------------------------------------------                                                         
   Author : Gheorghe Doros                                                                                                              
   Last modified                                                                                                           
 **eoh************************************************************************/      

/*** Test for overdisperion ***/
data one;
call streaminit(1000);
do simulations=1 to 10000;
 do ID=1 to 100;
  X1= rand('normal')*3+4;
  X2=rand('beta',3,12);
  X3=rand('poisson',7);
  X4=rand('binomial',.5,20);
  X5=rand('normal');
  X6=rand('normal');
  X7=rand('normal');
  MU=exp(2+X1-3*X2);
   Y=rand('gamma',10,MU/10);
output;
end;
end;
run;

proc sort data=one;by simulations;run;
ods output ParameterEstimates=PEf(where=(parameter='Scale') keep = Estimate parameter simulations)
ModelFit=MFf(where=(Criterion = 'Deviance')) contrasts=Cf(keep =simulations Probchisq);
proc genmod data=one;
 by simulations;
  model Y=X1 X2 X3 X4 X5 X6 X7/link=log d=gamma;
  contrast 'Overall' X3 1, X4 1, X5 1, X6 1, X7 1;
run;
 
ods output ParameterEstimates=PE0(where=(parameter='Scale') keep = Estimate parameter simulations)
ModelFit=MF0(where=(Criterion = 'Deviance'));
proc genmod data=one;
 by simulations;
  model Y=X1 X2/link=log d=gamma;
run;
 
data All;
merge PEf MFf PE0(rename=(estimate=estimate0)) MF0(rename =(DF=df0 Value=Value0)) CF;
  by simulations;
  Diff1=Value0-Value;
  Diff2=Value0*Estimate0-Value*Estimate;
  Diff3=(Value0-Value)*Estimate;
  P1=(Diff1<11.0705);
  P2=(Diff2<11.0705); 
  P3=(Diff3<11.0705); 
  Sign=(ProbChisq<0.05);
label Diff1='Standard Devience' Diff2='Difference in Scaled Deviances' Diff3='Scaled difference'
P1='95% quantile check (1)' P2='95% quantile check (2)' P3='95% quantile check (3)'
Sign='Significance based on Contrast';
 
run; 
title 'Summary of Results';
proc means data=All n mean;
  var Diff1 Diff2 Diff3 p1-p3 Sign;
run;
 
