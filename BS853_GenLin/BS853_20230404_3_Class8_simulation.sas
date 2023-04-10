  /*soh************************************************************************                                                        
   Boston University - Biostatistics Department                                                                                         
   PROGRAM NAME          : script - Lecture 8.sas                                                                                
                           (.\BS853\Class 8)                                        
   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
                           - Lecture: Generalized Linear Models for count data                                                          
                                                                                                                                        
   DESCRIPTION           : 1. Simulation investigating operating characteristics  of test for oiverdispersion                                                 
                                                                                                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3                                                                                              
   INFRASTRUCTURE        : Windows                                                                                                      
   INPUT                 :                                                                                                              
   OUTPUT                :                                                                                                              
  -----------------------------------------------------------------------------                                                         
   Author : Gheorghe Doros                                                                                                              
   Last modified                                                                                                           
 **eoh************************************************************************/      

/*** Test for overdisperion ***/
data negbin;
call streaminit(2020);
	do ID = 1 to 500;
	    X1=rand('normal');                  /* Normal predictor */
      X2=rand('normal');
      X3=rand('normal');
		yp = rand('Poisson',  exp(2-3*X1+X2-2*X3)); /* Assume a log link */
		output;
	end;
run;

ods output ModelFit=p0  convergencestatus=cP ;
proc genmod data=negbin;
  model yp=X1 X2 X3/d=p;
run;
ods output ModelFit=nb0 ;
proc genmod data=negbin;
  model yp=X1 X2 X3/d=nb;
run;
data p;
  set p0;
  if criterion="Log Likelihood";
  keep value criterion;
run;
data nb;
   set nb0;
   if criterion="Log Likelihood";
   keep  value criterion;
run;;
data all;
set p(rename=(value=ValueP));
 set nb(rename=(value=ValueNB));
 Diff=-2*(Valuep-Valuenb);
 Sig=diff>2.71; /* Compare against the q(10%)*/
run;

/*** Multiple simulations ***/
data negbin;
call streaminit(2021);
do Simulation=1 to 1000; /* multiple simulations */
	do ID = 1 to 500;
	    X1=rand('normal');  
      X2=rand('normal');               /* Normal predictor */
      X3=rand('normal');
		yp = rand('Poisson',  exp(2-3*X1+X2-2*X3)); /* Assume a log link */
		output;
	end;
	end;
run;
proc sort data=negbin;
  by Simulation;
 run;
ods listing off;
ods output ModelFit=p ;
proc genmod data=negbin;
by Simulation;
model yp=X1 X2 X3/d=p;
run;
ods output ModelFit=nb  convergencestatus=cNB ; /* Retain convergence status */
proc genmod data=negbin;
 by Simulation;
 model yp=X1 X2 X3/d=nb;
run;
ods listing;
proc freq data=cnb;table status;run;
data p1;
  set p;
  if criterion="Log Likelihood";
  keep  Simulation value criterion;
run;
data nb1;
  set nb;
  if criterion="Log Likelihood";
  keep  Simulation value criterion;
run;;
data all; /* Merge results */
  merge p1(rename=(value=ValueP)) nb1(rename=(value=ValueNB)) cNB;
  by Simulation;Diff=-2*(Valuep-Valuenb);
  Significance0=diff>2.71; 
  Significance1=diff>3.84; 
  if status eq 0;
run;
title "Type I Error Estimate";
proc means data=all mean;
  var significance0 Significance1 ;
  label significance0='Test at 10% Level' Significance1 ='Test at 5% Level' ;
run;
