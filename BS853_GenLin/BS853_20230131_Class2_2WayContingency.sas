   /*soh************************************************************************
   Boston University - Biostatistics Department
   PROGRAM NAME          : script - LogLinear Models.sas
                           (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 2)
   PROJECT NAME          : Generalized Linear Models (BS853) 
                           - Lecture: Generalized Linear Models

   DESCRIPTION           : 1. Read data
                           2. Loglinear Models for 2x2 tables and sets of 2x2 tables
                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3
   INFRASTRUCTURE        : Windows 
   INPUT                 : 
   OUTPUT                : 
  -----------------------------------------------------------------------------
   Author : Gheorghe Doros 
   Last modified 01/29/2023               
 **eoh************************************************************************/
options ps=60 ls=89 pageno=1 nodate;

/* read data in SAS */
data prenatal_care; 
input lessca $ death $ count; 
datalines; 
y y 20  
n y 6   
y n 373 
n n 316 
; 
run; 

ods output OBSTATS=check_s;
title 'Loglinear Models with PROC GENMOD';
title2 'Saturated model';
proc genmod data=prenatal_care; /*corresponds to different parametrization  */ 
  class lessca death; 
  model count=lessca death lessca*death/dist=poisson link=log obstats type3; 
run; 

ods output OBSTATS=check_i;
title2 'Independence model';
proc genmod data=prenatal_care; 
  class lessca death; 
  model count=lessca death /dist=poisson link=log obstats residuals; 
run;

/* use the output to construct the Likelihood ratio test by subtracting the likelihoods and multiply by 2 */

title1 "Predicted values and residuals for the independence model";
proc print data=check_i; 
  var count lessca death pred std reschi resdev; 
run;

/*Check that sum of squared residuals equals the two goodness of fir statistics*/
data fit; 
set check_i; 
 array res {2} reschi resdev ; 
 array ssq {2} ssqchi ssqdev ; 
do i=1 to 2; 
 ssq[i]=res[i]**2; 
end; 
run;

title1 "Check against the criteria for assessing goodness of fit from Genmod";
proc means data=fit sum; 
 var ssqchi ssqdev ; 
run; 

 
/* another way to test independence : using PROC CATMOD */
title 'Loglinear Models with PROC CATMOD';
title2 'Saturated model';
proc catmod data=prenatal_care; 
  weight count;  
  model lessca*death=_response_/noiter ; 
  loglin lessca|death; 
run; 

title2 'Independence model';
proc catmod data=prenatal_care; 
  weight count;  
  model lessca*death=_response_/noiter ; 
  loglin lessca death; 
run;

/* another way to test independence : using PROC FREQ */
proc freq; 
  tables lessca*death/chisq; 
  weight count;  
run; 

/**************************************************************/
/* Loglinear models in SxR tables - Analysis of melanoma data */
/**************************************************************/

title 'Loglinear models in SxR tables - Analysis of melanoma data'; 
data melanoma; 
input type site $ count; 
datalines; 
1 h 22
1 t 2  
1 e 10
2 h 16
2 t 54
2 e 115
3 h 19
3 t 33
3 e 73
4 h 11 
4 t 17
4 e 28
run;
options ps=60 ls=89 pageno=1 nodate;

title2 ' Using PROC GENMOD ';
ods select ModelFit ParameterEstimates Type3;
proc genmod data=melanoma; 
  class type site;
  model count=type|site/dist=poisson type3;
run;
quit;

ods select ModelFit ParameterEstimates Type3 ObStats;
proc genmod data=melanoma; 
  class type site;
  model count=type site/dist=poisson type3 obstats;
run;
quit;

title2 ' Test independence by using PROC CATMOD ';
ods select ResponseProfiles ResponseMatrix ANOVA  Estimates;
proc catmod data=melanoma; 
  weight count; 
  model type*site=_response_/response ; 
  loglin type site; 
run;
quit;

title2 ' Test independence by using PROC FREQ ';
ods select chisq;
proc freq data=melanoma;
  tables type*site/chisq; 
  weight count; 
run;
/***************************************  Simulations *******/
/* Parameter models */
%let LX=1; 
%let LY=-1;
%let L0=3;
%let LXY=1;

/*** Single data ***/
data one;
call streaminit( 1234 );
do X = 1 to 2;
 do Y = 1 to 2;
  Yi=rand('poisson',exp(&L0+&LX*(X=1)+&LY*(Y=1))); * Independence;
  Ys=rand('poisson',exp(&L0+&LX*(X=1)+&LY*(Y=1)+&LXY*(X=1)*(Y=1))); *Saturated;
  output;
 end;
end;
run;
title Testing independce using independent model;
ods output ChiSq=ind(where = (statistic in ('Likelihood Ratio Chi-Square','Chi-Square')) keep = statistic prob);
proc freq data=one;
 table X*Y/chisq;
 weight Yi;
run;
title Testing independce using non-independent model;
ods output ChiSq=NInd(where = (statistic in ('Likelihood Ratio Chi-Square','Chi-Square')) keep = statistic prob);
proc freq data=one;
 table X*Y/chisq;
 weight Ys;
run;

/*** Multiple data (Simulation) ***/
%let nSim=1000;
data Multi;
call streaminit( 4321 );
do X = 1 to 2;
 do Y = 1 to 2;
  do Simulation = 1 to &nSim;
  iLambda=exp(&L0+&LX*(X=1)+&LY*(Y=1)); *Independence;
  Yi=rand('poisson',iLambda);
  Lambda=exp(&L0+&LX*(X=1)+&LY*(Y=1)+&LXY*(X=1)*(Y=1));*Saturated;
  Ys=rand('poisson',Lambda);
  output;
  end;
 end;
end;
run;
proc sort data=Multi;
 by Simulation;
 run;
title Testing independce using independent model;
ods output ChiSq=ind(where = (statistic in ('Likelihood Ratio Chi-Square','Chi-Square')) keep = Simulation statistic value prob);
proc freq data=multi;
 by simulation;
 table X*Y/chisq;
 weight Yi;
run;
Title1 Summary of testing;
proc means data=ind mean std;
 var  value prob;
 class statistic;
 run;
title Testing independce using non-independent model;
ods output ChiSq=nind(where = (statistic in ('Likelihood Ratio Chi-Square','Chi-Square')) keep = Simulation statistic value prob);
proc freq data=multi;
 by simulation;
 table X*Y/chisq;
 weight Ys;
run;
Title1 Summary of testing;
proc sql;
select distinct a.statistic, mean(a.Value>3.84) as Tail,mean(a.Value>0.455) as Median, mean(b.Value>3.84) as Power 
from ind as a, nind as b where a.Simulation=b.Simulation and a.Statistic=b.Statistic group by A.Statistic;
quit;

