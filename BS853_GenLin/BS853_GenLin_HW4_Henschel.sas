*Exercise 0, Part 4: Bootstrap Sampling;
%let p=15;  /* Number of Predictors */
%let N=2000;/* Number of Observations */
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

*Exercise 1;
*1. Build a logsitic model for the following data on cancer remission;
data Remission ;
input remiss cell smear infil li blast temp ;
label remiss = "Remission";
datalines ;
1 .8 .83 .66 1.9 1.1 .996
1 .9 .36 .32 1.4 .74 .992
0 .8 .88 .7 .8 .176 .982
0 1 .87 .87 .7 1.053 .986
1 .9 .75 .68 1.3 .519 .98
0 1 .65 .65 .6 .519 .982
1 .95 .97 .92 1 1.23 .992
0 .95 .87 .83 1.9 1.354 1.02
0 1 .45 .45 .8 .322 .999
0 .95 .36 .34 .5 0 1.038
0 .85 .39 .33 .7 .279 .988
0 .7 .76 .53 1.2 .146 .982
0 .8 .46 .37 .4 .38 1.006
0 .2 .39 .08 .8 .114 .99
0 1 .9 .9 1.1 1.037 .99
1 1 .84 .84 1.9 2.064 1.02
0 .65 .42 .27 .5 .114 1.014
0 1 .75 .75 1 1.322 1.004
1 0 .5 .44 .22 .6 .114 .99
1 1 .63 .63 1.1 1.072 .986
0 1 .33 .33 .4 .176 1.01
0 .9 .93 .84 .6 1.591 1.02
1 1 .58 .58 1 .531 1.002
0 .95 .32 .3 1.6 .886 .988
1 1 .6 .6 1.7 .964 .99
1 1 .69 .69 .9 .398 .986
0 1 .73 .73 .7 .398 .986
;
run ;

title1 'Remission Logistic Model - All Covariates';
proc genmod data=remission;
	model remiss = cell smear infil li blast temp /dist=Binomial link=logit;
run;quit;

*2. Give the predicitive indexes;
proc logistic data=remission;
	model remiss = cell smear infil li blast temp /RSQUARE LACKFIT;
run;quit;

*3. estimate optimism for the previous measures; 
title 'Predictive Indexes Opimism';
%let p=6;  /* Number of Predictors */

proc logistic data=remission;
  model remiss = cell smear infil li blast temp /RSQUARE LACKFIT; 
  ods output Association=C0;
  score data=remission fitstats;
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
