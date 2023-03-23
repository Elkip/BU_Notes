libname S857 'C:\Users\ytrip\Dropbox\Courses\BS857\2020\Datasets'; 
data TLC_long;
set s857.TLC;
y=y0;time=1;OUTPUT;
y=y1;time=2;OUTPUT;
y=y4;time=4;OUTPUT;
y=y6;time=6;OUTPUT;
drop y0 y1 y4 y6;
run;
proc mixed data=TLC_long;
class TRT TIME;
model y=time time*trt/s;
repeated time/type=un subject=id;
run;
data  TLC_missing;
set  s857.TLC;
prop0=exp(2.5-.03*y1)/(1+exp(2.5-.03*y1));
prop1=exp(2.5-.03*y1*2)/(1+exp(2.5-.03*y1*2));
prop4=exp(2.5-.03*y1*3)/(1+exp(2.5-.03*y1*3));
prop6=exp(2.5-.03*y1*4)/(1+exp(2.5-.03*y1*4));
if prop0<.5 then y0=.;
if prop1<.5 then y1=.;
if prop4<.5 then y4=.;
if prop6<.5 then y6=.;
if y0=. then R0=0;
else R0=1;
if y1=. then R1=0;
else R1=1;
if y4=. then R4=0;
else R4=1;
if y6=. then R6=0;
else R6=1;
run;
proc freq data= TLC_missing;
tables (R0 R1 R4 R6)*TRT/chisq;
run;
data  TLC_missing_long;
set  TLC_missing;
y=y0;time=1;OUTPUT;
y=y1;time=2;OUTPUT;
y=y4;time=4;OUTPUT;
y=y6;time=6;OUTPUT;
drop y0 y1 y4 y6;
run;
proc mixed data=TLC_missing_long;
class TRT TIME;
model y=time time*trt/s;
repeated time/type=un subject=id;
run;
proc mi data=TLC_missing seed=364865 nimpute=25 out=miTLC;
var TRT y0 y1 y4 y6;
class TRT;
monotone regression(y4=TRT y0 y1 TRT*y0 TRT*y1);
monotone regression(y6=TRT y0 y1 y4 TRT*y0 TRT*y1 TRT*y4) ;
run;
data MITLC_long;
set MITLC;
y=y0;time=1;OUTPUT;
y=y1;time=2;OUTPUT;
y=y4;time=4;OUTPUT;
y=y6;time=6;OUTPUT;
drop y0 y1 y4 y6;
run;
proc sort data=MITLC_long;
by _IMPUTATION_;
run;
/*We have created 25 versions of the same dataset
with no missing values. We can run proc mixed 
for each version seperately...*/
proc mixed data=MITLC_long;
where _imputation_=2;
class TRT TIME;
model y=time time*trt/s covb;
repeated time/type=un subject=id;
run;
/*
...or run all 25 in one run using a by statement
and saving the solutions using an ods output statement
*/
proc mixed data=MITLC_long;
class TRT TIME;
model y=time time*trt/s covb;
repeated time/type=un subject=id;
by _IMPUTATION_;
ods output solutionf=beta covb=varbeta;
run;
proc mianalyze parms=beta;
class TRT TIME; 
modeleffects intercept time TRT*time;
run;

/*Using MCMC for imputation*/
proc sort data=TLC_missing;
by TRT;
run;
proc mi data=TLC_missing seed=364865 nimpute=25 out=miTLC_MCMC;
var y4 y6;
by TRT;
mcmc chain=multiple displayinit initial=em(itprint);
run;
data MITLC_MCMC_long;
set MITLC_MCMC;
y=y0;time=1;OUTPUT;
y=y1;time=2;OUTPUT;
y=y4;time=4;OUTPUT;
y=y6;time=6;OUTPUT;
drop y0 y1 y4 y6;
run;
proc sort data=MITLC_MCMC_long;
by _IMPUTATION_;
run;
proc mixed data=MITLC_MCMC_long;
class TRT TIME;
model y=time time*trt/s covb;
repeated time/type=un subject=id;
by _IMPUTATION_;
ods output solutionf=beta_mcmc covb=varbeta_mcmc;
run;
proc mianalyze parms=beta_mcmc;
class TRT TIME; 
modeleffects intercept time TRT*time;
run;
title 'Difference in df between Random Intercept and Compound Symmetry';

proc mixed data=TLC_long;
class TRT TIME;
model y=time time*trt/s ddfm=kenwardroger;
repeated time/type=cs subject=id;
run;
proc mixed data=TLC_long;
class TRT TIME;
model y=time time*trt/s ddfm=kenwardroger;
random int/type=un subject=id;
run;



proc mixed data=TLC_long;
class TRT TIME;
model y=time time*trt/s ;
repeated time/type=cs subject=id;
run;
proc mixed data=TLC_long;
class TRT TIME;
model y=time time*trt/s ;
random int/type=un subject=id;
run;


/*SAMPLE SIZE CALCULATION*/
/*Comparison of two groups 
with consistent difference across time
determines number per group ;
5 timepoints (ICC=.4);
effect size of .5;
power = .8 for a 2-tailed .05 test;*/
DATA one;
n = 5;
za = PROBIT(.975);
zb = PROBIT(.8);
rho = .4;
effsize = .5;
num = (2*(za + zb)**2)*(1 + (n-1)*rho);
den = n*(effsize**2);
npergrp = num/den;
PROC PRINT;VAR npergrp;
RUN;

/*Sample size calculation across time points
determines number per group;
3 timepoints;
linear increasing effect sizes of 0 .25 .5;
group by linear contrast across time;
AR1 structure with rho=.5;
power = .8 for a 2-tailed .05 test;*/
PROC IML;
za = PROBIT(.975);
zb = PROBIT(.8);
meandiff = {0, .25, .5};
contrast = {-1, 0, 1};
corrmat = {1 .5 .25 ,
			.5 1 .5 ,
			.25 .5 1 };
contdiff = T(contrast) * meandiff;
contvar = T(contrast)*corrmat*contrast;
NperGrp = ((2*(za+zb)**2) * contvar)/(contdiff**2);
PRINT NperGrp;
run;
