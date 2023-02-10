
/***********************************************************/
/*   SAS Script: Bayesian Modeling using SAS' proc MCMC    */
/*                                                         */
/*   Models: Simple Linear Regression, Logistic regression */
/*           Hierarchical Linear and Loigistic regression  */
/*                                                         */
/***********************************************************/

%let path=Z:/Documents/BRCT/BS855/Lecture/Lecture4;

/*** Linear regression : HDL Example  ***/
proc import datafile="&path/hdl-data.txt"
            out=class
            dbms=dlm
            replace;
     delimiter='09'x;
run;

* Calculate means to center;
proc sql;
select mean(age5), mean(bmi5) into :a,:b from class;
quit;

* Center vars;
data HDL;
 set class;
  genderc=Sex-1;
  BMIc=BMI5-&b;
  AGEc=AGE5-&a;
  HDL=HDL5;
  ID=_n_;
run;

/* Bayesian Multiple regression */
options ls=65 nocenter pageno=1;
ods listing gpath="&path";
ods graphics / imagename="sr" imagefmt=png;
title 'Simple Multiple Regression';
proc mcmc data=HDL seed=1 nbi=5000 nmc=20000 outpost=regOut thin=5; 
  parms beta0 beta1 beta2 beta3 s2;
  prior beta: ~ normal(0, var=10000);
  prior s2 ~ igamma(shape=1, scale=1);
  mu = beta0 + beta1*agec+beta2*genderc+ beta3*bmic;
  model HDL ~ normal(mu, var=s2); 
run;
ods graphics off;


title 'Multiple Regression with derived quantities';
proc mcmc data=HDL seed=1 nbi=5000 nmc=20000 outpost=regOut monitor=(beta0 beta1 beta2 beta3 s2 M25 F25) thin=5; 
  parms beta0 beta1 beta2 beta3 s2;
  prior beta: ~ normal(0, var=10000);
  prior s2 ~ igamma(shape=1, scale=1);
  mu = beta0 + beta1*agec+beta2*genderc+ beta3*bmic;
  model HDL ~ normal(mu, var=s2); 
* derived quantities ;  
BEGINNODATA;
  F25=beta0+beta3*(25-&b);
  M25=beta0+beta3*(25-&b)+beta2;
ENDNODATA;
run;

/*** Calculate residuals ***/
* Fuse the data and samples ;
proc sql;
create table fuse as select a.*, b.*  FROM regOut as a,  HDL as b;
quit;
* Create residuals ;
data fuse;
set fuse;
mu=beta0 + beta1*agec+beta2*genderc+ beta3*bmic;
r=HDL-mu;
sr=r/sqrt(s2);
run;

/*ods listing gpath="&path";
ods graphics / imagename="StR" imagefmt=png;
proc sgplot data=fuse;
  vbox sr / category=ID;
  xaxis display=(nolabel);
run;
ods graphics off;*/

* take means of residuals ;

proc sql;create table summary as
select distinct ID, mean(sr) as msr, mean(r) as ms,BMIc,HDL from fuse group by ID;
quit;

ods listing gpath="&path";
ods graphics / imagename="HDL" imagefmt=png;
proc sgplot data=summary;
  scatter Y=msr X=HDL ;
  label msr='Mean Student Residual';
run;
ods graphics off;

ods listing gpath="&path";
ods graphics / imagename="BMI" imagefmt=png;
proc sgplot data=summary;
  scatter Y=msr X=BMIc ;
  label msr='Mean Student Residual';
run;
ods graphics off;

/*** Logistic regression : Hospital data ***/
proc import datafile="&path/dbinom.xlsx" out=bin replace;run;

options ls=65 nocenter;
proc mcmc data=bin seed=10 nbi=5000 nmc=20000 outpost=binOut 
monitor=(theta) thin=5; 
  parms theta;
  prior theta ~ beta(1, 1);
  model Y ~ binomial(N,theta); 
run;

/*** Random effects Logistic regression ***/
options ls=65 nocenter;
proc mcmc data=bin seed=10 nbi=5000 nmc=20000 outpost=binOut monitor=(s2 p0) plot=all  thin=5;
  parms mu  s2;
  prior mu ~ normal(0, var=10000);
  random alpha ~ normal(0, var=s2) subject=_obs_ monitor=(alpha);
  prior s2 ~ igamma(shape=1, scale=1);
  theta= logistic(mu + alpha);
  model Y ~ binomial(N,theta); 
  BEGINNODATA;
  p0=logistic(mu);
  ENDNODATA;
run;

/*** Logistic regression : Rats data ***/
proc import datafile="&path/Rats.xlsx" out=rat replace;run;
data rat;
  set rat;
  Xc=D-22;
run;

/*** Simple Logistic regression : Rats data ***/
proc mcmc data=rat seed=10 nbi=5000 nmc=20000 outpost=binOut0 
monitor=(Ac beta0 beta1 s2)  plot=all thin=5; 
  parms beta0 beta1  s2;
  prior beta: ~ normal(0, var=10000);
  prior s2 ~ igamma(shape=1, scale=1);
  mu= beta0 + beta1*Xc;
  model W ~ Normal(mu,var=s2);
  BEGINNODATA;
  Ac=beta0+(8-22)*beta1;
  ENDNODATA; 
run;

/*** Hierarchical Logistic regression (random intercept) : Rats data ***/

ods listing gpath="&path";
ods graphics / imagename="HR" imagefmt=png reset=index;
proc mcmc data=rat seed=10 nbi=5000 nmc=40000 outpost=binOut1 
monitor=(Ac beta0 beta1 s2 s2a)  plot=all thin=20; 
  parms beta0 beta1 s2 s2a;
  prior beta: ~ normal(0, var=10000);
  random alpha ~ normal(0, var=s2a) subject=ID monitor=(alpha);
  prior s2 ~ igamma(shape=1, scale=1);
  prior s2a ~ igamma(shape=1, scale=1);
  mu= beta0 + alpha + beta1*Xc;
  model W ~ Normal(mu,var=s2); 
  BEGINNODATA;
  Ac=beta0+(8-22)*beta1;
  ENDNODATA; 
run;
ods graphics off;

/*** Hierarchical Logistic regression (random intercept) : blocking (beta0 beta1), (s2) and (s2a) ***/

ods listing gpath="&path";
ods graphics / imagename="HRs" imagefmt=png reset=index;
proc mcmc data=rat seed=10 nbi=5000 nmc=40000 outpost=binOut1 
monitor=(Ac beta0 beta1 s2 s2a)  plot=all thin=20; 
  parms beta0 beta1;
  parms s2;
  parms s2a;
  prior beta: ~ normal(0, var=10000);
  random alpha ~ normal(0, var=s2a) subject=ID monitor=(alpha);
  prior s2 ~ igamma(shape=1, scale=1);
  prior s2a ~ igamma(shape=1, scale=1);
  mu= beta0 + alpha + beta1*Xc;
  model W ~ Normal(mu,var=s2); 
  BEGINNODATA;
  Ac=beta0+(8-22)*beta1;
  ENDNODATA; 
run;
ods graphics off;

/*** Hierarchical Logistic regression (random intercept) : using NUTS sampler***/

ods listing gpath="&path";
ods graphics / imagename="HRR" imagefmt=png reset=index;
proc mcmc data=rat seed=10 nbi=5000 nmc=40000 outpost=binOut1r
monitor=(Ac beta0 beta1 s2 s2a)  plot=all thin=20 ALG=NUTS; 
  parms beta0 beta1 s2 s2a;
  prior beta: ~ normal(0, var=10000);
  random alpha ~ normal(0, var=s2a) subject=ID monitor=(alpha);
  prior s2 ~ igamma(shape=1, scale=1);
  prior s2a ~ igamma(shape=1, scale=1);
  mu= beta0 + alpha + beta1*Xc;
  model W ~ Normal(mu,var=s2); 
run;
ods graphics off;

/*** Hierarchical Logistic regression (random intercept and slope) ***/

proc mcmc data=rat seed=10 nbi=5000 nmc=20000 outpost=binOut2 
monitor=(Ac beta0 beta1 s2 s2a s2g)  plot=all thin=5; 
  parms beta0 beta1 s2 s2a s2g;
  prior beta: ~ normal(0, var=10000);
  random alpha ~ normal(0, var=s2a) subject=ID monitor=(alpha);
  random gamma ~ normal(0, var=s2g) subject=ID monitor=(gamma);
  prior s2 ~ igamma(shape=1, scale=1);
  prior s2a ~ igamma(shape=1, scale=1);
  prior s2g ~ igamma(shape=1, scale=1);
  mu= beta0 + alpha + (beta1+gamma)*Xc;
  model W ~ Normal(mu,var=s2); 
  BEGINNODATA;
  Ac=beta0+(8-22)*beta1;
  ENDNODATA; 
run;
