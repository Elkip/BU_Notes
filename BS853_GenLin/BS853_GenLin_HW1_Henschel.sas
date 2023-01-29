   /*
   Boston University - Biostatistics Department
   PROGRAM NAME          : BS853 HW1
   DESCRIPTION           : 1. Read data
                           2. Run some Generalized Linear Models
  -----------------------------------------------------------------------------
   Author : Mitchell Henschel 
   Date: 01/26/2023                
*/
libname HW1 'Z:\';

*EXERCISE 0;
/* Generate Predictors and outcome for 66 datasets*/
%let nSim=66;
%let b0=2.58;
%let b1=0.04;
%let b2=0.08;
%let b3=0.01;
%let sigma=1.06; * True residual standard deviation;
%let n=10; 		 * Assumed Sample size;

title Simulate more than one data;
data MANY;
	*Updated to use streaminit for seed declaration, rand('Uniform') for number gen.;
	call streaminit(315);
	do ID = 1 to &n;
		EN = rand('Uniform');
		Lit = rand('Uniform');
		US5 = rand('Uniform');
		do SIMULATION = 1 to &nSIM;
			Xb = &b0. + &b1.*EN + &b2.*Lit + &b3.*US5;
			INC = Xb + rand('normal')*&sigma;
			output;
		end;
	end;
run;

/* Analyze data */
proc sort data=MANY;
by SIMULATION;
run;

ods output ParameterEsimates = SE;
title "Simple linear regression of Income";
proc reg data=MANY;
by SIMULATION;
 model Inc=EN Lit US5;
run;

/* Summarize results */
title Parameter Estimates Summary (Multiple Simulation);
proc means data=SE mean STDERR;
VAR ESTIMATE;
Class VARIABLE;
WAYS 1;
run;

*EXERCISE 1;
*1 Identify outcome variables and predictor variables;
data houses;
	infile 'Z:\x26.txt';
	input ID A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 B;
	if missing(ID) then delete;
	logb = log(B);
 run;

*2 For each outcome variable discuss wether the assumption of normality is appropriate. 
Justify your response;
title 'Distribution of House Prices';
proc univariate data=houses;
	var B;
	histogram B;
run;

*3 In assessing the possible associations between the mean outcome and predictors.
Discuss weather a linear additive predictor is appropriate. Justify your response;
title 'Normal Distribution of House Prices';
ods output OBSTATS = check ;
proc genmod data=houses;
	model B=A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 / obstats;
run;

ods select plots testsfornormality ;
proc univariate normal plot data=check ;
var resdev ;
run ;

*4 Consider a linear regression with selling price as outcome. Check model assumptions
and compute model diagnostics;
title 'Linear Regression of Housing Prices';
proc reg data=houses;
	model B=A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11;
run;

*5 Repeat the 4 using log(selling price) as outcome. Interpret the coefficents;
title Bayesian Analysis of Linear Model with Log(Housing Price);
ods output OBSTATS=check;
proc genmod data=houses;
    model B=A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 / dist=normal link=log;
    bayes seed=1 OutPost=PostSurg;
run;

ods select plots testsfornormality; 
proc univariate normal plot data=check; 
  var resdev; 
run;

title 'Linear Regression of Log(Housing Prices)';
proc reg data=houses;
	model logb=A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11;
run;
