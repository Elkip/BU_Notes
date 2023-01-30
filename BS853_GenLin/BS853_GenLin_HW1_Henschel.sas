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
	call streaminit(1);
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
/*
#      I,   the index;
#      A1,  the local selling prices, in hundreds of dollars;
#      A2,  the number of bathrooms;
#      A3,  the area of the site in thousands of square feet;
#      A4,  the size of the living space in thousands of square feet;
#      A5,  the number of garages;
#      A6,  the number of rooms;
#      A7,  the number of bedrooms;
#      A8,  the age in years;
#      A9,  1 = brick, 2 = brick/wood, 3 = aluminum/wood, 4 = wood.
#      A10, 1 = two story, 2 = split level, 3 = ranch
#      A11, number of fire places.
#      B,   the selling price.
*/
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
title Selling Price Correlation Scatterplot;
proc corr data=houses plots=matrix;
	var B logb A3 A6 A8;
run;

title Construction Type Comparision;
proc means data=houses;
	class A9;
	var B;
run;

title Arch Type Comparision;
proc means data=houses;
	class A10;
	var B;
run; 

*4 Consider a linear regression with selling price as outcome. Check model assumptions
and compute model diagnostics;
title 'Linear Regression of Housing Prices';
proc glm data=houses plots(unpack)=diagnostics;
	class A9 (ref=1) A10 (ref=1);
	model B=A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 / solution;
	output out=prices (keep=B r lev cd diffit df) rstudent=r h=lev cookd=cd dffits=dffit;
run;

ods select plots testsfornormality ;
proc univariate data=prices normal;
	var B r;
	histogram B r;
run ;

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
proc glm data=houses;
	class A9 (ref=1) A10 (ref=1);
	model logb=A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11;
	output out=logprices (keep=B r lev cd diffit df) rstudent=r h=lev cookd=cd dffits=dffit;
run;

proc reg data=houses;
	model logb=A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11;
	output out=logprices (keep=B r lev cd diffit df) rstudent=r h=lev cookd=cd dffits=dffit;
run;

ods select plots testsfornormality; 
proc univariate normal plot data=logprices; 
  var resdev; 
run;
