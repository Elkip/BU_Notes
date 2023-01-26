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
*EXERCISE 1;
*1 Identify outcome variables and predictor variables;
data houses;
	infile 'Z:\x26.txt';
	input ID A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 B;
	if missing(ID) then delete;
 run;

*2 For each outcome variable discuss wether the assumption of normality is appropriate. 
Justify your response;

*3 In assessing the possible associations between the mean outcome and predictors.
Discuss weather a linear additive predictor is appropriate. Justify your response;

*4 Consider a linear regression with selling price as outcome. Check model assumptions
and compute model diagnostics;

*5 Repeat the 4 using log(selling price) as outcome. Interpret the coefficents;
