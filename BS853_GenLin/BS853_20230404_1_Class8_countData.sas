  /*soh************************************************************************                                                        
   Boston University - Biostatistics Department                                                                                         
   PROGRAM NAME          : script - Lecture 8.sas                                                                                
                           (.\BS853\Class 8)                                        
   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
                           - Lecture: Generalized Linear Models for count data                                                          
                                                                                                                                        
   DESCRIPTION           : 1. Read data                                                                                                 
                           2. Testing in Generalized Linear Models                                                    
                                                                                                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3                                                                                              
   INFRASTRUCTURE        : Windows                                                                                                      
   INPUT                 :                                                                                                              
   OUTPUT                :                                                                                                              
  -----------------------------------------------------------------------------                                                         
   Author : Gheorghe Doros                                                                                                              
   Last modified 03/22/2022                                                                                                             
 **eoh************************************************************************/                                                         
options ps=60 ls=89 pageno=1 nodate;                                                  

data claims;
do age=1 to 4;
input district car C N @@;
logN=log(N);
output;
end;
cards;
1 1 38 197 1 1 35 264 1 1 20 246 1 1 156 1680
1 2 63 284 1 2 84 536 1 2 89 696 1 2 400 3582
1 3 19 133 1 3 52 286 1 3 74 355 1 3 233 1640
1 4  4  24 1 4 18  71 1 4 19  99 1 4  77  452
2 1 22  85 2 1 19 139 2 1 22 151 2 1  87  931
2 2 25 149 2 2 51 313 2 2 49 419 2 2 290 2443
2 3 14  66 2 3 46 175 2 3 39 221 2 3 143 1110
2 4  4   9 2 4 15  48 2 4 12  72 2 4  53  322
3 1  5  35 3 1 11  73 3 1 10  89 3 1  67  648
3 2 10  53 3 2 24 155 3 2 37 240 3 2 187 1635
3 3  8  24 3 3 19  78 3 3 24 121 3 3 101  692
3 4  3   7 3 4  2  29 3 4  8  43 3 4  37  245
4 1  2  20 4 1  5  33 4 1  4  40 4 1  36  316
4 2  7  31 4 2 10  81 4 2 22 122 4 2 102  724
4 3  5  18 4 3  7  39 4 3 16  68 4 3  63  344
4 4  0   3 4 4  6  16 4 4  8  25 4 4  33  114
run;

options ls=97 pageno=1 nodate;
title1 'Claims data';
title2 'Only intercept model(1)';
ods select ModelFit ParameterEstimates;
proc genmod data=claims; 
 class district car age; 
 model C=/offset=logN dist=poisson link=log obstats; 
run;

title2 'Only district effect model(2)';
ods select ModelFit ParameterEstimates;
proc genmod data=claims; 
 class district car age; 
 model C=district/offset=logN dist=poisson link=log obstats; 
run;

title2 'District and Car main effects model(3)';
ods select ModelFit ParameterEstimates;
proc genmod data=claims; 
 class district car age; 
 model C=district car/offset=logN dist=poisson link=log obstats; 
run;

title2 'All main effects model(4)';
ods select ModelFit ParameterEstimates ObStats estimates;
ods output ObStats=check;
proc genmod data=claims; 
 class district car age; 
 model C=district car age/offset=logN dist=poisson link=log obstats;
 estimate 'district 1 vs. 4' district 1 0 0 -1/exp; 
run;

title2 "All main effects model(4') - Check coefficient for offset=1";
ods select Estimates;
proc genmod data=claims; 
 class district car age; 
 model C=district car age logN/ dist=poisson link=log; 
run;


/* We expect the effect of car to vary by age */
title2 'Effect of Age and Car independent of District(5)';
ods select ModelFit ParameterEstimates;
proc genmod data=claims; 
 class district car age; 
 model C=district age|car/offset=logN dist=poisson link=log obstats; 
run;

title2 'District and age independent given Car (6)';
ods select ModelFit ParameterEstimates;
proc genmod data=claims; 
 class district car age; 
 model C=district|car age|car/offset=logN dist=poisson link=log obstats; 
run;

title2 'All 2-way interactions(7)';
*ods select ModelFit ParameterEstimates;
proc genmod data=claims; 
 class district car age(ref='1')/param=ref; 
 model C=district|age|car @2/offset=logN dist=poisson link=log covb; 
run;


options pageno=1 nodate;
ods select normal plots;
title2 ' Check normality of the Chi-Squared residuals (Model 4)';
proc univariate data=check normal plot;
  var reschi resdev;
run;

 
data absences;  
input days C S A L @@;  
datalines;  
 2 1 1 1 1  11 1 1 1 1  14 1 1 1 1   5 1 1 1 2 
 5 1 1 1 2  13 1 1 1 2  20 1 1 1 2  22 1 1 1 2 
 6 1 1 2 1   6 1 1 2 1  15 1 1 2 1   7 1 1 2 2 
14 1 1 2 2   6 1 1 3 1  32 1 1 3 1  53 1 1 3 1 
57 1 1 3 1  14 1 1 3 2  16 1 1 3 2  16 1 1 3 2  
17 1 1 3 2  40 1 1 3 2  43 1 1 3 2  46 1 1 3 2 
 8 1 1 4 2  23 1 1 4 2  23 1 1 4 2  28 1 1 4 2 
34 1 1 4 2  36 1 1 4 2  38 1 1 4 2   3 1 2 1 1  
 5 1 2 1 2  11 1 2 1 2  24 1 2 1 2  45 1 2 1 2  
 5 1 2 2 1   6 1 2 2 1   6 1 2 2 1   9 1 2 2 1 
13 1 2 2 1  23 1 2 2 1  25 1 2 2 1  32 1 2 2 1  
53 1 2 2 1  54 1 2 2 1   5 1 2 2 2   5 1 2 2 2  
11 1 2 2 2  17 1 2 2 2  19 1 2 2 2   8 1 2 3 1  
13 1 2 3 1  14 1 2 3 1  20 1 2 3 1  47 1 2 3 1  
48 1 2 3 1  60 1 2 3 1  81 1 2 3 1   2 1 2 3 2 
 0 1 2 4 2   2 1 2 4 2   3 1 2 4 2   5 1 2 4 2  
10 1 2 4 2  14 1 2 4 2  21 1 2 4 2  36 1 2 4 2  
40 1 2 4 2   6 2 1 1 1  17 2 1 1 1  67 2 1 1 1 
 0 2 1 1 2   0 2 1 1 2   2 2 1 1 2   7 2 1 1 2 
11 2 1 1 2  12 2 1 1 2   0 2 1 2 1   0 2 1 2 1 
 5 2 1 2 1   5 2 1 2 1   5 2 1 2 1  11 2 1 2 1  
17 2 1 2 1   3 2 1 2 2   4 2 1 2 2  22 2 1 3 1 
30 2 1 3 1  36 2 1 3 1   0 2 1 3 2   1 2 1 3 2  
 5 2 1 3 2   7 2 1 3 2   8 2 1 3 2  16 2 1 3 2  
27 2 1 3 2   0 2 1 4 2  10 2 1 4 2  14 2 1 4 2  
27 2 1 4 2  30 2 1 4 2  41 2 1 4 2  69 2 1 4 2  
25 2 2 1 1  10 2 2 1 2  11 2 2 1 2  20 2 2 1 2  
33 2 2 1 2   0 2 2 2 1   1 2 2 2 1   5 2 2 2 1  
 5 2 2 2 1   5 2 2 2 1   5 2 2 2 1   5 2 2 2 1  
 7 2 2 2 1   7 2 2 2 1  11 2 2 2 1  15 2 2 2 1  
 5 2 2 2 2   6 2 2 2 2   6 2 2 2 2   7 2 2 2 2 
14 2 2 2 2  28 2 2 2 2   0 2 2 3 1   2 2 2 3 1  
 2 2 2 3 1   3 2 2 3 1   5 2 2 3 1   8 2 2 3 1  
10 2 2 3 1  12 2 2 3 1  14 2 2 3 1   1 2 2 3 2  
 1 2 2 4 2   3 2 2 4 2   3 2 2 4 2   5 2 2 4 2  
 9 2 2 4 2  15 2 2 4 2  18 2 2 4 2  22 2 2 4 2  
22 2 2 4 2  37 2 2 4 2  
; 
run;
proc freq data=absences;table days;run;
proc gchart data=absences;vbar days/discrete;run;
options ls=97 nodate pageno=1; 
title1' Means and Variances for each cell ';
proc means data=absences n mean var maxdec=2;
 class  C S A L; 
 var days;
 ways 4;
run; 

/*Model 1*/
title1 ' Model 1 - All 4 way interaction model - Poisson Regression ';
ods select modelfit;
proc genmod data=absences;  
  class C S A L/param=ref;  /*param=ref - to get rid of the bulky output*/
  model days=C|S|A|L/link=log dist=poisson;  
run;  

title1 ' Model 1 - All 4 way interaction model - Negative Binomial Regression ';
ods select modelfit;
proc genmod data=absences;  
  class C S A L/param=ref;  /*param=ref - to get rid of the bulky output*/
  model days=C|S|A|L/link=log dist=NB;  
run;  

/* Model 2 */ 
title1 ' Model 1 - Poisson Regression with Overdispersion (1)';
proc genmod data=absences;  
  class C S A L/param=ref;  
  model days=C|S|A|L/link=log dist=poisson pscale;  
run; 

/* Model 3 */ 
title1 ' Model 1 - Poisson Regression with Overdispersion (2)';
proc genmod data=absences;  
  class C S A L/param=ref;  
  model days=C|S|A|L/ link=log dist=poisson scale=3.085;  
run;

/* Model 1' */ 
title1 ' Model 1 - Poisson Regression with Overdispersion (Model 1) ';
ods select ModelFit type3;
proc genmod data=absences;  
 class C S A L/param=ref;  
 model days=C|S|A|L/ link=log dist=poisson scale=3.085 type3;  
run;

/* Model 2' */ 
title1 ' All 3 way interaction model - Poisson Regression with Overdispersion (Model 2) ';
ods select ModelFit type3;
proc genmod data=absences;  
 class C S A L/param=ref;  
 model days=C|S|A|L @3/ link=log dist=poisson scale=3.085  type3;  
run;

/* Model 3' */ 
title1 ' All 2 way interaction model - Poisson Regression with Overdispersion (Model 3) ';
ods select ModelFit type3;
proc genmod data=absences;  
 class C S A L/param=ref;  
 model days=C|S|A|L @2/ link=log dist=poisson scale=3.085  type3;  
run;

/* Model 4' */ 
title1 ' Poisson Regression with Overdispersion (Model 4) ';
ods select ModelFit type3;
proc genmod data=absences;  
 class C S A L/param=ref;  
 model days=C|A|L C|S A|S L|S/ link=log dist=poisson scale=3.085  type3;  
run;

/* Model 5' */ 
title1 ' Poisson Regression with Overdispersion (Model 5) ';
ods select ModelFit type3 parameterestimates;
proc genmod data=absences;  
 class C S A L/param=ref;  
 model days=C|A|L C|S|L A|S/ link=log dist=poisson scale=3.085  type3;  
run;

/* Model 6' */ 
title1 ' Poisson Regression with Overdispersion (Model 6) ';
ods select ModelFit type3;
proc genmod data=absences;  
 class C S A L/param=ref;  
 model days=C|A|L C|S|L C|A|S/ link=log dist=poisson scale=3.085  type3;  
run;

/* Model 7' */ 
title1 ' Poisson Regression with Overdispersion (Model 7) ';
ods select ModelFit type3;
proc genmod data=absences;  
 class C S A L/param=ref;  
 model days=C|A|L C|S|L A|S|L/ link=log dist=poisson scale=3.085  type3;  
run;

options ls=90;
title1 'Zero Inflated Poisson Regression (Model 7)';
proc genmod data = absences;
  class C S A L/param=ref;  
  model days = S|C|A|L @2/dist=zip;
  zeromodel C S /link = logit ;
run;
/* rates of initial glaucoma treatment among elderly New Jersey Medicaid enrollees
during the 1980*/

data incidence;
input Age $ Cases  	Person_Years race $;
rate=log(Cases/Person_years);
logT=log(Person_years);
cards;
65-69  	390 	25324  Black
70-74  	346  	21522  Black
75-79  	277  	17131  Black
80-84  	229  	12330  Black
85-99  	174  	11846  Black
65-69  	589  	65450  White
70-74  	635  	69031  White
75-79  	759  	64939  White
80-84  	618 	58636  White
85-99  	1017  	90047  White
;
run;


/* Construct plots to study the relationship of the outcomes with age */

 goptions reset=all ftext='arial' htext=1.2 hsize=9.5in vsize=7in aspect=1 horigin=1.5in vorigin=0.3in 
          device=emf rotate=landscape gsfname=TempOut1 gsfmode=replace;

filename TempOut1 "C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 6\treat.emf";  

title1 'Glaucoma Treatment';
symbol1 v=dot c=black;
symbol2 v=circle;
axis1 label=('AGE');
axis2 label=(a=90 'Log rate') minor=none ;
legend1 label=('Race');
proc gplot data=incidence;
 plot rate*age=race/vaxis=axis2 haxis=axis1 legend=legend1;
 run;
quit;
options nodate pageno=1 ls=97;
title2 ' Using GENMOD -only main effects model ';
ods select parameterestimates modelfit;
proc genmod data=incidence;
 class age race/param=ref;
 model cases = age race/dist=poisson link=log offset=logT;
run;

title2 ' Using GENMOD -saturated model ';
ods select parameterestimates modelfit estimates;
proc genmod data=incidence;
class age race;
 model cases = age|race/dist=poisson link=log offset=logT;
 estimate 'B vs W in age 1' race 1 -1 age*race 1 -1/exp;
 estimate 'B vs W in age 2' race 1 -1 age*race 0 0 1 -1/exp;
 estimate 'B vs W in age 3' race 1 -1 age*race 0 0 0 0 1 -1/exp;
 estimate 'B vs W in age 4' race 1 -1 age*race 0 0 0 0 0 0 1 -1/exp;
 estimate 'B vs W in age 5' race 1 -1 age*race 0 0 0 0 0 0 0 0 1 -1/exp;
run;

options pageno=1 ls=87;
title2 ' Using GENMOD - Poisson Regression ';
ods select Modelfit;
proc genmod data=incidence;
 class age race;
 model cases = age race /dist=poisson link=log offset=logT;
run;

title2 ' Using GENMOD - Negative Binomial Regression ';
proc genmod data=incidence;
 class age race;
 model cases = age race /dist=NB link=log offset=logT;
run;


