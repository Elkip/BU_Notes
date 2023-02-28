  /*soh************************************************************************                                                        
   Boston University - Biostatistics Department                                                                                         
   PROGRAM NAME          : script - Logistic Regression Models.sas                                                                                
                           (Z:/Documents/Class 5)                                        
   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
                           - Lecture: Generalized Linear Models                                                                         
                                                                                                                                        
   DESCRIPTION           : 1. Read data                                                                                                 
                           2. Testing in Generalized Linear Models                                                    
                                                                                                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3                                                                                              
   INFRASTRUCTURE        : Windows                                                                                                      
   INPUT                 :                                                                                                              
   OUTPUT                :                                                                                                              
  -----------------------------------------------------------------------------                                                         
   Author : Gheorghe Doros                                                                                                                                                                                                                        
 **eoh************************************************************************/                                                         

%let path=Z:/Documents/Class 5;
/**************************************************************/
/* Admission Data                                             */
/**************************************************************/
/* Ignoring Department */
data overall; 
input sex $ yes total; 
cards; 
	M 1198  2691 
	F   557  1835 
; 
/* Saturated model: with class statement*/;  
ODS select modelfit ParameterEstimates;
title 'Differential admission by Gender';
proc genmod data=overall; 
class sex; 
model yes/total=sex/link=logit dist=bin obstats; 
estimate 'overall gender' sex 1 -1/exp;
run; 

proc genmod data=overall; 
class sex; 
model yes/total=/link=logit dist=bin obstats; 
run; 
/* By Department */
data one;
do Department=1 to 6;
do Sex='M', 'F';;
input yes no;
logitp=log(yes/(yes+no));
total=yes+no;
output;
end;
end;
cards;
512	313
89	19
353  	207
17     	8
120  	205
202  	391
138  	279
131 	244
53  	138
94  	299
22  	351
24  	317
;run;

/* Explain why marginally women seem to be discriminated against
   - More women apply to harder to enter colodges!!!             */

proc sql; create table a as
select department, sum(total) as total , sum(yes)/sum(total) as adminp from one group by department  order by department;
 create table b as select department, total as fem from one where sex='F' order by department;
 create table c as
 select *, fem/total as pf from a as aa, b as bb where aa.department=bb.department;
quit;

 symbol1 v=circle i=join;
 proc gplot data=c;
 plot pf*adminp;
run;


goptions reset=all ftext='arial' htext=1.2 hsize=9.5in vsize=7in aspect=1 horigin=1.5in vorigin=0.3in ;
goptions device=emf rotate=landscape gsfname=TempOut1 gsfmode=replace;
  		filename TempOut1 "C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 4\admission.emf";  
		symbol1 v=dot i=j ci=red;
		symbol2 v=circle i=j ci=blue;
		axis1 label=(h=1.5 'Department') minor=none order=(1 to 6 by  1)  offset=(1);
axis2 label=(h=1.5  a=90 'Logit(Rate)') minor=none ;

title1 'Admitance by Department and Sex';
proc gplot data=one;
plot logitp*Department=sex/vaxis=axis2 haxis=axis1;
run;

title1 'Saturated Model';
proc genmod data=one;
class department sex;
model yes/total=sex|department/dist=b link=logit;
run;

title1 'Only main effects Model';
proc genmod data=one;
class department sex;
model yes/total=sex department/dist=b link=logit;
run;

title1 'Only Sex Model';
proc genmod data=one;
class department sex;
model yes/total=sex/dist=b link=logit;
estimate 'l' sex 1 -1/exp;
run;

title1 'Only Department effects Model';
proc genmod data=one;
class department sex;
model yes/total=department/dist=b link=logit;
run;

ods select estimates;
title1 'Estimated ODDS Ratio (F vs M) in each Department';
proc genmod data=one;  
class department sex;  
model yes/total=sex|department/link=logit dist = bin covb; 
estimate 'sex1'  sex 1 -1  department*sex 1 -1 0  0 0  0 0  0 0  0 0  0    /exp ; 
estimate 'sex2'  sex 1 -1  department*sex 0  0 1 -1 0  0 0  0 0  0 0  0    /exp ;  
estimate 'sex3'  sex 1 -1  department*sex 0  0 0  0 1 -1 0  0 0  0 0  0    /exp ;  
estimate 'sex4'  sex 1 -1  department*sex 0  0 0  0 0  0 1 -1 0  0 0  0    /exp ;  
estimate 'sex5'  sex 1 -1  department*sex 0  0 0  0 0  0 0  0 1 -1 0  0    /exp ;  
estimate 'sex6'  sex 1 -1  department*sex 0  0 0  0 0  0 0  0 0  0 1 -1    /exp ;   
run; 

/* Modeling Trends in Proportions */
data refcanc;
do age=1 to 4;
input cases ref;
total=cases+ref;
output;
end;
cards;
26	20
4	7
3	10
1	11
;
run;

/* only intercept */
title1 'Only Intercept model';
proc genmod data=refcanc;
class age;
model cases/total=;
run;

/* Saturated model */
title1 'Saturated model';
proc logistic data=refcanc;
class age;
model cases/total=age;
run;
title1 'Linear trend model';

proc logistic data=refcanc;
model cases/total=age;
run;

/* CHD in Framingham Study*/
data chd;  
input Chol SBP CHD Total;  
cards; 
1 1 2  119
1 2 3  124
1 3 3  50 
1 4 4  26 
2 1 3  88 
2 2 2  100
2 3 0  43 
2 4 3  23 
3 1 8  127
3 2 11 220
3 3 6  74 
3 4 6  49 
4 1 7  74 
4 2 12 111
4 3 11 57 
4 4 11 44 
;
run;

title1 'Only main Effects';
ods output obstats=check;
proc genmod data=CHD;
class CHOL SBP;
model CHD/Total= CHOL SBP /dist=Binomial link=logit covb;
run;

/*We are interested in the ODDS Ratio for remission comparing the first labeling index group ('8-12') 
to the last labeling index group ('34-38')*/
options nodate pageno=1;
data remission;
input LLI $ 1-5   total remiss grp;
cards;
 8-12 7 1 1
14-18 7 1 2
20-24 6 3 3
26-32 3 2 4
34-38 4 3 5
run;

proc format;
value $a '8-12' = '1' '14-18'='2' '20-24'='3' '26-32'='4' '34-38'='5';
run;


title ' Default order - Alphanumeric ordering ';
title2 ' GLM Coding ';
ods select classlevels parameterestimates contrasts;
proc genmod data=remission;
class LLI;
model remiss/total=LLI;
contrast '34-38 vs. 8-12' LLI 0 0 0 1 -1;
contrast '34-38 vs. 8-12' LLI 0 0 0 1 -1/wald;
run;

title ' Desired order - Using proc format ';
title2 ' GLM Coding ';
ods select classlevels parameterestimates contrasts;
proc genmod data=remission;
format LLI $a.;
class LLI;
model remiss/total=LLI;
contrast '34-38 vs. 8-12' LLI -1 0 0 0 1;
contrast '34-38 vs. 8-12' LLI -1 0 0 0 1/wald;
run;


title ' Changing Default coding in proc genmod';
title2 ' Change to Reference Coding ';
ods select classlevels parameterestimates contrasts;
proc genmod data=remission;
class LLI(ref='20-24')/param=ref;
model remiss/total=LLI;
contrast '34-38 vs. 8-12' LLI  0 0 1 -1;
run;

title ' Changing Default coding in proc genmod';
title2 ' Change to Effect Coding ';



ods select classlevels parameterestimates contrasts;
proc genmod data=remission;
class LLI/param=effect;
model remiss/total=LLI;
contrast '34-38 vs. 8-12' LLI  1 1 1 2;
run;

title ' Default coding in proc logistic';
ods select classlevelinfo parameterestimates estimates;
proc logistic data=remission;
class LLI;
model remiss/total=LLI;
estimate '34-38 vs. 8-12' LLI  1 1 1 2;
run;

title ' Changing Default coding in proc logistic';
title2 ' Change to GLM Coding (less than full rank) ';
ods select classlevelinfo  parameterestimates estimates;
proc logistic data=remission;
class LLI/param=GLM;
model remiss/total=LLI;
estimate '34-38 vs. 8-12' LLI  0 0 0 1 -1;
run;

title ' Changing Default coding in proc logistic';
title2 ' Change to Reference Coding ';
ods select classlevelinfo parameterestimates estimates;
proc logistic data=remission;
class LLI(ref='20-24')/param=ref;
model remiss/total=LLI;
estimate '34-38 vs. 8-12' LLI  0 0 1 -1;
run;


title1 'Two contrasts for multiple hypotheses';
ods select parameterestimates contrasts;
proc genmod data=remission;
class LLI;
model remiss/total=LLI;                              
contrast '34-38 vs. 14-18 vs. 8-12' LLI 0 0 0 1 -1, /*(H01)*/
                                    LLI 1 0 0 0 -1; /*(H02)*/ 
contrast '34-38 vs. 14-18 vs. 8-12' LLI 0 0 0 1 -1, /*(H01)*/
                                    LLI 1 0 0 0 -1/wald; /*(H02)*/ 
run;

/***************************************************************/

options ps=60 ls=89 pageno=1 nodate;                                                  

/* CHD in Framingham Study*/
proc format ;
value sbp 1='<127' 	2='127 - 146' 	3='147 - 166' 	4='167+'; 
value chl 1='<200'  2='200 - 219'   3='220 - 259'   4='>=260';
run;
data chd;  
input Chol SBP CHD Total;  
format sbp sbp. Chol chl.;
cards; 
1 1 2  119 
1 2 3  124 
1 3 3  50 
1 4 4  26 
2 1 3  88 
2 2 2  100 
2 3 0  43 
2 4 3  23 
3 1 8  127 
3 2 11 220 
3 3 6  74 
3 4 6  49  
4 1 7  74 
4 2 12 111 
4 3 11 57 
4 4 11 44 
;
run;
title1 'Contrasting <200 vs. 220-259 with CONTRAST and ESTIMATE statements';
ods output contrasts=contrast1 estimates=estimates1(drop=MeanEstimate MeanLowerCL MeanUpperCL);
proc genmod data=CHD;
class CHOL SBP;
model CHD/Total= CHOL SBP /dist=Binomial link=logit;
estimate '<200 vs. 220-259' CHOL 0 -1 1 0/exp;
contrast '<200 vs. 220-259' CHOL 0 -1 1 0;
run;
proc print data=estimates1 noobs label;run;

options pageno=1;
title1 'Only main Effects';
ods select estimates contrasts parminfo covB parameterestimates classlevels;
proc genmod data=CHD;
class CHOL SBP;
model CHD/Total= CHOL SBP /dist=Binomial link=logit covb;
contrast '<200 vs. 220-259' chol  0 -1  1 0;
estimate '<200 vs. 220-259' chol  0 -1  1 0/exp;
contrast '<127 and 147-166 vs. 127-14' SBP -1 0.5 0 0.5;
estimate '<127 and 147-166 vs. 127-14' SBP -1 0.5 0 0.5/exp;
run;



/**************************************************************/
/* Admission Data                                             */
/**************************************************************/

/* By Department */
data gradadmission;
do Department=1 to 6;
 do Sex='M', 'F';;
  input yes no;
  total=yes+no;
  output;
 end;
end;
cards;
512	313
89	19
353  	207
17     	8
120  	205
202  	391
138  	279
131 	244
53  	138
94  	299
22  	351
24  	317
;run;
/*Eliminate the mean estimates */
proc template;
edit stat.genmod.estimates;
   column Label LBetaEstimate StdErr Alpha LBetaLowerCL LBetaUpperCL ChiSq ProbChiSq;
end;   
run;
/*proc template; 
   delete stat.genmod.estimates;
run;*/
options pageno=1;
title1 'Estimated ODDS Ratio (F vs M) in each Department';
ods select estimates parminfo covB parameterestimates classlevels;
proc genmod data=gradadmission;  
class department sex;  
model yes/total=sex|department/link=logit dist = bin covb; 
estimate 'sex1'  sex 1 -1  department*sex 1 -1 0  0 0  0 0  0 0  0 0  0    /exp ; 
estimate 'sex2'  sex 1 -1  department*sex 0  0 1 -1 0  0 0  0 0  0 0  0    /exp ;  
estimate 'sex3'  sex 1 -1  department*sex 0  0 0  0 1 -1 0  0 0  0 0  0    /exp ;  
estimate 'sex4'  sex 1 -1  department*sex 0  0 0  0 0  0 1 -1 0  0 0  0    /exp ;  
estimate 'sex5'  sex 1 -1  department*sex 0  0 0  0 0  0 0  0 1 -1 0  0    /exp ;  
estimate 'sex6'  sex 1 -1  department*sex 0  0 0  0 0  0 0  0 0  0 1 -1    /exp ;   
run; 

/***************************************************/
/*** Treatment for Urinary Tract Infection (UTI) ***/
/***************************************************/

data UTI;
   input diag : $6. TRT $ resp $ count @@;
   datalines;
comp    A  Yes 78  comp   A No 28
comp    B  Yes 101 comp   B No 11
comp    C  Yes 68  comp   C No 46
uncomp  A  Yes 40  uncomp A No 5
uncomp  B  Yes 54  uncomp B No 5
uncomp  C  Yes 34  uncomp C No 6
;

options pageno=1 ls=90;
title1 'Effectiveness of three different regimens for treatment of UTI';
ods select estimates parminfo covB parameterestimates classlevels;
proc genmod data=UTI;
class diag TRT resp;
model count = diag|TRT|resp @2/dist=p covb type3;
   estimate 'TR A vs C' TRT*resp 1 -1  0  0 -1 1/exp;
   estimate 'TR A vs B' TRT*resp 1 -1 -1  1  0 0/exp;
   estimate 'TR B vs C' TRT*resp 0  0  1 -1 -1 1/exp;
run;

/********************************************************************************/
/***  Breathlessness and wheeze in coal miners Ashford and Sowden (1970)      ***/
/********************************************************************************/

/* read in data to use for plots */
data coalminr ;
input age brthlss $ wheeze count @@;
cage=age-5;   
sqage=cage**2;
brthlss1=2-(brthlss='yes');
cards;
1	yes	1	9	1	yes	2	7
1	 no	1	95	1	 no	2	1841
2	yes	1	23	2	yes	2	9
2	 no	1	105	2	 no	2	1654
3	yes	1	54	3	yes	2	19
3	 no	1	177	3	 no	2	1863
4	yes	1	121	4	yes	2	48
4	 no	1	257	4	 no	2	2357
5	yes	1	169	5	yes	2	54
5	 no	1	273	5	 no	2	1778
6	yes	1	269	6	yes	2	88
6	 no	1	324	6	 no	2	1712
7	yes	1	404	7	yes	2	117
7	 no	1	245	7	 no	2	1324
8	yes	1	406	8	yes	2	152
8	 no	1	225	8	 no	2	967
9	yes	1	372	9	yes	2	106
9	 no	1	132	9	 no	2	526
;
run;

/* read in data to use for plots */
data plot;
infile cards pad missover;
input age brthlss $ wheeze county @29 countn @33 OR;
cage=age-5;   
sqage=cage**2;
totalw=wy+wn;
r=county/(county+countn);
logitwp=log(r/(1-r));
cards;
1	yes	1	9	1	yes	2	7	24.91578947
1	 no	1	95	1	 no	2	1841			
2	yes	1	23	2	yes	2	9	40.25608466
2	 no	1	105	2	 no	2	1654			
3	yes	1	54	3	yes	2	19	29.91436218
3	 no	1	177	3	 no	2	1863			
4	yes	1	121	4	yes	2	48	23.11908236
4	 no	1	257	4	 no	2	2357			
5	yes	1	169	5	yes	2	54	20.38271605
5	 no	1	273	5	 no	2	1778			
6	yes	1	269	6	yes	2	88	16.15207632
6	 no	1	324	6	 no	2	1712			
7	yes	1	404	7	yes	2	117	18.66024769
7	 no	1	245	7	 no	2	1324			
8	yes	1	406	8	yes	2	152	11.47959064
8	 no	1	225	8	 no	2	967			
9	yes	1	372	9	yes	2	106	13.98456261
9	 no	1	132	9	 no	2	526			
;
run;


/* Construct plots to study the relationship of the outcomes with age */

 goptions reset=all ftext='arial' htext=1.2 hsize=9.5in vsize=7in aspect=1 horigin=1.5in vorigin=0.3in 
          device=emf rotate=landscape gsfname=TempOut1 gsfmode=replace;

filename TempOut1 "&path/miners.emf";  
symbol1 v=dot i=none cv=blue;
symbol2 v=square i=none cv=green;
title1 h=1.2 'Probit Weeze rate by AGE'; 
axis1 label=('AGE')  minor=none order=(1 to 9 by 1) value=(tick=1 '20-24' tick=2 '25-29' tick=3 '30-34'
tick=4 '35-39' tick=5 '40-44' tick=6 '45-49' tick=7 '50-54' tick=8 '55-59' tick=9 '60-64');
axis2 label=(a=90 'Probit Weeze rate') minor=none ;
legend1 label=('Breathlesness');
proc gplot data=plot;
  plot logitwp*age=brthlss/vaxis=axis2 haxis=axis1 legend=legend1;
run;

filename TempOut1 "&path/OR.emf";  
title1 h=1.2 'OR of Wheeze and Breathlessness by AGE'; 
axis2 label=(a=90 'OR of Wheeze and Breathlessness') minor=none ;
proc gplot data=plot;
 plot OR*age=brthlss/vaxis=axis2 haxis=axis1 nolegend;
run;

title1 'CMH test for conditional independence';
proc freq data=coalminr;
  table age*brthlss*wheeze/cmh chisq;
  weight count;
run;

/*Create data sets for studying the relation of wheeze and breathlessmess with age*/
proc sql;
  create table wheeze as
   select distinct age, sum(county) as wy, sum(countn) as wn,  sum(county)+ sum(countn) as totalw, cage, sqage from plot group by age;
  create table breathlessness1 as
   select distinct age, brthlss,sum(count) as count, cage, sqage from coalminr group by age, brthlss order by age, brthlss;
quit;

proc transpose data=breathlessness1 out = breathlessness prefix=b;
  var count;
  by age;
  id  brthlss;
  copy cage sqage;
run;
data breathlessness;
  set breathlessness;
 totalb=byes+bno;
run;

options pageno=1 ls=97;
/*Model 1*/
title1 'Predicting Breathlessness rates with age';
title2 'Only linear effect of age in the model';
proc genmod data=breathlessness;
  model byes/totalb=cage/ link=logit dist=bin obstats;
run;

/*Model 2*/
title1 'Predicting Breathlessness rates with age';
title2 'Linear and quadratic effects of age in the model';
proc genmod data=breathlessness;
  model byes/totalb=cage sqage/ link=logit dist=bin obstats;
run;

/*Model 3*/
title1 'Predicting Wheeze rates with age';
title2 'Only linear effect of age in the model';
proc genmod data=wheeze;
  model wy/totalw=cage/ link=logit dist=bin obstats;
run;

title1 'Predicting Breathlessness rates with age';
title2 'Linear and quadratic effects of age in the model';
title3 'Use estimate to compare age groups';
ods select estimates parminfo parameterestimates ;
proc genmod data=breathlessness;
  model byes/totalb=cage sqage/ link=logit dist=bin obstats;
  estimate 'agea' cage -4 sqage 16/exp; 
run;

/***********************************************************/
/*** Use Loglinear models to analyse the data ***/ 
 /***********************************************************/
title1 'All 2 way interaction model';
proc genmod data=coalminr;
  class age brthlss1 wheeze;
  model count=age|brthlss1|wheeze @2/dist=p;
run;



title1 'Allowing OR between Wheeze and Breathlessness to vary with age';
proc genmod data=coalminr;
  class age brthlss1 wheeze;
  model count=age|brthlss1 age|wheeze cage*brthlss1*wheeze brthlss1*wheeze/dist=p;
run;
