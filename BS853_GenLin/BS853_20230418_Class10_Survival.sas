  /*soh************************************************************************                                                         
   Boston University - Biostatistics Department                                                                                         
   PROGRAM NAME          : script - Time to event.sas                                                                                   
                           (/Documents/Projects/BS853/Class 10)                                                                         
   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
                           - Lecture: Survival methods                                                                                  
                                                                                                                                        
   DESCRIPTION           : 1. Read data                                                                                                 
                           2. time to event Models                                                                                      
                                                                                                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3                                                                                              
   INFRASTRUCTURE        : Windows                                                                                                      
   INPUT                 :                                                                                                              
   OUTPUT                 :                                                                                                              
  -----------------------------------------------------------------------------                                                         
   Author : Gheorghe Doros                                                                                                              
   Last modified                                                                                                           
 **eoh************************************************************************/                                                         
options ps=60 ls=89 pageno=1 nodate;                                                                                                    

/* Compare incidence rates */
Data one;
Input Group C Time;
LogT=log(Time);
Cards;
1 17 4095
2 19 5023
;
run;
 
proc genmod data=one;
class Group;
 Model C=Group/d=p offset=LogT;
 Estimate 'Group 1' Intercept 1 Group 1 0/exp;
 Estimate 'Group 2' Intercept 1 Group 0 1/exp;
 Estimate 'Group 2 vs 1' Group -1 1/exp;
Run;
/*Comparing survival using CMH test and 3 time strata*/
data RCMH;
  do group=1 to 2;
  Do time=1 to 3;
   input yes no @@;
   output;
  end;
  end;
cards;
 6 13 9 4 1  1 
 4 17 9 8 5  2 
run;
proc sort data=RCMH;
 by group time;
run;
proc transpose data=RCMH out=RCMH2 prefix=count name=died;
 by  group time;
run;
/*Comparing survival using CMH test and 3 time strata*/
title1 'Comparing survival using CMH test and 3 time strata';
options pageno=1 ps=57 ls=91 center nodate;
proc freq data=RCMH2 order=data;
  table time*group*died/cmh chisq relrisk;
  weight count1;
run;

/*Comparing Survival with Log-Rank test */
data ratc I1 I2 I3;
input group day censor @@;
if  0<=day<=200 then do;
DayI1=day;Day1=1-censor;
end;
if  201<=day<=250 then do;
DayI1=200;Day1=0;
DayI2=day-200;Day2=1-censor;
end;
if  251<=day<=300 then do;
DayI1=200;Day1=0;
DayI2=50;Day2=0;
DayI3=day-250;Day3=1-censor;
end;
if  301<=day then do;
DayI1=200;Day1=0;
DayI2=50;Day2=0;
DayI3=50;Day3=0;
end;
logt=log(day);
cards;
1   143 0 1   220 0 2   156 0 2   239 0 
1   164 0 1   227 0 2   163 0 2   240 0 
1   188 0 1   230 0 2   198 0 2   261 0 
1   188 0 1   234 0 2   205 0 2   280 0 
1   190 0 1   246 0 2   232 0 2   280 0 
1   192 0 1   265 0 2   232 0 2   296 0 
1   206 0 1   304 0 2   233 0 2   296 0 
1   209 0 1   216 1 2   233 0 2   323 0 
1   213 0 1   244 1 2   233 0 2   204 1 
1   216 0 2   142 0 2   233 0 2   344 1 
run;

title1 'Number of events by group and interval';
proc freq data=ratc;
table (censor day1 day2 day3)*Group/list;
run;

title1 'Person years by interval';
proc means data=ratc n mean std min max sum;;
class group;
vars Day DayI1 DayI2 DayI3;
ways 0 1;
run;
data I1(keep = Day1 DayI1 group) I2(keep = Day2 DayI2 group) I3(keep = Day3 DayI3 group);set ratc;run;

/* Create data to calculate rates */
data All;
 set I1(rename = (DayI1=Day0 Day1=Event) in=a1)  
I2(rename = (DayI2=Day0 Day2=Event) in=a2) I3(rename = (DayI3=Day0 Day3=Event) in=a3) ;
if a1 then Period=1;
if a2 then Period=2;
if a3 then Period=3;
Offset=log(Day0);
run; 

/* Calculate rates by interval*/
ods output estimates=IR;
proc genmod data=all;
class Group Period;
 model Event=Period|Group/d=p offset=Offset;
  estimate 'Period 1 for Group 1' intercept 1 Period 1 0 0 Group 1 0 Group*Period 1 0 0 0 0 0/exp;
  estimate 'Period 1 for Group 2' intercept 1 Period 1 0 0 Group 0 1 Group*Period 0 0 0 1 0 0/exp;
  estimate 'Period 2 for Group 1' intercept 1 Period 0 1 0 Group 1 0 Group*Period 0 1 0 0 0 0/exp;
  estimate 'Period 2 for Group 2' intercept 1 Period 0 1 0 Group 0 1 Group*Period 0 0 0 0 1 0/exp;
  estimate 'Period 3 for Group 1' intercept 1 Period 0 0 1 Group 1 0 Group*Period 0 0 1 0 0 0/exp;
  estimate 'Period 3 for Group 2' intercept 1 Period 0 0 1 Group 0 1 Group*Period 0 0 0 0 0 1/exp;
run;


title1 'Using Lor-Rank test'; 
options pageno=1 ps=57 ls=91 center nodate;
proc lifetest data=ratc notable; 
time day*censor(1); 
strata group; 
run;



/*** Angina Pectoris in Framingham study ***/
data anginaf;
   input censor time sex freq @@;
   years=1.5+3*time;
   cards;
0  0  1    33  0  0  2    35 
0  1  1    64  0  1  2    50 
0  2  1    59  0  2  2    63 
0  3  1    76  0  3  2    85 
0  4  1    83  0  4  2    85 
0  5  1    99  0  5  2    91 
0  6  1   110  0  6  2    91 
0  7  1   105  0  7  2    87 
0  8  1   932  0  8  2  1561 
1  0  1   139  1  0  2    82 
1  1  1    34  1  1  2    31 
1  2  1    36  1  2  2    35 
1  3  1    30  1  3  2    42 
1  4  1    37  1  4  2    39 
1  5  1    39  1  5  2    37 
1  6  1    31  1  6  2    34 
1  7  1    37  1  7  2    42 
1  8  1     0  1  8  2     0 
;
run;

goptions reset=all ftext='arial' htext=1.2 hsize=9.5in vsize=7in aspect=1 horigin=1.5in vorigin=0.3in ;
  symbol1 c=blue v=dot cv=red;
  symbol2 c=green v=circle cv=magenta;
options pageno=1 ps=57 ls=91 center nodate;
 title ' Angina Pectoris in Framingham study ';
   proc lifetest data=anginaf  method=lt intervals=(1.5 to 25.5 by 3) plots=(s); 
      time Years*Censor(0);
      freq Freq;
	  strata sex;
   run;

/*Relapse times after treatment for acute leukemia from Gehan (Biometrika 1965)*/
***relapse times in weeks (tweeks); 
***treatment group (1=placebo, 2=6-mercaptopurine (6-mp)); 
***censoring variable (1=censored obsn, 0=uncensored); 
***negative treatment times indicate censored observations; 

   data leuktrt; 
     input tweeks @@; 
     if _n_>21 then group=2; 
     else group=1; 
     censored=(tweeks < 0); 
     tweeks=abs(tweeks); 
     relapse=1-censored; 
     logwk=log(tweeks); 
     cards; 
1 1 2 2 3 4 4 5 5 8 8 8 8 11 11 12 12 15 17 22 23 
6 6 6 -6 7 -9 10 -10 -11 13 16 -17 -19 -20 22 23 -25 -32 -32 -34 -35 
; 
run;

options pageno=1 ps=57 ls=91 center nodate;
goptions reset=all ftext='arial' htext=1.2 hsize=9.5in vsize=7in aspect=1 horigin=1.5in vorigin=0.3in ;
  symbol1 c=blue v=dot cv=red;
title1 'Estimate and Compare time to relapse curves using Log-Rank test';
  symbol2 c=green v=circle cv=magenta;
proc lifetest data = leuktrt plots=(s,ls,lls); 
     time tweeks*censored(1); 
     strata group; 
run;

options pageno=1 nodate;
title1 ' Compare time to relapse by assuming an Exponential distribution - PROC LIFEREG';

proc lifereg data=leuktrt;
  model tweeks*censored(1) = group / distribution=exponential;
run;


proc genmod data=leuktrt; 
     class group; 
     model relapse=group/link=log dist=poisson offset=logwk; 
run; 

/*** recidivism in 432 male prisoners in the year following their release ***/
proc import datafile="Z:\Documents\BS853\Class 10\prison.xls" out=prison replace;
run;

options pageno=1 ps=57 ls=91 center nodate;
Title1 ' Predict the hazard of reicarceration with proportional hazard regression ';
proc phreg data=prison;
assess var=(mar paro race fin age prio educ) PH/resample;
model week*arrest(0)=mar paro race fin age prio educ/rl;
run;
