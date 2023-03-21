  /*soh************************************************************************                                                        
   Boston University - Biostatistics Department                                                                                         
   PROGRAM NAME          : script - Lecture7.sas                                                                                
                           (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 7)                                        
   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
                           - Lecture: Multinomial logit Models for ordinal data                                                                        
                                                                                                                                        
   DESCRIPTION           : 1. Read data                                                                                                 
                           2. Multinomial logit Models                                                    
                                                                                                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3                                                                                              
   INFRASTRUCTURE        : Windows                                                                                                      
   INPUT                 :                                                                                                              
   OUTPUT               :                                                                                                              
  -----------------------------------------------------------------------------                                                         
   Author : Gheorghe Doros                                                                                                              
   Last modified 03/08/2022                                                                                                             
 **eoh************************************************************************/                                                         
options ps=60 ls=89 pageno=1 nodate;                                                  


/*satisfaction with THR and hospital volume and year of surgery.*/
proc format ;
  value vol 1='High' 0='Low';  
run;
data thr;  
input  year highvol satisf count; 
format highvol vol.;
datalines;  
1         0         1        84
1         0         2       231
1         0         3        99
1         1         1       473
1         1         2       493
1         1         3        93
2         0         1       150
2         0         2       347
2         0         3       117
2         1         1       332
2         1         2       387
2         1         3        62
3         0         1       257
3         0         2       889
3         0         3       234
3         1         1       571
3         1         2       793
3         1         3       112
; 
run;

options pageno=1 nodate ps=57 ls=91;
title1 'Adjacent Logits models for THR';
proc catmod data=thr;  
  weight count;  
  response ALOGIT;  
  model satisf= _response_ highvol year; 
run; 
quit;


/* let the effect of year vary with the cutpoint*/
title1 'Adjacent Logits models for THR - with coefficents dependent on the level of satisfcation';
proc catmod data=thr ;  
  weight count;  
  response ALOGIT;  
  model satisf= _response_ highvol _response_|year; 
run; 
quit;


/* number of hip fractures by race age and BMI */
/* n=0 then no hip fractures, n=1 then 1 hip fractures, n=2 then more than 1 hip fractures */
data nhf;
do n=0 to 2;
input race $ age $ bmi count @@;
output;
end;
cards;
white	<=80 	1	39	white	<=80 	1	29	white	<=80 	1	8
white	<=80	2	4	white	<=80	2	8	white	<=80	2	1
white	<=80	3	11	white	<=80	3	9	white	<=80	3	6
white	<=80	4	48	white	<=80	4	17	white	<=80	4	8
white	>80 	1	231	white	>80 	1	115	white	>80 	1	51
white	>80	    2	17	white	>80	    2	21	white	>80	    2	13
white	>80	    3	18	white	>80	    3	28	white	>80	    3	45
white	>80	    4	197	white	>80	    4	111	white	>80	    4	35
black	<=80 	1	19	black	<=80 	1	40	black	<=80 	1	19
black	<=80	2	5	black	<=80	2	17	black	<=80	2	7
black	<=80	3	2	black	<=80	3	14	black	<=80	3	3
black	<=80	4	49	black	<=80	4	79	black	<=80	4	24
black	>80 	1	110	black	>80 	1	133	black	>80 	1	103
black	>80	    2	18	black	>80	    2	38	black	>80	    2	25
black	>80	    3	11	black	>80	    3	25	black	>80	    3	18
black	>80	    4	178	black	>80	    4	206	black	>80	    4	81
;
run;
options pageno=1 nodate ps=57 ls=91;
/* continuation ratio logits models*/
 title1 'Continuation-ratio Logits models for NHF using the response statement';
proc catmod data=nhf;
     weight count;
     response * 1 -1 0  0,
                0  0 1 -1  log  * 1 0 0,
                                  0 1 1,
                                  0 1 0,
                                  0 0 1;  /* p1
                                             p2
                                             p3  */
     model n=_response_ age bmi|race/  predict;
     run;
     quit;

	 /* Use second way to defining continuation ration logits */
title1 'Continuation-ratio Logits models for NHF using the response statement(2)';
  proc catmod data=nhf;
     weight count;
     response * -1 1  0 0,
                 0 0 -1 1  log  * 1 0 0,
                                  0 1 0,
                                  1 1 0,
                                  0 0 1;  
     model n=_response_ age bmi|race/ prob predict;
     run;
     quit;

/* let the coefficients depend on the number of hip fracturs */
 title2 'Coefficients dependent on the number of HF';
proc catmod data=nhf;
     weight count;
     response * 1 -1 0 0,
                0 0 1 -1  log  * 1 0 0,
                                 0 1 1,
                                 0 1 0,
                                 0 0 1;  

     model n=_response_|bmi _response_|age _response_|race  bmi|race/ freq;
     run;
     quit;

title1 'Adjacent Logits models for THR using the response statement';
proc catmod data=thr;  
  weight count;  
 response * -1  1 0,
             0 -1 1  log  * 1 0 0,
                            0 1 0,
                            0 0 1;  /* p1
                                       p2
                                       p3  */  
  model satisf= _response_ highvol year; 
run; 
quit;

title1 'cumulative Logits models for THR using the response statement';
proc catmod data=thr;  
  weight count;  
 response *  1 -1 0  0,
             0  0 1 -1  log  * 1 0 0,
                               0 1 1,
                               1 1 0,
                               0 0 1;  /* p1
                                       p2
                                       p3  */  
  model satisf= _response_ highvol year; 
run; 
quit;
title1 'cumulative Logits models for THR using Proc Logistic ';
proc logistic data=THR;
  class highvol year;
  model satisf= highvol year;
  freq count;
run;

/********************************************************************************/
/***               Overdispersion in binomial models                          ***/
/********************************************************************************/
DATA aggregate;
  INPUT pot total yes cultivar soil @@; 
cards;
 1 16  8 0 0   2 51 26 0 0 
 5 36  9 0 0   6 81 23 1 0 
 9 28  8 1 0  10 62 23 1 0 
13 41 22 0 1  14 12  3 0 1 
17 30 15 1 1  18 51 32 1 1 
 3 45 23 0 0   4 39 10 0 0 
 7 30 10 1 0   8 39 17 1 0 
11 51 32 0 1  12 72 55 0 1 
15 13 10 0 1  16 79 46 1 1 
19 74 53 1 1  20 56 12 1 1 
;
run;


/* no scale - aggregated data'*/
options pageno=1 nodate ps=57 ls=91;
title1 ' No Scale ';
proc genmod data=aggregate;
model yes/total = cultivar|soil/type3 dist=binomial ;
run;

/*  scale - aggregated data'*/
/* Model 2 */
title1 ' Scale parameter estimated by the scaled dispersion ';
proc genmod data=aggregate;
model yes/total = cultivar|soil/type3 dist=binomial dscale ;
run;

