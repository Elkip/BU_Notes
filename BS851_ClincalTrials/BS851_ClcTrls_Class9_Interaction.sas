
* BS851 Topic #9: Interaction;


************ CONTINUOUS OUTCOME ;

PROC IMPORT OUT= dbp 
DATAFILE= "C:\bp.csv" 
DBMS=CSV REPLACE;
GETNAMES=YES;
DATAROW=2; 
RUN;

proc format;
value $ trtf "A"="New Treatment" "B"="Placebo";
run;

data dbp;
set dbp;
format trt trtf.;
run;

proc print data=dbp (obs=10);
run;

title "Overall treatment group means";
proc means data=dbp;
class trt ;
var change;
run;

title "Treatment group means stratified by sex";
proc means data=dbp;
class trt sex;
var change;
run;

title "Unadjusted analysis";
proc glm data=dbp;
class trt;
model change=trt / solution clparm;
run;
quit;

title "Adjusted analysis";
proc glm data=dbp;
class trt sex;
model change=trt sex / solution clparm;
run;
quit;title;

proc means data=dbp;
class sex trt;
var change;
run;

title "Analysis with an interaction term";
proc glm data=dbp;
class trt sex;
model change=trt|sex /solution clparm ss3;
run;
quit;title;


* dummy variable coding;

data dbp;
set dbp;
if sex='F' then new_sex=1;
else new_sex=0;
if trt='A' then new_trt=1;
else new_trt=0;
trt_sex=new_trt*new_sex;
run;

proc reg data=dbp;
model change=new_trt new_sex trt_sex;
INTERACTION: test trt_sex=0;
run;
quit;


* INTERACTION WITH A CONTINUOUS COVARIATE ;

PROC IMPORT OUT= WORK.one 
            DATAFILE= "C:\InteractionContinuous.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print data=one;
run;

* Analysis with an interaction term;
proc glm data=one;
class trt;
model value=trt|age;
run; 
quit;

* Create dummy variables;
data two;
set one;
	*dummy variable for trt;
	if trt="N" then do; x1=1; x2=0; end;
	else if trt="A" then do; x1=0; x2=1; end;
	else if trt="P" then do; x1=0; x2=0; end;
	* Interaction terms;
	x1Age=x1*Age; 
 	x2Age=x2*Age;
run;

proc reg data=two; 
	model value=x1 x2 age x1Age x2Age;
	INTERACTION: test x1Age=x2Age=0; * Joint test for both interaction terms;
run;
quit;



************ BINARY OUTCOME ;

proc format;
  value corfmt 	0='NORMAL'
        		1='ABNORMAL';
run;
data kawasaki;
  	  input arm $ center corabn count;
	  format corabn corfmt.;
cards;
2-ASA 1 0 43
2-ASA 1 1 14
1-GG  1 0 48
1-GG  1 1  4
2-ASA 2 0 45
2-ASA 2 1 35
1-GG  2 0 46
1-GG  2 1  9
run;


*  UNADJUSTED ANALYSES:

* Using PROC FREQ;
proc freq data=kawasaki order=formatted;
tables arm*corabn / chisq cmh nocol nopercent;
weight count;
run;

* Using PROC LOGISTIC;
proc logistic data=kawasaki;
	class arm(param=ref ref='2-ASA') ;
	model corabn(event='ABNORMAL')=arm /risklimits ;
	freq count;
run;


*  ADJUSTED ANALYSES;
proc logistic data=kawasaki;
	class arm(param=ref ref='2-ASA') center;
	model corabn(event='ABNORMAL')=arm center / risklimits ;
	freq count;
run;

* Is effect the same in the 2 centers?;
proc freq data=kawasaki order=formatted;
tables center*arm*corabn / nocol nopercent;
weight count;
run;

* testing for interaction using cmh method, stratified 2x2 tables;
proc freq data=kawasaki order=formatted;
tables center*arm*corabn / cmh nocol nopercent;
weight count;
run;

* with "bdt" option;
proc freq data=kawasaki order=formatted;
tables center*arm*corabn / cmh bdt nocol nopercent;
weight count;
run;

* Exact test for small cell counts;
proc freq data=kawasaki order=formatted;
table center*arm*corabn / cmh bdt nocol nopercent;
weight count;
exact eqor comor;
run;

* testing for interaction using proc logistic regression ;
proc logistic data=kawasaki;
	class arm(param=ref ref='2-ASA') center;
	model corabn(event='ABNORMAL')=arm|center / risklimits;
	freq count;
	oddsratio arm / at (center=all);
run;


* KAWASAKI example with 3 treatments;
proc format;
   value corfmt 0='NORMAL'
                1='ABNORMAL';
run;

data kawasaki;
input arm $ center corabn count;
format corabn corfmt.;
cards;
ASA 1 0 43
ASA 1 1 14
GG  1 0 48
GG  1 1  4
PLAC  1 0 30
PLAC  1 1 20
ASA 2 0 45
ASA 2 1 35
GG  2 0 46
GG  2 1  9
PLAC  2 0 35
PLAC  2 1 45 
run;


proc logistic data=kawasaki;
	  class arm(param=ref ref="ASA") center;
	  model corabn(event="ABNORMAL")=arm|center / risklimits;
	  freq count;
	  oddsratio arm / at (center=all);
run;


************ SURVIVAL EXAMPLE ;

data Survdata;
input trt $ agegrp $ months remiss;
cards;
N Young 38.00 .00
N Young 40.00 .00
N Young 42.00 .00
N Young 31.00 .00
N Young 11.00 1.00
N Young 20.00 1.00
N Young 21.00 1.00
N Young 10.00 1.00
N Young 28.00 1.00
N Young 23.00 1.00
N Young 26.00 1.00
N Young 23.00 1.00
N Young 25.00 1.00
N Young 27.00 1.00
N Young 18.00 1.00
N Old 14.00 .00
N Old 10.00 .00
N Old 12.00 .00
N Old 28.00 .00
N Old 26.00 .00
N Old 35.00 .00
N Old 25.00 .00
N Old 25.00 .00
N Old 22.00 1.00
N Old 22.00 1.00
N Old 36.00 1.00
N Old 23.00 1.00
N Old 38.00 1.00
N Old 37.00 1.00
N Old 37.00 1.00
N Old 37.00 1.00
N Old 33.00 1.00
N Old 20.00 1.00
B Young 55.00 .00
B Young 50.00 .00
B Young 40.00 .00
B Young 48.00 .00
B Young 55.00 .00
B Young 15.00 .00
B Young 25.00 .00
B Young 50.00 .00
B Young 38.00 1.00
B Young 33.00 1.00
B Young 50.00 1.00
B Young 45.00 1.00
B Young 43.00 1.00
B Young 30.00 1.00
B Young 25.00 1.00
B Young 25.00 1.00
B Young 32.00 1.00
B Young 30.00 1.00
B Old 50.00 .00
B Old 48.00 .00
B Old 33.00 .00
B Old 23.00 .00
B Old 55.00 .00
B Old 33.00 .00
B Old 40.00 .00
B Old 41.00 .00
B Old 42.00 .00
B Old 55.00 .00
B Old 42.00 .00
B Old 40.00 1.00
B Old 41.00 1.00
B Old 39.00 1.00
B Old 45.00 1.00
B Old 40.00 1.00
B Old 51.00 1.00
;
run;

proc phreg data=Survdata;
	class trt (ref='N') agegrp (ref='Young'); 
	model months*remiss(0) = trt|agegrp ;
	hazardratio trt / at (agegrp=all); 
	* 'hazardratio' statement is similar to the 'oddsratio' statement in PROC LOGISTIC;
run;




*For your reference: code for forest plot;
data forest;                                                                                                                            
   input study $1-16 grp OddsRatio LowerCL UpperCL Weight;                                                                              
   format weight percent5. Q1 Q3 4.2 oddsratio lowercl uppercl 5.3;                                                                     
   ObsId=_N_;                                                                                                                           
   OR='OR'; LCL='LCL'; UCL='UCL'; WT='Weight';                                                                                          
   if grp=1 then do;                                                                                                                    
      weight=weight*.05;                                                                                                                
      Q1=OddsRatio-OddsRatio*weight;                                                                                                    
      Q3=OddsRatio+OddsRatio*weight;                                                                                                    
        lcl2=lowercl;                                                                                                                   
      ucl2=uppercl;                                                                                                                     
   end;                                                                                                                                 
   else study2=study;                                                                                                                   
datalines;                                                                                                                              
Center1           1  0.26 0.08 0.84  1                                                                                               
Center2           1  0.25 0.11 0.58  1                                                                                             
Overall           2  0.25 0.13 0.50  .                                                                                               
;                                                                                                                                       
run;

proc sort data=forest out=forest2;                                                                                                      
   by descending obsid;                                                                                                                 
run;                                                                                                                                    
                                                                                                                                        
/* Add sequence numbers to each observation */                                                                                       
data forest3;                                                                                                                           
   set forest2 end=last;                                                                                                                
   retain fmtname 'Study' type 'n';                                                                                                     
   studyvalue=_n_;                                                                                                                      
   if study2='Overall' then study2value=1;                                                                                              
   else study2value = .;                                                                                                                
/* Output values and formatted strings to data set */                                                                                   
   label=study;                                                                                                                         
   start=studyvalue;                                                                                                                    
   end=studyvalue;                                                                                                                      
   output;                                                                                                                              
   if last then do;                                                                                                                     
      hlo='O';                                                                                                                          
      label='Other';                                                                                                                    
   end;                                                                                                                                 
run;                                                                                                                                    

/* Create the format from the data set */                                                                                                                                                                                                                                      
proc format library=work cntlin=forest3;                                                                                                
run;                                                                                                                                    
/* Apply the format to the study values and remove Overall from Study column. */                                                        
/* Compute the width of the box proportional to weight in log scale. */                                                                 
data forest4;                                                                                                                           
   format studyvalue study2value study.;                                                                                                
   drop fmtname type label start end hlo pct;                                                                                           
   set forest3 (where=(studyvalue > 0)) nobs=nobs;                                                                                      
   if studyvalue=1 then studyvalue=.;                                                                                                   
   /* Compute marker width */                                                                                                           
   x1=oddsratio / (10 ** (weight/2));                                                                                                   
   x2=oddsratio * (10 ** (weight/2));
/* Compute top and bottom offsets */                                                                                                    
   if _n_ = nobs then do;                                                                                                                  
      pct=0.75/nobs;                                                                                                                        
      call symputx("pct", pct);                                                                                                             
      call symputx("pct2", 2*pct);                                                                                                          
      call symputx("count", nobs);                                                                                                          
   end;                                                                                                                                    
run;                                                                                                                                    

ods listing close;                                                                                                                      
ods html image_dpi=100 path="." file='sgplotforest.html';                                                                               
ods graphics / reset width=600px height=400px imagename="Forest_Plot_Vector" imagefmt=gif;                                              
                                                                                                                                        
title "Impact of Treatment on CA Abnormality by Center";                                                                                      
title2 h=8pt 'Odds Ratio and 95% CL';                                                                                                   

proc sgplot data=forest4 noautolegend;                                                                                                  
   scatter y=study2value x=oddsratio / markerattrs=graphdata2(symbol=diamondfilled size=10);                                            
   scatter y=studyvalue x=oddsratio / xerrorupper=ucl2 xerrorlower=lcl2 markerattrs=graphdata1(symbol=squarefilled size=0);             
   vector x=x2 y=studyvalue / xorigin=x1 yorigin=studyvalue lineattrs=graphdata1(thickness=8) noarrowheads;                             
   scatter y=studyvalue x=or / markerchar=oddsratio x2axis;                                                                             
   scatter y=studyvalue x=lcl / markerchar=lowercl x2axis;                                                                              
   scatter y=studyvalue x=ucl / markerchar=uppercl x2axis;                                                                              
   scatter y=studyvalue x=wt / markerchar=weight x2axis;                                                                                
   refline 1 100  / axis=x;                                                                                                             
   refline 0.1 10 / axis=x lineattrs=(pattern=shortdash) transparency=0.5;                                                              
   inset '        Favors ASA+GG'  / position=bottomleft;                                                                             
   inset 'Favors GG'  / position=bottom;                                                                                           
   xaxis type=log offsetmin=0 offsetmax=0.35 min=0.01 max=100 minor display=(nolabel) ;                                                 
   x2axis offsetmin=0.7 display=(noticks nolabel);                                                                                      
   yaxis display=(noticks nolabel) offsetmin=0.1 offsetmax=0.05 values=(1 to &count by 1);                                              
run;                                                                                                                                    
ods html close;                                                                                                                         
ods listing;


