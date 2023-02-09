libname S857 'C:\Users\ytrip\Dropbox\Courses\BS857\2020\Datasets';
/************************************************
* Six cities study of air pollution and health	*
*************************************************/
data fev1;
set s857.fev1;
lgfevht=Log_FEV1_-logHgt;
t=age;
run;
proc sgplot data=fev1  noautolegend ;
	* spaghetti plot;
	yaxis min = -0.3 max = 1;
	reg x=age y=lgfevht 
    / group = id nomarkers LINEATTRS = (COLOR= gray PATTERN = 1 THICKNESS = 1) ;
	* overall spline;
	reg x=age y=lgfevht 
    / nomarkers LINEATTRS = (COLOR= red PATTERN = 1 THICKNESS = 3) ;
run;
quit;
proc mixed data=fev1 covtest;
class t;
model Log_FEV1_=age logHgt Init_Age logInitHgt/s chisq;
repeated  t/type=cs subject=id R Rcorr;
run;
proc mixed data=fev1 covtest method=REML;
model Log_FEV1_=age logHgt Init_Age logInitHgt/s chisq ;
random intercept /type=un subject=id G V;
run;
proc mixed data=fev1 covtest method=REML;
model Log_FEV1_=age logHgt Init_Age logInitHgt/s chisq;
random intercept age/type=un subject=id G V;
run;

proc mixed data=fev1 covtest;
model Log_FEV1_=age logHgt Init_Age logInitHgt/s chisq;
random intercept logHgt/type=un subject=id G V;
run;


proc mixed data=fev1 covtest method=REML;
model Log_FEV1_=age logHgt Init_Age logInitHgt/s chisq;
random  intercept age logHgt/type=un subject=id G V;
run;

/************************************************
* Study of influence of menarche on changes in	*
* body fat										*
*************************************************/

proc sgplot data=s857.fat  noautolegend ;
	* spaghetti plot;
	yaxis min = 0 max = 50;
	reg x=time_men y=Perc_BF 
    / group = id nomarkers LINEATTRS = (COLOR= gray PATTERN = 1 THICKNESS = 1) ;
	* overall spline;
	reg x=time_men y=Perc_BF
    / nomarkers LINEATTRS = (COLOR= red PATTERN = 1 THICKNESS = 3) ;
run;
quit;

   ods graphics on; 

   proc loess data=s857.fat plots=all;
      model Perc_BF = time_men;
   run;
 ods graphics off;

 data fat;
 set s857.fat;
 knot=max(time_men,0);
 run;
ods output solutionr=bluptable;
*ods trace on;
*ods listing;
 proc mixed data=fat covtest;
 model Perc_BF = time_men knot/s chisq outpred=yhat outpm = pred1f;
 random intercept time_men knot/type=un subject=id G V solution;
run;
*ods trace off;
ods close;

data yhat;
set yhat;
rename Pred=Pred_r;
run;
data Prediction;
merge yhat pred1f;
keep id pred pred_r time_men perc_BF;
run;

data prediction_long;
length Scale $20;
set prediction;
y=pred;Scale="Average prediction";output;
y=pred_r;Scale="Subject prediction";output;
y=perc_bf;Scale="Raw Data";output;
run;
goptions reset=all;
symbol1 c=blue  v=star   h=.8 i=j      w=5;
symbol2 c=red   v=dot    h=.8 i=j      w=1;
symbol3 c=green   v=squarefilled    h=.8 i=j      w=1;
axis1 order=(0 to 50 by 10) label=( 'Predicted % Body Fat');

proc gplot data=Prediction_long;
where id=10;
plot y*time_men=Scale/ vaxis=axis1 ;
run;
quit;

/************************************************
* Repeated CD4 counts data from AIDS 			*
*clinical trial.								*
*************************************************/
data CD4;
set s857.CD4;
knot=max(week-16,0);
if trt=. then treatment=.;
else if trt<4 then treatment=1;
else treatment=2;
run;
 ods graphics on; 

proc transreg ss2 data=cd4;
      model identity(log_cd4) = class(treatment / zero=none) *
                          smooth(week / sm=80);
      output p;
   run;
  
 ods graphics off;
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq;
random intercept  week knot/type=un subject=id G V;
run;

proc mixed data=cd4 covtest;
class treatment;
model log_cd4= week knot treatment*week treatment*knot/s chisq;
random intercept  week knot/type=un subject=id G V;
contrast'Interaction test' treatment*week 1 -1, 
							treatment*knot 1 -1;
contrast 'Slope test' treatment*week 1 -1  treatment*knot 1 -1;
run;



proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot Age Gender/s chisq outpred=random;
random intercept  week knot/type=un subject=id G V solution;
ods output solutionr=sr(keep=effect subject estimate probt);
run;
proc print data=sr;
where effect='Week' and Estimate<0 and probt<0.05;
run;
