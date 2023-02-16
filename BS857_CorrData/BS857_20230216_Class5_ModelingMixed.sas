libname S857 'C:\Users\ytrip\Dropbox\Courses\BS857\2020\Datasets';
/************************************************
* Six cities study of air pollution and health	*
*************************************************/
data fev1;
set s857.fev1;
lgfevht=Log_FEV1_-2*logHgt;
run;

*Durbin-Wu-Hausman test;
proc means data=fev1 ;
*where id ne 197;
by id;
var age ;
output out=mean_age;
run;
data mean_age;
set mean_age;
if _STAT_='MEAN' then;else delete;
rename age=age_mean;
keep id age;
run;
data fev1;
merge fev1 mean_age;
by id;
run;
data fev1;
set fev1;
age_dev=age-age_mean;
run;
proc means data=fev1 mean var;
var age age_dev;
run;
proc means data=mean_age;
var age_mean;
run;

proc mixed data=fev1 method=ml covtest;
*where id ne 197;
class id;
model lgfevht=age age_mean/s chisq;
random intercept/type=un subject=id G V ;
run;
*Fixed Effects model;
proc glm data=fev1;
*where id ne 197;
class id;
model lgfevht=id age/solution;
run;
quit;
proc mixed data=fev1 method=ml covtest;
*where id ne 197;
class id;
model lgfevht=age /s chisq;
random intercept/type=un subject=id G V;
run;
proc mixed data=fev1 method=ml covtest;
*where id ne 197;
class id;
model lgfevht=/s chisq;
random intercept/type=un subject=id G V ;
run;
proc mixed data=fev1 method=ml covtest;
*where id ne 197;
class id;
model age=/s chisq;
random intercept/type=un subject=id G V ;
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
 ods graphics;
 proc mixed data=fat covtest plots=all;
 model Perc_BF = time_men knot/s chisq outpred=yhat outpm = pred1f residual;
 random intercept time_men knot/type=un subject=id G V ;
run;

/*****************************************
*     Mahalanobis Distance				*
*******************************************/
data pred1f;
set pred1f;
sqStudentResid=StudentResid**2;
visits=1;
run;
proc means data=pred1f noprint;
var sqStudentResid visits;
output out=mahalanobis (drop=_type_ _freq_)
sum(sqStudentResid visits)=distance novisits;
by id;
run;
data mahalanobis;
set mahalanobis;
pvalue=1-probchi(distance,novisits);
run;
proc print data=mahalanobis;
where pvalue<0.05/162;
run;
/*****************************************/

proc means data=fat;
var time_men;
run;
proc variogram data=pred1f outv=outv noprint;
   compute lagd=1 maxlag=10;
   coord xc=time_men yc=visits;
   by id;
   var StudentResid;
run;
   ods graphics on; 

   proc loess data=outv plots=all;
      model variog = distance;
   run;
 ods graphics off;


 /*ALternative model*/
 data fat;
 set fat;
 t=time_men;
 run;
 proc mixed data=fat covtest plots=all;
 class t;
 model Perc_BF = time_men knot/s chisq outpred=yhat2 outpm = pred2f residual;
 random intercept time_men knot/type=un subject=id G V;
repeated t/type=sp(exp)(t) subject=id;
run;
data pred2f;
set pred2f;
sqStudentResid=StudentResid**2;
visits=1;
run;
proc variogram data=pred2f outv=outv2 noprint;
   compute lagd=1 maxlag=10;
   coord xc=time_men yc=visits;
   by id;
   var StudentResid;
run;
   ods graphics on; 

   proc loess data=outv2 plots=all;
      model variog = distance;
   run;
 ods graphics off;
