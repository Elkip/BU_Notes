libname HW5 'Z:\';

data birth;
    set HW5.birthwt;
    by mid;
    time_men=age - lag(age);
    if first.mid then time_men=0;
run;

*1 Fit the mixed effects model with linear trend for 
maternal age and randomly varying intercepts;
proc mixed data=birth covtest plots=all;
model weight=age/s chisq;
random intercept/type=un subject=mid G V;
run;

*2 Fit the fixed effect model with beta-age (FE);
proc glm data=birth;
class mid;
model weight=age/solution;
run;quit;

*4 Construct a histogram of standardized residuals along with a QQ plot;
proc mixed data=birth method=ml covtest plots=all;
model weight = age/s chisq outpred=yhat outpm=pred residual;
random intercept /type=un subject=mid G V ;
run;

*5 Are any mothers outliers?;
data pred;
set pred;
sqStudentResid=StudentResid**2;
visits=1;
run;

proc means data=pred noprint;
var sqStudentResid visits;
output out=mahalanobis (drop=_type_ _freq_)
sum(sqStudentResid visits)=distance novisits;
by mid;
run;

data mahalanobis;
set mahalanobis;
pvalue=1-probchi(distance,novisits);
run;

proc print data=mahalanobis;
where pvalue<0.05/878;
run;

*6 Plot the semi-variogram for the standardized residuals and lowess smoothed curve;
proc variogram data=pred outv=outv noprint;
compute lagd=1 maxlag=10;
coord xc=time_men yc=visits;
by mid;
var StudentResid;
run;

proc loess data=outv plots=all;
   model variog = distance;
run;
