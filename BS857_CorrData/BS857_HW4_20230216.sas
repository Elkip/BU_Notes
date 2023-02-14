libname HW4 'Z:\';

*1 Construct the smoothed time plot of mean lig CD4 over time;
data CD4;
	set HW4.CD4;
	knot=max(week-17,0);
	if trt=. then treatment=.;
	else if trt<4 then treatment=1;
	else treatment=2;
run;

proc transreg ss2 data=cd4;
      model identity(log_cd4) = class(treatment / zero=none) *
                          smooth(week / sm=80);
      output p;
run;

/* 2 
Fit a model where each patient's response is represented by a randomly
varying pieceise linear spline with a knot at week 17
*/
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq;
random intercept  week knot/type=un subject=id G V;
run;

/* 3
Is the model with only only randomly varying intercepts defensible?
*/
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq;
random intercept/type=un subject=id G V;
run;

data LR;
LR = 12166.2 - 11941.5;
pvalue=1-probchi(LR,5);
run;
proc print data=LR;
run;

/* 7.
3 df hypothesis test for change between treatment groups on or after week 17
*/
