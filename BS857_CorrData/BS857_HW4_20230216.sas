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
title 'Change Between Treatment Groups After or On Week 17';
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq;
random intercept week knot/type=un subject=id G V;
contrast 'Interaction test' knot 1, 
	treatment*week 1 -1, 
	treatment*knot 1 -1;
run;


data cd4;
set hw4.cd4;
knot=max(week-17,0); 
if trt=. then treatment=.;
else if trt<4 then treatment=1;
else treatment=2;
run;

proc mixed data=cd4 covtest;
class treatment;
model log_cd4= week knot treatment*week treatment*knot/s chisq;
random intercept  week knot/type=un subject=id G V;
ods output solutionr=sr(keep=effect subject estimate probt);
contrast'Interaction test' knot 1,
						   treatment*week 1 -1, 
						   treatment*knot 1 -1;
run;

/* 8
Use the model from part 2 of the sample code
Do any subjects have a significant difference in change from baseline to week 17
that is significantly different from the average change from baseline to week 17
in their treatment group?
*/
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot Age Gender/s chisq outpred=random;
random intercept week knot/type=un subject=id G V solution;
ods output solutionr=sr(keep=effect subject estimate probt);
run;
proc print data=sr;
where effect='Week' and Estimate<0 and probt<0.05;
run;

*9;
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq;
random intercept week/type= subject=id G V;
run;

data LR;
LR = 12166.2 - 12011.9;
pvalue=1-probchi(LR,3);
run;
proc print data=LR;
run;
