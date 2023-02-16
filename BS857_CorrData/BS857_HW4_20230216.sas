libname HW4 'Z:\';

*1 Construct the smoothed time plot of mean lig CD4 over time;
data CD4;
	set HW4.CD4;
	knot=max(week-17,0);
	if trt=. then treatment=.;
	else treatment = trt;
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
LR = 12186.9 - 11967.9;
pvalue=1-probchi(LR,1);
run;
proc print data=LR;
run;

*4 Interaction Test;
proc mixed data=cd4 covtest;
class treatment;
model log_cd4= week knot treatment*week treatment*knot/s chisq;
random intercept week knot/type=un subject=id G V;
contrast'Interaction test' week*treatment -1 0 0 1,
							 week*treatment 0 -1 0 1,
							 week*treatment 0 0 -1 1,
							 knot*treatment -1 0 0 1,
							 knot*treatment 0 -1 0 1,
							 knot*treatment 0 0 -1 1;
run;

*5 Expected weekly change in logCD4 count from week 1 to week 17;
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq;
random intercept week knot/type=un subject=id G V;
estimate 'Treatment 1' int 1 week 1 knot 0 treatment*week 1 0 0 0;
estimate 'Treatment 2' int 1 week 1 knot 0 treatment*week 0 1 0 0;
estimate 'Treatment 3' int 1 week 1 knot 0 treatment*week 0 0 1 0;
estimate 'Treatment 4' int 1 week 1 knot 0 treatment*week 0 0 0 1;
run;

*6 Expected weekly change in logCD4 count from week 17 to week 40;
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq;
random intercept week knot/type=un subject=id G V;
estimate 'Treatment 1' int 1 week 1 knot 1 treatment*week 1 0 0 0 treatment*knot 1 0 0 0;
estimate 'Treatment 2' int 1 week 1 knot 1 treatment*week 0 1 0 0 treatment*knot 0 1 0 0;
estimate 'Treatment 3' int 1 week 1 knot 1 treatment*week 0 0 1 0 treatment*knot 0 0 1 0;
estimate 'Treatment 4' int 1 week 1 knot 1 treatment*week 0 0 0 1 treatment*knot 0 0 0 1;
run;

/* 7.
3 df hypothesis test for change between treatment groups on or after week 17
*/
title 'Change Between Treatment Groups After or On Week 17';
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq;
random intercept week knot/type=un subject=id G V;
contrast'Interaction test'   knot*treatment -1 0 0 1,
							 knot*treatment 0 -1 0 1,
							 knot*treatment 0 0 -1 1;
run;


/* 8
Do any subjects have a significant difference in change from baseline to week 17
that is significantly different from the average change from baseline to week 17
in their treatment group?
*/
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq ;
random intercept week knot/solution type=un subject=id G V;
ods output solutionr=sr(keep=effect subject estimate probt);
run;
proc print data=sr;
where effect='Week' and probt<0.05;
run;

*9 Prove the variance only depends on treatment in treatment only random effect;
proc mixed data=cd4 covtest;
class treatment;
model log_cd4=week knot treatment*week treatment*knot/s chisq;
random treatment/type=un subject=id G V;
run;

