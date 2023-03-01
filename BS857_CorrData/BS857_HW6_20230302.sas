libname HW6 'Z:\';

*Exercise 1;
data tumor;
	set HW5.tumor;
run;

*a. Using a bernoulli distribution fit a logistic regression;
title 'Logistic Model of Tumour - Bernoulli';
proc logistic data=tumor plots=effect;
class trt;
model trt=tumor;
run;

*Exercise 2;
*Convert Data to Long;
proc transpose data=HW5.respir out=respir;
	by clinic id trt;
run;

data respir;
	set respir;
	time=_name_;
	y = col1;
	keep clinic id trt y time;
run;

*a. Model log odds of respiratory infections between treatment groups;
proc genmod data=respir descending;
	class id time(ref='Y0') trt(ref='P');
	model y=trt/dist=bin link=logit type3 wald;
	repeated subject=id / withinsubject=time logor=fullclust;
run;quit;
