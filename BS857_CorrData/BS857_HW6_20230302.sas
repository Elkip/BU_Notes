libname HW6 'Z:\';

*Exercise 1;
data tumor;
	set HW6.tumor;
run;

*a. Using a bernoulli distribution fit a logistic regression;
title 'Logistic Model of Tumour Outcome with Treatment';
proc logistic data=tumor descending plots=effect;
class trt(ref='0');
model tumor=trt;
run;

proc genmod data=tumor;
class trt(ref = '0');
model tumor(event='1') = trt/DIST=binomial LINK=logit;
run;

*Exercise 2;
*Convert Data to Long;
data respir;
	set HW6.respir;
	y=y0;time=0;output;
	y=y1;time=1;output;
	y=y2;time=2;output;
	y=y3;time=3;output;
	y=y4;time=4;output;
	drop y0 y1 y2 y3 y4;
run;

*a. Model log odds of respiratory infections between treatment groups;
proc genmod data=respir descending;
	class id time(ref='0') trt(ref='P');
	model y(event='1')=trt time trt*time/dist=bin link=logit type3 wald;
	repeated subject=id / withinsubject=time logor=fullclust;
run;quit;

proc genmod data=respir descending;
	class id time(ref='0') trt(ref='P');
	model y(event='1')=trt time trt*time/dist=bin link=logit type3 wald;
	repeated subject=id / withinsubject=time logor=fullclust
	 logor=zrep( (1 2) 1 0 0 0 0 0 0 0 0 0,
                 (1 3) 0 1 0 0 0 0 0 0 0 0,
                 (1 4) 0 0 1 0 0 0 0 0 0 0,
				 (1 5) 0 0 0 1 0 0 0 0 0 0,
                 (2 3) 1 0 0 0 1 0 0 0 0 0,
                 (2 4) 0 1 0 0 0 1 0 0 0 0,
				 (2 5) 0 0 1 0 0 0 1 0 0 0,
                 (3 4) 1 0 0 0 0 0 0 1 0 0,
                 (3 5) 0 1 0 0 0 0 0 0 1 0,
				 (4 5) 1 0 0 0 0 0 0 0 0 1);
run;


/*Fitting a model with the association between occasions 
  1-2, 3-4, 3-5, and 4-5 are the same
  1-3, 1-5, and 2-4 are the same
  1-4, 2-3, and 2-5 are the same*/
proc genmod data=respir descending;
     class id trt time;   
     model y(event='1')=trt time trt*time / dist=bin link=logit type3 wald;
     repeated subject=id / withinsubject=time 
         logor=zrep( (1 2) 1 0 0 0,
                     (1 3) 0 1 0 0,
                     (1 4) 0 0 1 0,
					 (1 5) 0 1 0 0,
					 (2 3) 0 0 1 0,
					 (2 4) 0 1 0 0,
					 (2 5) 0 0 1 0,
					 (3 4) 1 0 0 0,
					 (3 5) 1 0 0 0,
					 (4 5) 1 0 0 0);
run;

proc genmod data=respir ;
     class id time trt y;   
     model y=trt time time*trt / dist=bin link=logit

	 type3 wald;
	 contrast 'Age X Gender Interaction' trt*time 1 -1/wald;
     repeated subject=id / withinsubject=time logor=fullclust
	 logor=zrep( (1 2) 1 0 0 0 ,
                 (1 3) 0 1 0 0 ,
                 (1 4) 0 0 1 0 ,
				 (1 5) 0 0 0 1 ,
                 (2 3) 1 0 0 0 ,
                 (2 4) 0 1 0 0 ,
				 (2 5) 0 0 1 0 ,
                 (3 4) 1 0 0 0 ,
                 (3 5) 0 1 0 0 ,
				 (4 5) 1 0 0 0 );
				 run;


*Question 2.b;
proc sort data = respir;
by trt;
run;

proc freq data=respir;
by trt;
tables clinic*time*y;
run;

proc genmod data=respir;
class id trt time clinic;
model y(event='1')=trt time trt*time clinic*trt/DIST=binomial LINK=LOGit;
title1 'Simple Logistic Regression using PROC GENMOD';
run;
