libname HW7 '/home/elkip/Datasets';

data toenails;
	set HW7.toenails;
run;

*1. Marginal model;
proc genmod data=toenails;
class id trt(ref='0') visit/param=ref;
model y = month trt month*trt/dist=binomial link=logit type3 wald;
repeated subject=id/ type=cs withinsubject=visit logor=exch;
run;

*3. Generalized linear mixed model with random intercept;
proc glimmix  data=toenails method=quad(qpoints=5) order=data empirical order=data;
class id trt(ref='0') ;
model y = month month*trt /dist=bernoulli link=logit s oddsratio;
random intercept/subject=id type=un;
run;

*5. Rerun the mixed effects model with 2, 5, 10, 20, 30, and 50 quadrature points;
proc glimmix  data=toenails method=quad(qpoints=2) order=data empirical order=data;
class id trt(ref='0') ;
model y = month month*trt /dist=bernoulli link=logit s oddsratio;
random intercept/subject=id type=un;
run;

proc glimmix  data=toenails method=quad(qpoints=5) order=data empirical order=data;
class id trt(ref='0') ;
model y = month month*trt /dist=bernoulli link=logit s oddsratio;
random intercept/subject=id type=un;
run;

proc glimmix  data=toenails method=quad(qpoints=10) order=data empirical order=data;
class id trt(ref='0') ;
model y = month month*trt /dist=bernoulli link=logit s oddsratio;
random intercept/subject=id type=un;
run;

proc glimmix  data=toenails method=quad(qpoints=20) order=data empirical order=data;
class id trt(ref='0') ;
model y = month month*trt /dist=bernoulli link=logit s oddsratio;
random intercept/subject=id type=un;
run;

proc glimmix  data=toenails method=quad(qpoints=30) order=data empirical order=data;
class id trt(ref='0') ;
model y = month month*trt /dist=bernoulli link=logit s oddsratio;
random intercept/subject=id type=un;
run;

proc glimmix  data=toenails method=quad(qpoints=50) order=data empirical order=data;
class id trt(ref='0') ;
model y = month month*trt /dist=bernoulli link=logit s oddsratio;
random intercept/subject=id type=un;
run;