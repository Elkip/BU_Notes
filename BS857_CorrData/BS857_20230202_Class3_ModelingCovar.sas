libname S857 'Z:\';
data lead;
set s857.tlc;
y=y0;time=0;output;
y=y1;time=1;output;
y=y4;time=4;output;
y=y6;time=6;output;
drop y0 y1 y4 y6;
run;

ODS graphics on;
Proc Glimmix data=lead;
class time TRT;
model y =time TRT time*trt;
lsmeans time*trt
/ plots=(meanplot( join sliceby=trt)); 
run;
ODS graphics off;


/****************************************
*			Linear Trend Model			*
*****************************************/
data lead;
set lead;
t=time;
run;
title 'Linear Trend Model';
proc mixed data=lead method=ML;
class id trt t;
model y=time trt trt*time/s chisq;
repeated t/type=un subject=id r rcorr;
run;
estimate 'Baseline for trt' int 1 trt 1 0;
estimate 'Slope for trt' time 1 trt*time 1 0;
estimate 'Diff at week2'  trt 1 -1 trt*time 2 -2;
run;

/****************************************
*			Quadratic Trend Model		*
*****************************************/
data lead;
set lead;
timesq=(time-2.75)**2;
run;
title 'Quadratic Trend Model';

proc mixed data=lead method=ML;
class id trt t;
model y=  time timesq trt trt*time trt*timesq/s chisq;
repeated t/type=un subject=id r rcorr;
run;
estimate 'Quadratic factor for trt' timesq 1 timesq*trt 1 0;
estimate 'Linear factor for trt' time 1 time*trt 1 0;
run;

/****************************************
*			Spline Model				*
*****************************************/
data lead;
set lead;
time_1=max(time-1,0);
run;
title 'Spline Model';

proc mixed data=lead method=ML;
class id trt t;
model y=time time_1 trt trt*time trt*time_1/s chisq;
repeated t/type=un subject=id r rcorr;
estimate 'Slope of Treatment week 0 and week 1' time 1 time*trt 1 0;
estimate 'Slope of Placebo after week 1' time 1 time_1 1 time*trt 0 1 time_1*trt 0 1;
estimate 'Slope of Treatment after week 1' time 1 time_1 1 time*trt 1 0 time_1*trt 1 0;
estimate 'Differences in the slopes between groups after week 1' trt*time 1 -1 trt*time_1 1 -1;
run;

/*Likelihood Ratio Test*/
data LR;
LR=2556-2511;
pvalue=1-probchi(LR,2);
run;
proc print data=LR;
run;
/****************************************
*			COVARIANCE MODELS			*
*****************************************/


/****************************************
*			Symmetry  			*
*****************************************/
title 'Unstructured';

proc mixed data=lead method=REML;
class id trt t;
model y=trt time time_1 trt*time trt*time_1/s chisq;
repeated t/type=un subject=id r rcorr;
run;

title 'Compound Symmetry';

proc mixed data=lead method=REML;
class id trt t;
model y=trt time time_1 trt*time trt*time_1/s chisq;
repeated t/type=cs subject=id r rcorr;
run;
data LR;
LR=2479.6-2435.6;
pvalue=1-probchi(LR,8);
run;
proc print data=LR;
run;

/****************************************
*	Heterogeneous Compound Symmetry 	*
*****************************************/
title 'Heterogeneous Compound Symmetry';
proc mixed data=lead method=REML;
class id trt t;
model y=trt time time_1 trt*time trt*time_1/s chisq;
repeated t/type=csh subject=id r rcorr;
run;
data LR;
LR=2479.6-2452.4;
pvalue=1-probchi(LR,5);
run;
proc print data=LR;
run;

/****************************************
*	Autoregressive Order 1				*
*****************************************/
title 'Autoregressive Order 1';
proc mixed data=lead method=REML;
class id trt t;
model y=trt time time_1 trt*time trt*time_1/s chisq;
repeated t/type=ar(1) subject=id r rcorr;
run;

/****************************************
*	Heterogeneous AR(1)					*
*****************************************/
title 'Heterogeneous AR(1)';
proc mixed data=lead method=REML;
class id trt t;
model y=trt time time_1 trt*time trt*time_1/s chisq;
repeated t/type=arh(1) subject=id r rcorr;
run;

data LR;
LR=2494.7-2472.8;
pvalue=1-probchi(LR,3);
run;
proc print data=LR;
run;


data LR;
LR=2472.8-2435.6;
pvalue=1-probchi(LR,5);
run;
proc print data=LR;
run;
/****************************************
*	Exponential					*
*****************************************/
title 'Exponential';

proc mixed data=lead method=REML;
class id trt t;
model y=trt time time_1 trt*time trt*time_1/s chisq;
repeated t/type=sp(exp)(t) subject=id r rcorr;
run;
ods rtf close;
