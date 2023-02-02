*BS857 HW2;
*Mitchell Henschel;
libname HW1 'Z:\';

proc corr data=HW1.chol cov;
var y0 y6 y12 y20 y24;
run;

*Load Data into CHOL;
data chol;
set HW1.chol;
y=y0; time=0; output;
y=y6; time=6; output;
y=y12; time=12; output;
y=y20; time=20; output;
y=y24; time=24; output;
drop y0 y6 y12 y20 y24;
run;

*1. Conduct analysis of repsonse profiles and Determine whether the 
patterns of change over time differ in the two treatment groups;
title 'Time*Treatment Interaction';
proc mixed data=chol method=ML covtest;
class id trt (ref="2") time (ref="0");
model y=trt time trt*time/s chisq;
repeated time/type=un subject=id r rcorr;
estimate 'Interaction time 6' trt*time 1 0 0 0 -1 -1 0 0 0 1;
estimate 'Interaction time 12' trt*time 0 1 0 0 -1 0 -1 0 0 1;
estimate 'Interaction time 20' trt*time 0 0 1 0 -1 0 0 -1 0 1;
estimate 'Interaction time 24' trt*time 0 0 0 1 -1 0 0 0 -1 1;
run;

*2 Display the estimated 5x5 Covariances and correlation matrices for 
ttitle 'REML Estimated Covariances and Correlation Matrices';
proc mixed data=chol method=REML;
class trt time (ref="0");
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr;
run;

proc mixed data=chol method=ML covtest;
class id trt (ref="2") time (ref="0");
model y=trt time trt*time/s chisq;
repeated time/type=un subject=id r rcorr;
contrast 'Interaction' trt*time 1 0 0 0 -1 -1 0 0 0 1,
  trt*time 0 1 0 0 -1 0 -1 0 0 1,
 trt*time 0 0 1 0 -1 0 0 -1 0 1,
 trt*time 0 0 0 1 -1 0 0 0 -1 1;
run;

*5 Estimate AUC for treatment and placebo;
title 'Estimate of AUC';
proc mixed data=chol method=ML;
class trt (ref="2") time (ref="0");
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr;
estimate 'AUC with unequal weight'  trt*time 6 7 6 2 -21 -6 -7 -6 -2 21;
run;

*6 Run a model using baseline cholestrol as covariate and interpret;
title 'Analysis using baseline as a covariate';
data chol_bl;
set chol;
if time=0 then output;
run;
proc means data=chol_bl;
var y;
run;
data chol_bl;
set chol_bl;
baseline=y-229.961;
keep id baseline;
run;
proc sort data=chol;
by id;
run;
proc sort data=chol_bl;
by id;
run;
data chol_bl_analysis;
merge chol chol_bl;
by id;
run;
data chol_bl_analysis;
set chol_bl_analysis;
if time=0 then delete;
run;

proc mixed data=chol_bl_analysis method=ML order=data;
class id trt time(ref='6');
model y=baseline trt time trt*time/s chisq ;
repeated /type=un subject=id r rcorr ;
run;


*7 Show that in the case of a correct variance-covariance model the sandwich
estimator of the variance of the MLE estimates is equal to the model based one;
title 'Sandwich based ML Estimator';
proc mixed data=chol method=ML empirical;
class trt (ref="2") time (ref="0");
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr;
run;

title 'Model based ML Estimator';
proc mixed data=chol method=ML covtest;
class trt (ref="2") time (ref="0");
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr;
run;
