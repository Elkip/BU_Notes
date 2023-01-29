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
estimate 'TRT a time 0' int 1 trt 1 0 time 0 0 0 0 1 trt*time  0 0 0 0 1 0 0 0 0 0;
estimate 'TRT a time 24' int 1 trt 1 0 time 0 0 0 1 0 trt*time 0 0 0 1 0 0 0 0 0 0;
estimate 'TRT a time 20' int 1 trt 1 0 time 0 0 1 0 0 trt*time 0 0 1 0 0  0 0 0 0 0;
estimate 'TRT a time 12' int 1 trt 1 0 time 0 1 0 0 0 trt*time 0 1 0 0 0 0 0 0 0 0;
estimate 'TRT a time 6' int 1 trt 1 0 time 1 0 0 0 0 trt*time 1 0 0 0 0 0 0 0 0 0;
estimate 'PLCB a time 0' int 1 trt 0 1 time 0 0 0 0 1 trt*time  0 0 0 0 0 0 0 0 0 1;
estimate 'PLCB a time 24' int 1 trt 0 1 time 0 0 0 1 0 trt*time 0 0 0 0 0 0 0 0 1 0;
estimate 'PLCB a time 20' int 1 trt 0 1 time 0 0 1 0 0 trt*time 0 0 0 0 0  0 0 1 0 0;
estimate 'PLCB a time 12' int 1 trt 0 1 time 0 1 0 0 0 trt*time 0 0 0 0 0 0 1 0 0 0;
estimate 'PLCB a time 6' int 1 trt 0 1 time 1 0 0 0 0 trt*time 0 0 0 0 0 1 0 0 0 0;
estimate 'Group Diff time 0' trt 1 -1 trt*time 0 0 0 0 1 0 0 0 0 -1;
estimate 'Group Diff time 24' trt 1 -1 trt*time 0 0 0 1 0 0 0 0 -1 0;
estimate 'Group Diff time 20' trt 1 -1 trt*time 0 0 1 0 0  0 0 -1 0 0;
estimate 'Group Diff time 12' trt 1 -1 trt*time 0 1 0 0 0 0 -1 0 0 0;
estimate 'Group Diff time 6' trt 1 -1 trt*time 1 0 0 0 0 -1 0 0 0 0;
estimate 'Interaction time 0' trt*time 0 0 0 0 0 0 0 0 0 0;
estimate 'Interaction time 6' trt*time 0 0 0 1 -1 0 0 0 -1 1;
estimate 'Interaction time 12' trt*time 0 0 1 0 -1 0 0 -1 0 1;
estimate 'Interaction time 20' trt*time 0 1 0 0 -1 0 -1 0 0 1;
estimate 'Interaction time 24' trt*time 1 0 0 0 -1 -1 0 0 0 1;
run;

*2 Display the estimated 5x5 Covariances and correlation matrices for 
the five repeated measurements of serum level using REML;
title 'REML Estimated Covariances and Correlation Matrices';
proc mixed data=chol method=REML;
class trt (ref="2") time (ref="0");
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr;
estimate 'Interaction time 0' trt*time 0 0 0 0 0 0 0 0 0 0;
estimate 'Interaction time 6' trt*time 0 0 0 1 -1 0 0 0 -1 1;
estimate 'Interaction time 12' trt*time 0 0 1 0 -1 0 0 -1 0 1;
estimate 'Interaction time 20' trt*time 0 1 0 0 -1 0 -1 0 0 1;
contrast 'Interaction time 24' trt*time 1 0 0 0 -1 -1 0 0 0 1;
run;

*4 Describe an weight matrix L for H0: that the patterns of change over
time do not differ in the two treatment groups using contrast statements;



*5 Estimate AUC for treatment and placebo;
title 'Estimate of AUC';


*6 Run a model using baseline cholestrol as covariate and interpret;
title 'Analysis using baseline as a covariate';



*7 Show that in the case of a correct variance-covariance model the sandwich
estimator of the variance of the MLE estimates is equal to the model based one;
title 'Sandwich based Estimator';

