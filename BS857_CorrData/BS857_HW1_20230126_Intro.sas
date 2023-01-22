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

*3a. Calculate mean, sd and variance of serum chol levels from each treatment group;
proc means data=chol mean std var;
class time trt;
var y;
run;

*3b. Construct a time plot of the mean chol vs time;
ODS graphics on;
Proc Glimmix data=chol;
class time trt;
model y =time trt time*trt;
lsmeans time*trt
/ plots=(meanplot( join sliceby=trt)); 
run;
ODS graphics off;
ods rtf close;

/*3c. Conduct a profile analysis using time as a categorical variable 
and treatement group as predictor by aplying the following estimation methods.
Calculate mean outcome at each time point for each group */
ODS graphics on;
proc mixed data=chol;
class id trt (ref="2") time (ref="0");
model y=trt time trt*time/s;
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
run;
ODS graphics off;
ods rtf close;
