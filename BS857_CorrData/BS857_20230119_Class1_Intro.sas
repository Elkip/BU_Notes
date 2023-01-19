libname S857 'C:\Users\yorghos\Dropbox\Courses\BS857\2021\Datasets';
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
ods rtf close;

proc corr data=s857.tlc cov;
var y0 y1 y4 y6;
run;

/*Repeated Measures MANOVA*/
proc mixed data=lead;
class id trt time;
model y=trt time trt*time/s chisq;
repeated time/type=un subject=id r rcorr;
run;


proc mixed data=lead method=ML;
class id trt (ref='P') time(ref="0");
model y=trt time trt*time/s  ;
repeated time/type=un subject=id r rcorr ;
estimate 'TRT a time 0' int 1 trt 1 0 time 0 0 0 1 trt*time  0 0 0 1 0 0 0 0;
estimate 'TRT a time 6' int 1 trt 1 0 time 0 0 1 0 trt*time 0 0 1 0 0 0 0 0 ;
estimate 'TRT a time 4' int 1 trt 1 0 time 0 1 0 0 trt*time 0 1 0 0  0 0 0 0;
estimate 'TRT a time 1' int 1 trt 1 0 time 1 0 0 0 trt*time  1 0 0 0 0 0 0 0;

estimate 'TRT Change Time 1 - Time 0' time 1 0 0 -1 trt*time  1 0 0 -1 0 0 0 0;

run;
