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

title 'GLS with known  theta';
proc mixed data=lead ;
class id trt time(ref='0');
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r  rcorr ;
parms (25)(18)(75)(19)(59)(65)(22)(38)(37)(60)/noiter;
run;

title 'GLS with known  theta with empirical sandwich estimator';
proc mixed data=lead empirical;
class id trt time(ref='0');
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr;
parms (25)(18)(75)(19)(59)(65)(22)(38)(37)(60)/noiter;
run;

title 'GLS with unknown  theta';
proc mixed data=lead method=mivque0;
class id trt time(ref='0');
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr ;
run;

title 'GLS with unknown  theta with empirical sandwich estimator';
proc mixed data=lead method=mivque0 empirical;
class id trt time(ref='0');
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr ;
run;

title 'Maximum Likelihood';
proc mixed data=lead method=ML covtest;
class id trt time (ref='0');
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr ;
run;

title 'Maximum Likelihood with empirical sandwich estimator';
proc mixed data=lead method=ML empirical;
class id trt time (ref='0');
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr ;
run;


title 'Restricted Maximum Likelihood';
proc mixed data=lead method=REML;
class id trt time (ref='0');
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr ;
run;

title 'Restricted Maximum Likelihood with empirical sandwich estimator';
proc mixed data=lead method=REML empirical;
class id trt time (ref='0');
model y=trt time trt*time/s chisq covb;
repeated time/type=un subject=id r rcorr ;
run;

title 'Maximum Likelihood';
proc mixed data=lead method=ML;
class id trt (ref='P') time(ref="0");
model y=trt time trt*time/s  ;
repeated /type=un subject=id r rcorr ;
estimate 'TRT a time 0' int 1 trt 1 0 time 0 0 0 1 trt*time  0 0 0 1 0 0 0 0;
estimate 'TRT a time 6' int 1 trt 1 0 time 0 0 1 0 trt*time 0 0 1 0 0 0 0 0 ;
estimate 'TRT a time 4' int 1 trt 1 0 time 0 1 0 0 trt*time 0 1 0 0  0 0 0 0;
estimate 'TRT a time 1' int 1 trt 1 0 time 1 0 0 0 trt*time  1 0 0 0 0 0 0 0;

estimate 'TRT Change Time 1 - Time 0' time 1 0 0 -1 trt*time  1 0 0 -1 0 0 0 0;

run;


/***********************************************
*												*
*				RESTRICTIONS					*
*												*
************************************************/

proc mixed data=lead method=ML ;
class id trt(ref='P') time(ref='0');
model y=trt time trt*time/s chisq ;
repeated /type=un subject=id r rcorr ;
estimate 'TRT at week 1' int 1 trt 1 0  time 1 0 0 0 trt*time 1 0 0 0 0 0 0 0;
estimate 'P at week 1' 	 int 1 trt 0 1  time 1 0 0 0 trt*time 0 0 0 0 1 0 0 0;
estimate 'Diff Trt-Placebo at week 1' trt 1 -1 trt*time 1 0 0 0 -1 0 0 0;
estimate 'TRT at week 2' int 1 trt 1 0  time 0 1 0 0 trt*time 0 1 0 0 0 0 0 0;
estimate 'TRT at week 2' int 1 trt 0 1  time 0 1 0 0 trt*time 0 0 0 0 0 1 0 0;
estimate 'Diff Trt-Placebo at week 2' trt 1 -1 trt*time 0 1 0 0 0 -1 0 0;

contrast 'Diff Trt-Placebo at week 1' trt 1 -1 trt*time 1 0 0 0 -1 0 0 0;

contrast 'TRT*TIME '   trt*time 0 0 1 -1 0 0 -1 1,
					  trt*time 0 1 0 -1 0 -1 0 1,
					  trt*time 1 0 0 -1 -1 0 0 1;
contrast 'sum of betas' trt*time 1 1 -3 1 -1 -1 3 -1 ,
 						 trt*time 1 1 1 -3 -1 -1 -1 3 ;
estimate 'AUC' trt*time 2 2.5 1 -5.5 -2 -2.5 -1 5.5;

run;
*contrast 'trt x time effect' 
 trt*time 1 -0.33 -0.33 -0.33;
estimate 'TRT a time 0' int 1 trt 0 1 time 0 0 0 1 trt*time 0 0 0 0 0 0 0 1;
estimate 'TRT a time 6' int 1 trt 0 1 time 0 0 1 0 trt*time 0 0 0 0 0 0 1 0;
estimate 'TRT a time 4' int 1 trt 0 1 time 0 1 0 0 trt*time 0 0 0 0 0 1 0 0;
estimate 'TRT a time 1' int 1 trt 0 1 time 1 0 0 0 trt*time 0 0 0 0 1 0 0 0;

estimate 'Placebo a time 0' int 1 trt 1 0 time 0 0 0 1 trt*time 0 0 0 1 0 0 0 0;
estimate 'Placebo a time 6' int 1 trt 1 0 time 0 0 1 0 trt*time 0 0 1 0 0 0 0 0;
estimate 'Placebo a time 4' int 1 trt 1 0 time 0 1 0 0 trt*time 0 1 0 0 0 0 0 0;
estimate 'Placebo a time 1' int 1 trt 1 0 time 1 0 0 0 trt*time 1 0 0 0 0 0 0 0;

estimate 'Group diff at time 0'  trt -1 1 trt*time 0 0 0 -1 0 0 0 1;
estimate 'Group diff at time 6'  trt -1 1 trt*time 0 0 -1 0 0 0 1 0;
estimate 'Group diff at time 4'  trt -1 1 trt*time 0 -1 0 0 0 1 0 0;
estimate 'Group diff at time 1'  trt -1 1 trt*time -1 0  0 0 1 0 0 0;

estimate 'TRT diff time4-time6' time 0 1 -1 0 trt*time 0 0 0 0 0 1 -1 0 ;
estimate 'Placebo diff time4-time6' time 0 0 1 -1 trt*time 0 1 -1 0 0 0 0 0;
estimate 'Difference between group btw t=4 and t=6'  trt*time 0 1 -1 0 0 -1 1 0 ;

estimate 'AUC for trt' time 0.333 0.333 0.333 -.999 trt*time 0 0 0 0 0.333 0.333 0.333 -.999;
estimate 'AUC for placebo' time 0.333 0.333 0.333 -.999 trt*time 0.333 0.333 0.333 -.999 0 0 0 0;
estimate 'AUC with equal weight'  trt*time 0.333 0.333 0.333 -.999 -0.333 -0.333 -0.333 .999;
estimate 'AUC with unequal weight'  trt*time 1 2.5 2 -5.5 -1 -2.5 -2 5.5;

contrast 'Treatment' trt -1 1;
contrast 'Time'  time 1 0 0 -1,
				 time 0 1 0 -1, 
				 time 0 0 1 -1;
contrast 'TRT*TIME '   trt*time 0 0 1 -1 0 0 -1 1,
					  trt*time 0 1 0 -1 0 -1 0 1,
					  trt*time 1 0 0 -1 -1 0 0 1;


title 'Analysis using baseline as a covariate';

data lead_bl;
set lead;
if time=0 then output;
run;
proc means data=lead_bl;
var y;
run;
data lead_bl;
set lead_bl;
baseline=y-26.406;
keep id baseline;
run;
proc sort data=lead;
by id;
run;
proc sort data=lead_bl;
by id;
run;
data lead_bl_analysis;
merge lead lead_bl;
by id;
run;
data lead_bl_analysis;
set lead_bl_analysis;
if time=0 then delete;
run;

proc mixed data=lead_bl_analysis method=ML order=data;
class id trt(ref='P') time(ref='1');
model y=baseline trt time trt*time/s chisq ;
repeated /type=un subject=id r rcorr ;
run;
ods rtf close;
