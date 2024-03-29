libname S857 'Z:\';

data amenorrhea;
set s857.amenorrhea;
t=time;
time2=time**2;
run;

proc sort data=amenorrhea;
by descending trt;
run;
title1 'Marginal Models';
title2 'Clinical Trial of Contracepting Women';

proc genmod data=amenorrhea descending;
class id trt (ref='0') t/param=ref;
model y = time time2 trt*time trt*time2 /dist=binomial link=logit type3 wald;
     repeated subject=id / withinsubject=t logor=fullclust;
store p1;
run;
ods graphics on;
ods html style=journal;
proc plm source=p1;
  score data = amenorrhea out=pred /ilink;
run;
proc sort data = pred;
  by trt time;
run;
proc sgplot data = pred;
  series x = time y = predicted /group=trt;
run;
ods graphics off;

title1 'Generalized Linear Mixed Effects Models';

proc glimmix  data=amenorrhea  method=quad(qpoints=5) empirical order=data ;
class id trt ;
model y = time time2 trt*time trt*time2 /dist=binomial link=logit s oddsratio;
random intercept time/subject=id type=un;
run;

title1 'Generalized Linear Mixed Effects Models';
title2 'Clinical Trial of AntiEpileptic-Drug';
data epilepsy;
set s857.epilepsy;
time=0;y=y0;output;
time=1;y=y1;output;
time=2;y=y3;output;
time=3;y=y3;output;
time=4;y=y4;output;
drop y0 y1 y2 y3 y4;
run;
data epilepsy;
set epilepsy;
if time=0 then t=log(8);
else t=log(2);
run;
proc sort data=epilepsy;
by descending trt;
run;
proc glimmix  data=epilepsy  method=quad(qpoints=50) empirical order=data ;
class id trt ;
model y = time trt trt*time /dist=poisson link=log s offset=t;
random intercept time/subject=id type=un;
run;
ods rtf close;
